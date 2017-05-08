{-# LANGUAGE RecordWildCards #-}
module Network.LXD.Client (
  module Network.LXD.Client.API
, module Network.LXD.Client.Types

  -- * LXD Host Management
, RemoteHost(..)
, ClientAuth(..)
, ServerAuth(..)

  -- * HTTPS Clients
, remoteHostClient
, remoteHostManager
, clientManager
) where

import Control.Exception (SomeException, tryJust, toException)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..))

import Data.Default (Default, def)
import Data.Either.Combinators (mapLeft)
import Data.X509 (CertificateChain)
import Data.X509.Validation (ValidationCache,
                             FailedReason(NameMismatch),
                             ServiceID,
                             validateDefault)
import Data.X509.CertificateStore (CertificateStore, readCertificateStore)

import Network.LXD.Client.API
import Network.LXD.Client.Types

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (Manager, ManagerSettings, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (ClientHooks(onCertificateRequest, onServerCertificate),
                    ClientParams(clientShared, clientHooks, clientSupported),
                    Credential,
                    Shared(sharedCAStore),
                    Supported(supportedCiphers),
                    credentialLoadX509,
                    defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_all)

import Servant.Client (BaseUrl(..), ClientEnv(..), Scheme(Https))

import System.Directory (getHomeDirectory)

type RemoteName = String
type Host = String
type Certificate = FilePath
type Key = FilePath

-- | A structure containing everything to connect to a remote LXD host.
data RemoteHost = RemoteHost {
    remoteHostHost :: Host                -- ^ The remote host to use when querying the HTTP endpoint. (default=@127.0.0.1@)
  , remoteHostPort :: Int                 -- ^ The remote port to use when querying the HTTP endpoint. (default=@8443@)
  , remoteHostBasePath :: String          -- ^ The base path to use when querying the HTTP endpoint. (default=@/@)
  , remoteHostClientKey :: ClientAuth     -- ^ The client authentication to use when connecting.
  , remoteHostCertificate :: ServerAuth   -- ^ The server certificate to trust.
}

instance Default RemoteHost where
    def = RemoteHost { remoteHostHost = ""
                     , remoteHostPort = 8443
                     , remoteHostBasePath = ""
                     , remoteHostClientKey = DefaultClientAuth
                     , remoteHostCertificate = DefaultCAStore }

-- | Specifies the client authentication method.
data ClientAuth = NoClientAuth              -- ^ Do not authenticate the client.
                | DefaultClientAuth         -- ^ Look in @~/.config/lxc@ and fetch the client certificate.
                | ClientAuthKey PrivateKey   -- ^ Use a custom private key.

data PrivateKey = PrivateKey Certificate Key

privateKey :: ClientAuth -> Maybe PrivateKey
privateKey NoClientAuth        = Nothing
privateKey DefaultClientAuth   = Just (PrivateKey "~/.config/lxc/client.crt" "~/.config/lxc/client.key")
privateKey (ClientAuthKey key) = Just key

-- | Specifies the server authentication method.
data ServerAuth = DefaultCAStore                -- ^ Use the default CA store when checking the certificate.
                | DefaultServerAuth RemoteName  -- ^ Look in @~/.config/lxc/servercerts@ and fetch the server certificate for
                                                --   the specified remote.
                | ServerAuth Certificate        -- ^ Use a custom server certificate.

serverCertificate :: ServerAuth -> Maybe Certificate
serverCertificate DefaultCAStore             = Nothing
serverCertificate (DefaultServerAuth remote) = Just ("~/.config/lxc/servercerts/" ++ remote ++ ".crt")
serverCertificate (ServerAuth cert)          = Just cert

remoteHostManager :: MonadIO m => RemoteHost -> EitherT String m Manager
remoteHostManager RemoteHost{..} = clientManager remoteHostHost
                                                 (privateKey remoteHostClientKey)
                                                 (serverCertificate remoteHostCertificate)

remoteHostClient :: MonadIO m => RemoteHost -> EitherT String m ClientEnv
remoteHostClient remote@RemoteHost{..} =
    ClientEnv <$> remoteHostManager remote <*> pure baseUrl
  where
    baseUrl = BaseUrl Https remoteHostHost remoteHostPort remoteHostBasePath


clientManager :: MonadIO m => Host -> Maybe PrivateKey -> Maybe Certificate -> EitherT String m Manager
clientManager = ((.).(.).(.)) (>>= newManager') clientManagerSettings
  where newManager' = EitherT . liftIO . (Right <$>) . newManager

clientManagerSettings :: MonadIO m => Host -> Maybe PrivateKey -> Maybe Certificate -> EitherT String m ManagerSettings
clientManagerSettings host clientKey serverCert = do
    tlsSettings <- clientTlsSettings host <$> credentials clientKey <*> caStore serverCert
    return $ mkManagerSettings tlsSettings Nothing
  where
    credentials Nothing                      = return Nothing
    credentials (Just (PrivateKey cert key)) = do
        cert' <- lift $ expandHomeDirectory cert
        key'  <- lift $ expandHomeDirectory key
        Just <$> (EitherT . liftIO . catchAdditional $ credentialLoadX509 cert' key')

    caStore Nothing     = return Nothing
    caStore (Just cert) = Just <$> (EitherT . liftIO $ readCertificateStore' cert)
    readCertificateStore' cert = maybe (Left $ "error: could not read certificate at " ++ cert) Right <$> readCertificateStore cert

clientTlsSettings :: Host -> Maybe Credential -> Maybe CertificateStore -> TLSSettings
clientTlsSettings host creds caStore =
    TLSSettings clientParams
  where
    hooks = def { onCertificateRequest = const $ return creds
                , onServerCertificate  = validateServerCert }
    clientParams = (defaultParamsClient host "")
                   { clientShared = shared
                   , clientHooks  = hooks
                   , clientSupported = def { supportedCiphers = ciphersuite_all }
                   }
    shared | Just store <- caStore = def { sharedCAStore = store }
           | otherwise             = def

validateServerCert :: CertificateStore
                   -> ValidationCache
                   -> ServiceID
                   -> CertificateChain
                   -> IO [FailedReason]
validateServerCert a b c d = filter (not . ignore) <$> validateDefault a b c d
  where ignore x | NameMismatch _ <- x = True
                 | otherwise = False

catchAdditional :: IO (Either String a) -> IO (Either String a)
catchAdditional action = join . mapLeft show <$> tryJust (Just . toException') action
  where
    toException' :: SomeException -> SomeException
    toException' = toException

expandHomeDirectory :: MonadIO m => FilePath -> m FilePath
expandHomeDirectory ('~':'/':xs) = (++ "/" ++ xs) <$> liftIO getHomeDirectory
expandHomeDirectory x            = return x
