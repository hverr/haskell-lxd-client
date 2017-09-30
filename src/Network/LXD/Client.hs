{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module exposes functionality to create LXD clients. These can
-- be used to communciate to an LXD daemon, either using the high-level
-- "Network.LXD.Client.Commands" module, or the low-level
-- "Network.LXD.Client.API" module.
--
-- __You are probably looking for "Network.LXD.Client.Commands"__, which
-- exposes a high-level interface to communicate with the LXD daemon.
--
-- If you are simply connecting to the LXD daemon on your local host,
-- you shouldn't import this module. The "Network.LXD.Client.Commands"
-- module probably re-exports enough functionality for your needs.
--
--
module Network.LXD.Client (
  module Network.LXD.Client.Types

  -- * LXD Host Management
  -- ** HTTPS Clients
  -- *** Types
, RemoteHost(..)
, ClientAuth(..)
, ServerAuth(..)
, Host, RemoteName, Certificate, Key, PrivateKey(..)
  -- *** Functions
, remoteHostClient
, remoteHostManager
, clientManager

  -- * Unix Clients
, LocalHost(..)
, localHostClient

  -- * WebSockets Clients
, runWebSocketsRemote
, runWebSocketsLocal
) where

import Network.LXD.Client.Internal.Prelude

import Control.Exception (SomeException, tryJust, toException, throwIO, bracket)

import Data.Default (Default, def)
import Data.Either.Combinators (mapLeft)
import Data.X509 (CertificateChain)
import Data.X509.Validation (ValidationCache,
                             FailedReason(NameMismatch),
                             ServiceID,
                             validateDefault)
import Data.X509.CertificateStore (CertificateStore, readCertificateStore)
import qualified Data.ByteString.Lazy as B

import Network.LXD.Client.Types

import Network.Connection (ConnectionParams(..), TLSSettings(..), initConnectionContext)
import Network.HTTP.Client (Manager, ManagerSettings(..), newManager, defaultManagerSettings, socketConnection)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (ClientHooks(onCertificateRequest, onServerCertificate),
                    ClientParams(clientShared, clientHooks, clientSupported),
                    Credential,
                    Shared(sharedCAStore),
                    Supported(supportedCiphers),
                    credentialLoadX509,
                    defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Network.Connection as Con
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS

import Servant.Client (BaseUrl(..), ClientEnv(..), Scheme(Http, Https))

import System.Directory (getHomeDirectory)
import System.IO.Error (catchIOError, isEOFError)

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

-- | A structure containing everything to connect to a lcoal LXD host.
newtype LocalHost = LocalHost {
    localHostUnix :: FilePath             -- ^ The path to the local unix socket.
  }

instance Default LocalHost where
    def = LocalHost { localHostUnix = "/var/lib/lxd/unix.socket" }

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

remoteHostManager :: (MonadError String m, MonadIO m) => RemoteHost -> m Manager
remoteHostManager RemoteHost{..} = clientManager remoteHostHost
                                                 (privateKey remoteHostClientKey)
                                                 (serverCertificate remoteHostCertificate)

remoteHostClient :: (MonadError String m, MonadIO m) => RemoteHost -> m ClientEnv
remoteHostClient remote@RemoteHost{..} =
    ClientEnv <$> remoteHostManager remote <*> pure baseUrl
  where
    baseUrl = BaseUrl Https remoteHostHost remoteHostPort remoteHostBasePath


clientManager :: (MonadError String m, MonadIO m) => Host -> Maybe PrivateKey -> Maybe Certificate -> m Manager
clientManager = ((.).(.).(.)) (>>= newManager') clientManagerSettings
  where newManager' = liftIO . newManager

clientManagerSettings :: (MonadError String m, MonadIO m) => Host -> Maybe PrivateKey -> Maybe Certificate -> m ManagerSettings
clientManagerSettings host clientKey serverCert = do
    tlsSettings <- clientTlsSettings host clientKey serverCert
    return $ mkManagerSettings tlsSettings Nothing

clientTlsSettings :: (MonadError String m, MonadIO m) => Host -> Maybe PrivateKey -> Maybe Certificate -> m TLSSettings
clientTlsSettings host clientKey serverCert =
    clientTlsSettings' host <$> credentials clientKey <*> caStore serverCert
  where
    credentials Nothing                      = return Nothing
    credentials (Just (PrivateKey cert key)) = do
        cert' <- expandHomeDirectory cert
        key'  <- expandHomeDirectory key
        creds <- liftIO . catchAdditional $ credentialLoadX509 cert' key'
        Just <$> eitherToError creds

    caStore Nothing     = return Nothing
    caStore (Just cert) = Just <$> (eitherToError =<< liftIO (readCertificateStore' cert))
    readCertificateStore' cert = maybe (Left $ "error: could not read certificate at " ++ cert) Right <$> readCertificateStore cert

clientTlsSettings' :: Host -> Maybe Credential -> Maybe CertificateStore -> TLSSettings
clientTlsSettings' host creds caStore =
    TLSSettings clientParams
  where
    hooks = def { onCertificateRequest = const $ return creds
                , onServerCertificate  = validateServerCert }
    clientParams = (defaultParamsClient host "")
                   { clientShared = shared
                   , clientHooks  = hooks
                   , clientSupported = def { supportedCiphers = ciphersuite_default }
                   }
    shared | Just store <- caStore = def { sharedCAStore = store }
           | otherwise             = def

clientConnectionParams :: (MonadError String m, MonadIO m) => RemoteHost -> m ConnectionParams
clientConnectionParams RemoteHost{..} = do
    tlsSettings <- clientTlsSettings remoteHostHost
                                     (privateKey remoteHostClientKey)
                                     (serverCertificate remoteHostCertificate)
    return ConnectionParams { connectionHostname = remoteHostHost
                            , connectionPort = fromIntegral remoteHostPort
                            , connectionUseSecure = Just tlsSettings
                            , connectionUseSocks = Nothing }

localHostClient :: MonadIO m => LocalHost -> m ClientEnv
localHostClient host = do
    m <- liftIO $ newManager defaultManagerSettings { managerRawConnection = createUnixConnection }
    return $ ClientEnv m baseUrl
  where
    createUnixConnection = return $ \_ _ _ -> do
        s <- unixSocket host
        socketConnection s 4096
    baseUrl = BaseUrl Http "localhost" 80 ""

runWebSocketsRemote :: (MonadError String m, MonadIO m) => RemoteHost -> String -> WS.ClientApp a -> m a
runWebSocketsRemote host path app = do
    ctx    <- liftIO initConnectionContext
    params <- clientConnectionParams host
    liftIO $ bracket (Con.connectTo ctx params)
                     Con.connectionClose
                     action
  where
    action con = do
        stream <- WS.makeStream (reader con) (writer con)
        WS.runClientWithStream stream
                               (remoteHostHost host)
                               path
                               WS.defaultConnectionOptions
                               []
                               app

    reader con = catchIOError (Just <$> Con.connectionGetChunk con) $ \e ->
                              if isEOFError e
                                  then return Nothing
                                  else throwIO e
    writer con bs = case bs of
        Nothing -> return ()
        Just bs' -> Con.connectionPut con (B.toStrict bs')

runWebSocketsLocal :: (MonadError String m, MonadIO m) => LocalHost -> String -> WS.ClientApp a -> m a
runWebSocketsLocal host path app = liftIO $ do
    s <- unixSocket host
    WS.runClientWithSocket s "localhost" path WS.defaultConnectionOptions [] app

validateServerCert :: CertificateStore
                   -> ValidationCache
                   -> ServiceID
                   -> CertificateChain
                   -> IO [FailedReason]
validateServerCert a b c d = filter (not . ignore) <$> validateDefault a b c d
  where ignore x | NameMismatch _ <- x = True
                 | otherwise = False

unixSocket :: LocalHost -> IO Socket.Socket
unixSocket LocalHost{..} = do
    s <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
    Socket.connect s (Socket.SockAddrUnix localHostUnix)
    return s

catchAdditional :: IO (Either String a) -> IO (Either String a)
catchAdditional action = join . mapLeft show <$> tryJust (Just . toException') action
  where
    toException' :: SomeException -> SomeException
    toException' = toException

expandHomeDirectory :: MonadIO m => FilePath -> m FilePath
expandHomeDirectory ('~':'/':xs) = (++ "/" ++ xs) <$> liftIO getHomeDirectory
expandHomeDirectory x            = return x

eitherToError :: MonadError err m => Either err a -> m a
eitherToError (Left  x) = throwError x
eitherToError (Right x) = return x
