{-# LANGUAGE LambdaCase #-}
module Network.LXD.Client.Types where

import Network.LXD.Prelude

import Data.Aeson
import Data.List (stripPrefix)
import Data.Text (pack, unpack)

import Web.Internal.HttpApiData (ToHttpApiData(..))

data Response a = Response {
    responseType :: ResponseType
  , status :: String
  , statusCode :: Int
  , operation :: String
  , errorCode :: Int
  , error :: String
  , metadata :: a
} deriving (Show)

instance FromJSON a => FromJSON (Response a) where
    parseJSON = withObject "Response" $ \v -> Response
        <$> v .: "type"
        <*> v .: "status"
        <*> v .: "status_code"
        <*> v .: "operation"
        <*> v .: "error_code"
        <*> v .: "error"
        <*> v .: "metadata"

data ApiConfig = ApiConfig {
    apiExtensions :: [String]
  , apiStatus :: ApiStatus
  , apiVersion :: String
  , authStatus :: AuthStatus
  , serverConfig :: Maybe Value
  , serverEnv :: Maybe Value
  , serverPublic :: Bool
} deriving (Show)

instance FromJSON ApiConfig where
    parseJSON = withObject "ApiConfig" $ \v -> ApiConfig
        <$> v .: "api_extensions"
        <*> v .: "api_status"
        <*> v .: "api_version"
        <*> v .: "auth"
        <*> v .:? "config"
        <*> v .:? "environment"
        <*> v .: "public"

newtype CertificateHash = CertificateHash String deriving (Eq, Show)

instance FromJSON CertificateHash where
    parseJSON = withText "CertificateHash" $ \text ->
        let prefix = "/1.0/certificates/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse hash: no prefix " ++ prefix
            Just hash -> return $ CertificateHash hash

newtype ContainerName = ContainerName String deriving (Eq, Show)

instance FromJSON ContainerName where
    parseJSON = withText "ContainerName" $ \text ->
        let prefix = "/1.0/containers/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse container name: no prefix " ++ prefix
            Just name -> return $ ContainerName name

instance IsString ContainerName where
    fromString = ContainerName

instance ToHttpApiData ContainerName where
    toUrlPiece (ContainerName name) = pack name

data Container = Container {
    containerArchitecture :: String
  , containerName :: String
  , containerConfig :: Value
  , containerCreatedAt :: String
  , containerDevices :: Value
  , containerEphemeral :: Bool
  , containerProfiles :: [String]
  , containerStateful :: Bool
  , containerExpandedConfig :: Value
  , containerExpandedDevices :: Value
  , containerStatus :: String
  , containerSatusCode :: Int
  , containerLastUsedAt :: String
} deriving (Show)

instance FromJSON Container where
    parseJSON = withObject "Container" $ \v -> Container
        <$> v .: "architecture"
        <*> v .: "name"
        <*> v .: "config"
        <*> v .: "created_at"
        <*> v .: "devices"
        <*> v .: "ephemeral"
        <*> v .: "profiles"
        <*> v .: "stateful"
        <*> v .: "expanded_config"
        <*> v .: "expanded_devices"
        <*> v .: "status"
        <*> v .: "status_code"
        <*> v .: "last_used_at"

data ResponseType = Sync | Async deriving (Eq, Show)

instance FromJSON ResponseType where
    parseJSON = withText "ResponseType" $ \case
        "sync"  -> pure Sync
        "async" -> pure Async
        v       -> fail $ "Unknown value: " ++ show v

newtype ApiVersion = ApiVersion String deriving (Eq, Show)

instance FromJSON ApiVersion where
    parseJSON = withText "ApiVersion" $ pure . ApiVersion . unpack

data ApiStatus = Development | Stable | Deprecated deriving (Eq, Show)

instance FromJSON ApiStatus where
    parseJSON = withText "ApiStatus" $ \case
        "development" -> pure Development
        "stable"      -> pure Stable
        "deprecated"  -> pure Deprecated
        v             -> fail $ "Unknown value: " ++ show v

data AuthStatus = Guest | Untrusted | Trusted deriving (Eq, Show)

instance FromJSON AuthStatus where
    parseJSON = withText "AuthStatus" $ \case
        "guest"     -> pure Guest
        "untrusted" -> pure Untrusted
        "trusted"   -> pure Trusted
        v           -> fail $ "Unknown value: " ++ show v
