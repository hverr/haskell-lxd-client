{-# LANGUAGE LambdaCase #-}
module Network.LXD.Client.Types where

import Data.Aeson
import Data.Text (unpack)

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

data ResponseType = Sync | Async deriving (Show)

instance FromJSON ResponseType where
    parseJSON = withText "ResponseType" $ \case
        "sync"  -> pure Sync
        "async" -> pure Async
        v       -> fail $ "Unknown value: " ++ show v

newtype ApiVersion = ApiVersion String deriving (Show)

instance FromJSON ApiVersion where
    parseJSON = withText "ApiVersion" $ pure . ApiVersion . unpack

data ApiStatus = Development | Stable | Deprecated deriving (Show)

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
