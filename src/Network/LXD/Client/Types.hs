{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Network.LXD.Client.Types (
  -- * Generic responses
  Response(..)
, ResponseType(..)
  -- ** Background operations
, BackgroundOperation(..)

  -- * API
, ApiConfig(..)
, ApiStatus(..)
, AuthStatus(..)
, ApiVersion(..)

  -- * Certificates
, CertificateHash(..)

  -- * Containers
  -- ** Querying information
, ContainerName(..)
, Container(..)
  -- ** Executing commands
, ExecParams(..)
, ExecRequest(..)
, ExecResponseImmediate
, ExecResponseWebsocket(..)
, ExecResponse
  -- ** Working with file descriptors
, FdSet(..)
, Fds(..)
, ExecFds
) where

import Network.LXD.Prelude

import Data.Aeson
import Data.Default
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import Data.Text (pack, unpack)
import qualified Data.Map.Strict as Map

import Web.Internal.HttpApiData (ToHttpApiData(..))

-- | Generic LXD API response object.
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

-- | Background operation response object, with metadata of type @m@.
data BackgroundOperation m = BackgroundOperation {
    backgroundOperationId :: String
  , backgroundOperationClass :: String
  , backgroundOperationCreatedAt :: String
  , backgroundOperationUpdatedAt :: String
  , backgroundOperationStatus :: String
  , backgroundOperationStatusCode :: Int
  , backgroundOperationMetadata :: m
  , backgroundOperationMayCancel :: Bool
  , backgroundOperationeErr :: String
  }

instance FromJSON m => FromJSON (BackgroundOperation m) where
    parseJSON = withObject "BackgroundOperation" $ \v -> BackgroundOperation
        <$> v .: "id"
        <*> v .: "class"
        <*> v .: "created_at"
        <*> v .: "updated_at"
        <*> v .: "status"
        <*> v .: "status_code"
        <*> v .: "metadata"
        <*> v .: "may_cancel"
        <*> v .: "err"

-- | LXD API configuration object.
--
-- Returend when querying @GET \/1.0@. Some objects may not be present if
-- an untrusted requeset was made.
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

-- | LXD trusted certificate hash.
newtype CertificateHash = CertificateHash String deriving (Eq, Show)

instance FromJSON CertificateHash where
    parseJSON = withText "CertificateHash" $ \text ->
        let prefix = "/1.0/certificates/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse hash: no prefix " ++ prefix
            Just hash -> return $ CertificateHash hash

-- | LXD container name.
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

-- | LXD container information.
--
-- Returned when querying @GET \/1.0\/containers\/\<name\>@.
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

-- | Configuration parameter to 'ExecRequest'.
data ExecParams = ExecImmediate               -- ^ Don't wait for a websocket connection before executing.
                | ExecWebsocketInteractive    -- ^ Wait for websocket, allocate PTY.
                | ExecWebsocketNonInteractive -- ^ Wait for websocket, don't allocate PTY.
                deriving (Show)

-- | LXD container exec request, configured using 'ExecParams' as type parameter.
--
-- Request body when querying @POST \/1.0\/containers\/\<name\>\/exec@.
data ExecRequest (params :: ExecParams) = ExecRequest {
    execRequestCommand :: [String]
  , execRequestEnvironment :: Map String String
  , execRequestRecordOutput :: Bool
  , execRequestWidth :: Int
  , execRequestHeight :: Int
} deriving (Show)

instance Default (ExecRequest a) where
    def = ExecRequest { execRequestCommand = []
                      , execRequestEnvironment = Map.empty
                      , execRequestRecordOutput = False
                      , execRequestWidth = 80
                      , execRequestHeight = 25 }

instance ToJSON (ExecRequest 'ExecImmediate) where
    toJSON ExecRequest{..} = object [
        "command" .= execRequestCommand
      , "environment" .= execRequestEnvironment
      , "wait-for-websocket" .= False
      , "record-output" .= execRequestRecordOutput
      , "interactive" .= False
      , "width" .= execRequestWidth
      , "height" .= execRequestHeight
      ]

instance ToJSON (ExecRequest 'ExecWebsocketInteractive) where
    toJSON ExecRequest{..} = object [
        "command" .= execRequestCommand
      , "environment" .= execRequestEnvironment
      , "wait-for-websocket" .= True
      , "record-output" .= execRequestRecordOutput
      , "interactive" .= True
      , "width" .= execRequestWidth
      , "height" .= execRequestHeight
      ]
instance ToJSON (ExecRequest 'ExecWebsocketNonInteractive) where
    toJSON ExecRequest{..} = object [
        "command" .= execRequestCommand
      , "environment" .= execRequestEnvironment
      , "wait-for-websocket" .= True
      , "record-output" .= execRequestRecordOutput
      , "interactive" .= False
      , "width" .= execRequestWidth
      , "height" .= execRequestHeight
      ]

-- | A set of selected file descriptors.
data FdSet = FdAll | FdPty deriving (Show)

-- | A set of file descriptors.
data Fds set where
    FdsAll :: { fdsAllStdin :: String
              , fdsAllStdout :: String
              , fdsAllStderr :: String
              , fdsAllControl :: String } -> Fds 'FdAll
    FdsPty :: { fdsPty :: String
              , fdsPtyControl :: String } -> Fds 'FdPty

deriving instance Show (Fds set)

instance FromJSON (Fds 'FdAll) where
    parseJSON = withObject "Fds 'FdAll" $ \v -> FdsAll
        <$> v .: "0"
        <*> v .: "1"
        <*> v .: "2"
        <*> v .: "control"

instance FromJSON (Fds 'FdPty) where
    parseJSON = withObject "Fds 'FdPty" $ \v -> FdsPty
        <$> v .: "0"
        <*> v .: "control"

-- | Type family converting an 'ExecParams' to an 'FdSet'.
type family ExecFds (params :: ExecParams) :: FdSet where
    ExecFds 'ExecWebsocketInteractive    = 'FdPty
    ExecFds 'ExecWebsocketNonInteractive = 'FdAll

-- | Metadata of an immediate exec response.
--
-- Returned when querying @POST \/1.0\/containers\/\<name\>\/exec@ with
-- 'ExecImmediate' as configuration.
type ExecResponseImmediate = Value

-- | Metadata of a websocket exec repsonse.
--
-- Returned when querying @POST \/1.0\/containers\/\<name\>\/exec@ with
-- 'ExecWebsocketInteractive' or 'ExecWebsocketNonInteractive' as
-- configuration.
--
-- Paramtrized by a file descriptor set 'FdSet', see also the type family
-- 'ExecFds'.
newtype ExecResponseWebsocket fdset = ExecResponseWebsocket {
    execResponseWebsocketFds :: Fds fdset
} deriving (Show)

instance FromJSON (ExecResponseWebsocket 'FdPty) where
    parseJSON = withObject "ExecResponse 'FdPty" $ \v ->
        ExecResponseWebsocket <$> v .: "fds"

instance FromJSON (ExecResponseWebsocket 'FdAll) where
    parseJSON = withObject "ExecResponse 'FdAll" $ \v ->
        ExecResponseWebsocket <$> v .: "fds"

-- | Type family converting an 'ExecParams' to the corresponding response type.
type family ExecResponse (params :: ExecParams) :: * where
    ExecResponse 'ExecImmediate               = ExecResponseImmediate
    ExecResponse 'ExecWebsocketInteractive    = ExecResponseWebsocket 'FdPty
    ExecResponse 'ExecWebsocketNonInteractive = ExecResponseWebsocket 'FdAll

-- | The type of a generic response object.
data ResponseType = Sync | Async deriving (Eq, Show)

instance FromJSON ResponseType where
    parseJSON = withText "ResponseType" $ \case
        "sync"  -> pure Sync
        "async" -> pure Async
        v       -> fail $ "Unknown value: " ++ show v

-- | LXD API version string, e.g. 1.0.
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
