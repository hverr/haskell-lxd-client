{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.LXD.Client.Types (
  -- * Generic responses
  GenericResponse(..)
, Response
, ResponseOp
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
, ExecResponse(..)
, ExecResponseMetadataImmediate
, ExecResponseMetadataWebsocket(..)
, ExecResponseMetadata
  -- ** Working with file descriptors
, Secret(..)
, FdSet(..)
, Fds(..)
, ExecFds

  -- * Images
, ImageId(..)
, Image(..)
, ImageAlias(..)
, ImageProperties(..)

  -- * Operations
, OperationId(..)
, OperationStatus
, AllOperations(..)
, Operation(..)
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
data GenericResponse op a = Response {
    responseType :: ResponseType
  , status :: String
  , statusCode :: Int
  , responseOperation :: op
  , errorCode :: Int
  , error :: String
  , metadata :: a
} deriving (Show)

-- | LXD API repsonse object, without resulting operation.
type Response a = GenericResponse String a

-- | LXD API response object, with resulting operation
type ResponseOp a = GenericResponse OperationId a

instance (FromJSON op, FromJSON a) => FromJSON (GenericResponse op a) where
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

-- | Configuration parameter to 'ExecRequest' and 'ExecResponse'.
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

-- | A secret used to connect to a websocket.
newtype Secret = Secret String deriving (Eq, Show)

instance FromJSON Secret where
    parseJSON = withText "Secret" $ return . Secret . unpack

-- | A set of selected file descriptors.
data FdSet = FdAll | FdPty deriving (Show)

-- | A set of file descriptors.
data Fds set where
    FdsAll :: { fdsAllStdin :: Secret
              , fdsAllStdout :: Secret
              , fdsAllStderr :: Secret
              , fdsAllControl :: Secret } -> Fds 'FdAll
    FdsPty :: { fdsPty :: Secret
              , fdsPtyControl :: Secret } -> Fds 'FdPty

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

-- | Response of an exec request, configured using 'ExecParams' as type
-- parameter.
--
-- Returned when querying @POST \/1.0\/containers\/\<name\>\/exec@.
data ExecResponse (params :: ExecParams) = ExecResponse {
    execResponseId :: String
  , execResponseClass :: String
  , execResponseCreatedAt :: String
  , execResponseUpdatedAt :: String
  , execResponseStatus :: String
  , execResponseStatusCode :: Int
  , execResponseMetadata :: ExecResponseMetadata params
  , execResponseMayCancel :: Bool
  , execResponseErr :: String
}

deriving instance Show (ExecResponseMetadata params) => Show (ExecResponse params)

instance FromJSON (ExecResponseMetadata params) => FromJSON (ExecResponse (params :: ExecParams)) where
    parseJSON = withObject "ExecResponse" $ \v -> ExecResponse
        <$> v .: "id"
        <*> v .: "class"
        <*> v .: "created_at"
        <*> v .: "updated_at"
        <*> v .: "status"
        <*> v .: "status_code"
        <*> v .: "metadata"
        <*> v .: "may_cancel"
        <*> v .: "err"

-- | Metadata of an immediate exec response.
--
-- Returned when querying @POST \/1.0\/containers\/\<name\>\/exec@ with
-- 'ExecImmediate' as configuration.
type ExecResponseMetadataImmediate = Value

-- | Metadata of a websocket exec repsonse.
--
-- Returned when querying @POST \/1.0\/containers\/\<name\>\/exec@ with
-- 'ExecWebsocketInteractive' or 'ExecWebsocketNonInteractive' as
-- configuration.
--
-- Paramtrized by a file descriptor set 'FdSet', see also the type family
-- 'ExecFds'.
newtype ExecResponseMetadataWebsocket fdset = ExecResponseMetadataWebsocket {
    execResponseMetadataWebsocketFds :: Fds fdset
} deriving (Show)

instance FromJSON (ExecResponseMetadataWebsocket 'FdPty) where
    parseJSON = withObject "ExecResponseMetadata 'FdPty" $ \v ->
        ExecResponseMetadataWebsocket <$> v .: "fds"

instance FromJSON (ExecResponseMetadataWebsocket 'FdAll) where
    parseJSON = withObject "ExecResponseMetadata 'FdAll" $ \v ->
        ExecResponseMetadataWebsocket <$> v .: "fds"

-- | Type family converting an 'ExecParams' to the corresponding response type.
type family ExecResponseMetadata (params :: ExecParams) :: * where
    ExecResponseMetadata 'ExecImmediate               = ExecResponseMetadataImmediate
    ExecResponseMetadata 'ExecWebsocketInteractive    = ExecResponseMetadataWebsocket 'FdPty
    ExecResponseMetadata 'ExecWebsocketNonInteractive = ExecResponseMetadataWebsocket 'FdAll

-- | LXD image identifier.
newtype ImageId = ImageId String deriving (Eq, Show)

instance FromJSON ImageId where
    parseJSON = withText "ImageId" $ \text ->
        let prefix = "/1.0/images/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse image id: no prefix " ++ prefix
            Just img -> return $ ImageId img

instance ToHttpApiData ImageId where
    toUrlPiece (ImageId img) = pack img

-- | Alias of an image.
data ImageAlias = ImageAlias {
    imageAliasName :: String
  , imageAliasDescription :: String
  } deriving (Show)

instance FromJSON ImageAlias where
    parseJSON = withObject "ImageAlias" $ \v -> ImageAlias
        <$> v .: "name"
        <*> v .: "description"

-- | Properties of an image.
data ImageProperties = ImageProperties {
    imagePropertiesArchitecture :: Maybe String
  , imagePropertiesDescription :: String
  , imagePropertiesOs :: Maybe String
  , imagePropertiesRelease :: Maybe String
  } deriving (Show)

instance FromJSON ImageProperties where
    parseJSON = withObject "ImageProperties" $ \v -> ImageProperties
        <$> v .:? "architecture"
        <*> v .:  "description"
        <*> v .:? "os"
        <*> v .:? "release"

-- | LXD image information.
--
-- Returned when querying @GET \/1.0\/images\/\<fingerprint\>@.
data Image = Image {
    imageAliases :: [ImageAlias]
  , imageArchitecture :: String
  , imageAutoUpdate :: Bool
  , imageCached :: Bool
  , imageFingerprint :: String
  , imageFilename :: String
  , imageProperties :: ImageProperties
  , imagePublic :: Bool
  , imageSize :: Integer
  , imageCreatedAt :: String
  , imageExpiresAt :: String
  , imageLastUsedAt :: String
  , imageUplaodedAt :: String
  } deriving (Show)

instance FromJSON Image where
    parseJSON = withObject "Image" $ \v -> Image
        <$> v .: "aliases"
        <*> v .: "architecture"
        <*> v .: "auto_update"
        <*> v .: "cached"
        <*> v .: "fingerprint"
        <*> v .: "filename"
        <*> v .: "properties"
        <*> v .: "public"
        <*> v .: "size"
        <*> v .: "created_at"
        <*> v .: "expires_at"
        <*> v .: "last_used_at"
        <*> v .: "uploaded_at"

-- | LXD operation identifier.
newtype OperationId = OperationId String deriving (Eq, Show)

instance FromJSON OperationId where
    parseJSON = withText "OperationId" $ \text ->
        let prefix = "/1.0/operations/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse operation id: no prefix " ++ prefix
            Just operation -> return $ OperationId operation

instance IsString OperationId where
    fromString = OperationId

instance ToHttpApiData OperationId where
    toUrlPiece (OperationId operation) = pack operation

-- | LXD operation status.
type OperationStatus = String

-- | LXD list of all operations.
newtype AllOperations = AllOperations (Map OperationStatus [OperationId])
                      deriving (Show)

instance FromJSON AllOperations where
    parseJSON v = AllOperations <$> parseJSON v

-- | LXD operation.
--
-- Returned when querying @GET \/1.0\/operations\/\<uuid\>@.
data Operation = Operation {
    operationId :: String
  , operationClass :: String
  , operationCreatedAt :: String
  , operationUpdatedAt :: String
  , operationStatus :: OperationStatus
  , operationStatusCode :: Int
  , operationMetadata :: Value
  , operationMayCancel :: Bool
  , operationErr :: String
} deriving (Show)

instance FromJSON Operation where
    parseJSON = withObject "Operation" $ \v -> Operation
        <$> v .: "id"
        <*> v .: "class"
        <*> v .: "created_at"
        <*> v .: "updated_at"
        <*> v .: "status"
        <*> v .: "status_code"
        <*> v .: "metadata"
        <*> v .: "may_cancel"
        <*> v .: "err"

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
