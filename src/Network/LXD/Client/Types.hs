{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.LXD.Client.Types (
  -- * Generic responses
  GenericResponse(..)
, Response
, AsyncResponse
, ResponseType(..)
, StatusCode(..)
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
  -- ** Configuration
, ContainerPut(..)
, ContainerPatch(..)
, ContainerRename(..)
  -- ** State
, MemoryState(..)
, NetworkState(..)
, NetworkAddress(..)
, NetworkCounters(..)
, ContainerState(..)
, StateAction(..)
, ContainerPutState(..)
, containerNewState
  -- ** Creating containers
, ContainerCreateRequest(..)
, containerCreateRequest
, ContainerSource(..)
  -- ** Deleting containers
, ContainerDeleteRequest(..)
  -- ** Executing commands
, ExecParams(..)
, ExecRequest(..)
, ExecResponseMetadataImmediate
, ExecResponseMetadataWebsocket(..)
, ExecResponseMetadata
  -- ** Working with file descriptors
, Secret(..)
, FdSet(..)
, Fds(..)
, ExecFds
  -- ** Working with files
, Gid(..)
, Uid(..)
, FileMode(..)
, FileType(..)
, RawFileResponse(..)
, rawFileResponseBody
, fileResponse
, FileResponse(..)
, PathResponse(..)
  -- ** Referencing containers
, LocalContainer(..)

  -- * Images
  -- ** Querying information
, ImageId(..)
, Image(..)
, ImageAlias(..)
, defaultImageAlias
, ImageProperties(..)
, ImageAliasName(..)
  -- ** Creating and publishing new images
, ImageCreateRequest(..)
, imageCreateRequest
, ImageSource(..)
  -- ** Deleting images
, ImageDeleteRequest(..)
  -- ** Referencing images
, LocalImageByAlias(..)
, LocalImageByFingerprint(..)
, RemoteImage(..)
, remoteImage
, remoteImageId

  -- * Networks
, NetworkName(..)
, Network(..)
, NetworkCreateRequest(..)
, NetworkConfigRequest(..)

  -- * Profiles
, ProfileName(..)
, Profile(..)
, ProfileCreateRequest(..)
, ProfileConfigRequest(..)

  -- * Operations
, OperationId(..)
, OperationStatus
, AllOperations(..)
, Operation(..)
, OperationProgress(..)

  -- * Events
, EventType(..)
, Event(..)
, EventMetadata(..)

  -- * Servant Helpers
, JsonOrBinary
) where

import Network.LXD.Prelude
import qualified Prelude as P

import Data.Aeson
import Data.Bimap (Bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map

import Network.HTTP.Media.MediaType (MediaType, (//))

import Servant.API.ContentTypes (Accept(..), MimeUnrender(..))

import Web.Internal.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

-- | Generic LXD API response object.
data GenericResponse op a = Response {
    responseType :: ResponseType
  , status :: String
  , statusCode :: StatusCode
  , responseOperation :: op
  , errorCode :: Int
  , error :: String
  , metadata :: a
} deriving (Show)

-- | LXD API synchronous repsonse object, without resulting operation.
type Response a = GenericResponse String a

-- | LXD API asynchronous response object, with resulting operation
type AsyncResponse a = GenericResponse OperationId (BackgroundOperation a)

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
  , backgroundOperationStatusCode :: StatusCode
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

instance ToJSON ContainerName where
    toJSON (ContainerName name) = toJSON name

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
  , containerConfig :: Map String String
  , containerCreatedAt :: String
  , containerDevices :: Map String (Map String String)
  , containerEphemeral :: Bool
  , containerProfiles :: [String]
  , containerStateful :: Bool
  , containerExpandedConfig :: Map String String
  , containerExpandedDevices :: Map String (Map String String)
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

-- | Used to set the configuration of an LXD container.
--
-- Used when querying @PUT \/1.0\/containers\/\<name\>@.
data ContainerPut = ContainerPut {
    containerPutArchitecture :: String
  , containerPutConfig :: Map String String
  , containerPutDevices :: Map String (Map String String)
  , containerPutEphemeral :: Bool
  , containerPutProfiles :: [String]
} deriving (Show)

instance ToJSON ContainerPut where
    toJSON ContainerPut{..} = object[
        "architecture" .= containerPutArchitecture
      , "config"       .= containerPutConfig
      , "devices"      .= containerPutDevices
      , "ephemeral"    .= containerPutEphemeral
      , "profiles"     .= containerPutProfiles
      ]

-- | Used to patch the configuration of an LXD container.
--
-- Used when querying @PATCH \/1.0\/containers\/\<name\>@.
data ContainerPatch = ContainerPatch {
    containerPatchArchitecture :: Maybe String
  , containerPatchConfig :: Maybe (Map String String)
  , containerPatchDevices :: Maybe (Map String (Map String String))
  , containerPatchEphemeral :: Maybe Bool
  , containerPatchProfiles :: Maybe [String]
} deriving (Show)

instance ToJSON ContainerPatch where
    toJSON ContainerPatch{..} = object $ catMaybes [
        (.=) <$> pure "architecture" <*> containerPatchArchitecture
      , (.=) <$> pure "config"       <*> containerPatchConfig
      , (.=) <$> pure "devices"      <*> containerPatchDevices
      , (.=) <$> pure "ephemeral"    <*> containerPatchEphemeral
      , (.=) <$> pure "profiles"     <*> containerPatchProfiles
      ]

-- | Used to rename a container to the given name.
--
-- Used when querying @POST \/1.0\/containers\/\<name\>@.
newtype ContainerRename = ContainerRename String deriving (Show)

instance ToJSON ContainerRename where
    toJSON (ContainerRename name) = object [ "name" .= name ]

-- | Memory state of an LXD container. As used by 'ContainerState'.
data MemoryState = MemoryState {
    memoryStateUsage :: Integer
  , memoryStateUsagePeak :: Integer
  , memoryStateSwapUsage :: Integer
  , memoryStateSwapUsagePeak :: Integer
} deriving (Show)

instance FromJSON MemoryState where
    parseJSON = withObject "MemoryState" $ \v -> do
        memoryStateUsage         <- v .: "usage"
        memoryStateUsagePeak     <- v .: "usage_peak"
        memoryStateSwapUsage     <- v .: "swap_usage"
        memoryStateSwapUsagePeak <- v .: "swap_usage_peak"
        return MemoryState{..}

-- | Network state of an LXD container network device. As used by 'ContainerState'.
data NetworkState = NetworkState {
    networkStateAddresses :: [NetworkAddress]
  , networkStateCounters :: NetworkCounters
  , networkStateHwaddr :: String
  , networkStateHostName :: String
  , networkStateMtu :: Int
  , networkStateState :: String
  , networkStateType :: String
} deriving (Show)

instance FromJSON NetworkState where
    parseJSON = withObject "NetworkState" $ \v -> do
        networkStateAddresses <- v .: "addresses"
        networkStateCounters  <- v .: "counters"
        networkStateHwaddr    <- v .: "hwaddr"
        networkStateHostName  <- v .: "host_name"
        networkStateMtu       <- v .: "mtu"
        networkStateState     <- v .: "state"
        networkStateType      <- v .: "type"
        return NetworkState{..}

-- | Network address of an LXD container network device. As used by 'NetworkState'.
data NetworkAddress = NetworkAddress {
    networkAddressFamily :: String
  , networkAddressAddress :: String
  , networkAddressNetmask :: String
  , networkAddressScope :: String
} deriving (Show)

instance FromJSON NetworkAddress where
    parseJSON = withObject "NetworkAddress" $ \v -> do
        networkAddressFamily  <- v .: "family"
        networkAddressAddress <- v .: "address"
        networkAddressNetmask <- v .: "netmask"
        networkAddressScope   <- v .: "scope"
        return NetworkAddress{..}

-- | Collection of statistics of an LXD container network device. As used by 'NetworkState'.
data NetworkCounters = NetworkCounters {
    networkCountersBytesReceived :: Integer
  , networkCountersBytesSent :: Integer
  , networkCountersPacketsReceived :: Integer
  , networkCountersPacketsSent :: Integer
} deriving (Show)

instance FromJSON NetworkCounters where
    parseJSON = withObject "NetworkCounters" $ \v -> do
        networkCountersBytesReceived   <- v .: "bytes_received"
        networkCountersBytesSent       <- v .: "bytes_sent"
        networkCountersPacketsReceived <- v .: "packets_received"
        networkCountersPacketsSent     <- v .: "packets_sent"
        return NetworkCounters{..}

-- | State of an LXD container.
--
-- Used when querying @GET \/1.0\/container\/\<name\>\/state@.
data ContainerState = ContainerState {
    containerStateStatus :: String
  , containerStateStatusCode :: StatusCode
  , containerStateCpu :: Integer
  , containerStateDisk :: Map String Integer
  , containerStateMemory :: MemoryState
  , containerStateNetwork :: Map String NetworkState
  , containerStatePid :: Int
  , containerStateProcesses :: Int
} deriving (Show)

instance FromJSON ContainerState where
    parseJSON = withObject "ContainerState" $ \v -> do
        cpu <- v .: "cpu"
        let containerStateCpu = fromMaybe 0 $ Map.lookup ("usage" :: String) cpu

        containerStateStatus     <- v .: "status"
        containerStateStatusCode <- v .: "status_code"
        containerStateDisk       <- v .: "disk"
        containerStateMemory     <- v .: "memory"
        containerStateNetwork    <- v .: "network"
        containerStatePid        <- v .: "pid"
        containerStateProcesses  <- v .: "processes"
        return ContainerState{..}

-- | State change action for an LXD container, as used by 'ContainerPutState'.
data StateAction = Stop
                 | Start
                 | Restart
                 | Freeze
                 | Unfreeze
                 deriving (Eq, Show)

instance ToJSON StateAction where
    toJSON Stop     = toJSON ("stop"     :: Text)
    toJSON Start    = toJSON ("start"    :: Text)
    toJSON Restart  = toJSON ("restart"  :: Text)
    toJSON Freeze   = toJSON ("freeze"   :: Text)
    toJSON Unfreeze = toJSON ("unfreeze" :: Text)

-- | State change request for an LXD container.
--
-- Used when querying @PUT \/1.0\/container\/\<name\>\/state@.
data ContainerPutState = ContainerPutState {
    containerPutStateAction :: StateAction
  , containerPutStateTimeout :: Int
  , containerPutStateForce :: Bool
  , containerPutStateStateful :: Bool
} deriving (Show)

instance ToJSON ContainerPutState where
    toJSON ContainerPutState{..} = object [
        "action"   .= containerPutStateAction
      , "timeout"  .= containerPutStateTimeout
      , "force"    .= containerPutStateForce
      , "stateful" .= containerPutStateStateful
      ]

containerNewState :: StateAction -> Bool -> ContainerPutState
containerNewState action force = ContainerPutState {
    containerPutStateAction = action
  , containerPutStateTimeout = 30
  , containerPutStateForce = force
  , containerPutStateStateful = False
  }

-- | LXD create container request object.
--
-- Used when querying @POST \/1.0\/containers@.
data ContainerCreateRequest = ContainerCreateRequest {
    containerCreateRequestName :: String
  , containerCreateRequestArchitecture :: String
  , containerCreateRequestProfiles :: [String]
  , containerCreateRequestEphemeral :: Bool
  , containerCreateRequestConfig :: Map String String
  , containerCreateRequestDevices :: Map String (Map String String)
  , containerCreateRequestInstanceType :: Maybe String
  , containerCreateRequestSource :: ContainerSource
  } deriving (Show)

instance ToJSON ContainerCreateRequest where
    toJSON ContainerCreateRequest{..} = object $ [
        "name" .= containerCreateRequestName
      , "architecture" .= containerCreateRequestArchitecture
      , "profiles" .= containerCreateRequestProfiles
      , "ephemeral" .= containerCreateRequestEphemeral
      , "config" .= containerCreateRequestConfig
      , "devices" .= containerCreateRequestDevices
      , "source" .= containerCreateRequestSource
      ] ++ catMaybes [
        (.=) <$> pure "instance_type" <*> containerCreateRequestInstanceType
      ]

-- | Create a default 'ContainerCreateRequest'.
containerCreateRequest :: String -> ContainerSource -> ContainerCreateRequest
containerCreateRequest name src = ContainerCreateRequest {
    containerCreateRequestName = name
  , containerCreateRequestArchitecture = "x86_64"
  , containerCreateRequestProfiles = ["default"]
  , containerCreateRequestEphemeral = False
  , containerCreateRequestConfig = mempty
  , containerCreateRequestDevices = mempty
  , containerCreateRequestInstanceType = Nothing
  , containerCreateRequestSource = src
  }

-- | Source for creating a container, as used by 'ContainerCreateRequest'.
data ContainerSource = ContainerSourceLocalByAlias LocalImageByAlias             -- ^ Container based on a local image with a certain alias.
                     | ContainerSourceLocalByFingerprint LocalImageByFingerprint -- ^ Container based on a local image with a certain alias.
                     | ContainerSourceNone                                             -- ^ Container without a pre-populated rootfs.
                     | ContainerSourceRemote RemoteImage                         -- ^ Continer based on a public remote image.
                     deriving (Show)

instance ToJSON ContainerSource where
    toJSON (ContainerSourceLocalByAlias (LocalImageByAlias alias)) = object [
        "type" .= ("image" :: String)
      , "alias" .= alias
      ]
    toJSON (ContainerSourceLocalByFingerprint (LocalImageByFingerprint img)) = object [
        "type" .= ("image" :: String)
      , "fingerprint" .= img
      ]
    toJSON ContainerSourceNone = object [
        "type" .= ("none" :: String)
      ]
    toJSON (ContainerSourceRemote RemoteImage{..}) = object $ [
        "type" .= ("image" :: String)
      , "mode" .= ("pull" :: String)
      , "server" .= remoteImageServer
      ] ++ catMaybes [
        (.=) <$> pure "secret" <*> remoteImageSecret
      , (.=) <$> pure "certificate" <*> remoteImageCertificate
      , (.=) <$> pure "alias" <*> remoteImageAlias
      , (.=) <$> pure "fingerprint" <*> remoteImageFingerprint
      ]
      where
        remoteImageAlias = either Just (const Nothing) remoteImageAliasOrFingerprint :: Maybe ImageAliasName
        remoteImageFingerprint = either (const Nothing) Just remoteImageAliasOrFingerprint :: Maybe ImageId

-- | Source for a local image, specified by its alias.
newtype LocalImageByAlias = LocalImageByAlias ImageAliasName deriving (Show)

instance IsString LocalImageByAlias where
    fromString = LocalImageByAlias . ImageAliasName

-- | Source for a local image, specified by its fingerprint
newtype LocalImageByFingerprint = LocalImageByFingerprint ImageId deriving (Show)

-- | Source for an image from a public or private remote.
data RemoteImage = RemoteImage {
    remoteImageServer :: String
  , remoteImageSecret :: Maybe String
  , remoteImageCertificate :: Maybe String
  , remoteImageAliasOrFingerprint :: Either ImageAliasName ImageId
  } deriving (Show)

-- | Create a remote image reference form a public remote.
remoteImage :: String -> ImageAliasName -> RemoteImage
remoteImage server alias = RemoteImage {
    remoteImageServer = server
  , remoteImageSecret = Nothing
  , remoteImageCertificate = Nothing
  , remoteImageAliasOrFingerprint = Left alias }

-- | Create a remote image reference form a public remote, using an image ID.
remoteImageId :: String -> ImageId -> RemoteImage
remoteImageId server img = RemoteImage {
    remoteImageServer = server
  , remoteImageSecret = Nothing
  , remoteImageCertificate = Nothing
  , remoteImageAliasOrFingerprint = Right img }

-- | LXD delete container request object.
--
-- Used when querying @DELETE \/1.0\/containers\/\<name\>@.
data ContainerDeleteRequest = ContainerDeleteRequest

instance Default ContainerDeleteRequest where
    def = ContainerDeleteRequest

instance ToJSON ContainerDeleteRequest where
    toJSON _ = object []

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

-- | Group ID of a container file.
newtype Gid = Gid Int deriving (Bounded, Enum, Eq, FromHttpApiData, Integral, Num, Ord, Read, Real, Show, ToHttpApiData)

-- | User ID of a container file.
newtype Uid = Uid Int deriving (Bounded, Enum, Eq, FromHttpApiData, Integral, Num, Ord, Read, Real, Show, ToHttpApiData)

-- | Mode of a container file. Encoded in standard octal notation, e.g. @0644@.
newtype FileMode = FileMode String deriving (Eq, FromHttpApiData, IsString, Ord, Read, Show, ToHttpApiData)

-- | Type of a container file. Can be one of @directory@, @file@ or @symlink@.
newtype FileType = FileType String deriving (Eq, FromHttpApiData, IsString, Ord, Read, Show, ToHttpApiData)

-- | LXD file response object, representing either a file or a directory.
--
-- Used by the @GET \/1.0\/containers\/\<name\>\/files\/\<filename\>@ endpoints.
data FileResponse = File ByteString
                  | Directory (Response [String])
                  deriving (Show)

-- | Raw file response, not yet decoded, used because of a bug in Servant.
--
-- Use headers to get actual content type.
data RawFileResponse = RawFileResponse MediaType ByteString deriving (Show)

-- | Get the body of a 'RawFileResponse'.
rawFileResponseBody :: RawFileResponse -> ByteString
rawFileResponseBody (RawFileResponse _ bs) = bs

-- | Construct a file response from a type and a 'ByteString'.
fileResponse :: FileType -> ByteString -> Either String FileResponse
fileResponse "file" bs = Right $ File bs
fileResponse "directory" bs = eitherDecode bs
fileResponse t _ = Left $ "unsupported file type: " ++ show t

instance FromJSON FileResponse where
    parseJSON v = Directory <$> do
        r <- parseJSON v
        -- If the directory is empty, the LXD server will send @null@ instead
        -- of the empty list. Handle that case here.
        case metadata r of
            Nothing -> return r { metadata = [] }
            Just xs -> return r { metadata = xs }

instance MimeUnrender JsonOrBinary RawFileResponse where
    mimeUnrenderWithType _ mt = Right . RawFileResponse mt

-- | LXD path response object, which is a file and metadata.
--
-- Used by the @\/1.0\/containers\/\<name\>\/files\/...@ endpoints.
data PathResponse = PathResponse {
    pathUid :: Uid
  , pathGid :: Gid
  , pathMode :: FileMode
  , pathType :: FileType
  , getFile :: FileResponse
  } deriving (Show)

-- | Reference to a local container, as used by 'ImageSource'.
newtype LocalContainer = LocalContainer ContainerName deriving (Show)

instance IsString LocalContainer where
    fromString = LocalContainer . ContainerName

-- | LXD image identifier.
newtype ImageId = ImageId String deriving (Eq, Show)

instance FromJSON ImageId where
    parseJSON = withText "ImageId" $ \text ->
        let prefix = "/1.0/images/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse image id: no prefix " ++ prefix
            Just img -> return $ ImageId img

instance ToJSON ImageId where
    toJSON (ImageId image) = toJSON image

instance ToHttpApiData ImageId where
    toUrlPiece (ImageId img) = pack img

-- | Alias of an image.
--
-- Returned when querying @GET \/1.0\/images/aliases\/\<name\>@,
-- and as a part of @GET \/1.0\/images\/\<fingerprint\>@.
data ImageAlias = ImageAlias {
    imageAliasName :: String
  , imageAliasDescription :: String
  , imageAliasTarget :: Maybe String
  } deriving (Show)

instance FromJSON ImageAlias where
    parseJSON = withObject "ImageAlias" $ \v -> ImageAlias
        <$> v .: "name"
        <*> v .: "description"
        <*> v .:? "target"

instance ToJSON ImageAlias where
    toJSON ImageAlias{..} = object [
        "name" .= imageAliasName
      , "description" .= imageAliasDescription
      ]

-- | Create a default 'ImageAlias', with empty description and target.
defaultImageAlias :: String -> ImageAlias
defaultImageAlias name = ImageAlias {
    imageAliasName = name
  , imageAliasDescription = ""
  , imageAliasTarget = Nothing
  }

-- | Properties of an image.
data ImageProperties = ImageProperties {
    imagePropertiesArchitecture :: Maybe String
  , imagePropertiesDescription :: Maybe String
  , imagePropertiesOs :: Maybe String
  , imagePropertiesRelease :: Maybe String
  } deriving (Show)

instance FromJSON ImageProperties where
    parseJSON = withObject "ImageProperties" $ \v -> ImageProperties
        <$> v .:? "architecture"
        <*> v .:?  "description"
        <*> v .:? "os"
        <*> v .:? "release"

-- | LXD image information.
--
-- Returned when querying @GET \/1.0\/images\/\<fingerprint\>@.
data Image = Image {
    imageAllAliases :: [ImageAlias]
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

-- | LXD alias name.
--
-- Returned when querying @GET \/1.0\/images/aliases@.
newtype ImageAliasName = ImageAliasName String deriving (Eq, Show)

instance IsString ImageAliasName where
    fromString = ImageAliasName

instance FromJSON ImageAliasName where
    parseJSON = withText "ImageAliasName" $ \text ->
        let prefix = "/1.0/images/aliases/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse image alias name id: no prefix " ++ prefix
            Just name -> return $ ImageAliasName name

instance ToJSON ImageAliasName where
    toJSON (ImageAliasName image) = toJSON image

instance ToHttpApiData ImageAliasName where
    toUrlPiece (ImageAliasName name) = pack name

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

-- | LXD image create request object.
--
-- Used when querying @POST \/1.0\/images@.
data ImageCreateRequest = ImageCreateRequest {
    imageCreateRequestPublic :: Bool
  , imageCreateRequestAutoUpdate :: Bool
  , imageCreateRequestProperties :: Value
  , imageCreateRequestAliases :: [ImageAlias]
  , imageCreateRequestSource :: ImageSource
  } deriving (Show)

-- | Construct a new default 'ImageCreateRequest'.
imageCreateRequest :: ImageSource -> ImageCreateRequest
imageCreateRequest src = ImageCreateRequest {
    imageCreateRequestPublic = False
  , imageCreateRequestAutoUpdate = False
  , imageCreateRequestProperties = object []
  , imageCreateRequestAliases = []
  , imageCreateRequestSource = src
  }

instance ToJSON ImageCreateRequest where
    toJSON ImageCreateRequest{..} = object [
        "public" .= imageCreateRequestPublic
      , "auto_update" .= imageCreateRequestAutoUpdate
      , "properties" .= imageCreateRequestProperties
      , "aliases" .= imageCreateRequestAliases
      , "source" .= imageCreateRequestSource
      ]

-- | A generic image source, used by 'ImageCreateRequest'.
data ImageSource = ImageSourceRemoteImage RemoteImage
                 | ImageSourceLocalContainer LocalContainer
                 deriving (Show)

instance ToJSON ImageSource where
    toJSON (ImageSourceLocalContainer (LocalContainer name)) = object [
        "type" .= ("container" :: String)
      , "name" .= name
      ]
    toJSON (ImageSourceRemoteImage RemoteImage{..}) = object $ [
        "type" .= ("image" :: String)
      , "mode" .= ("pull" :: String)
      , "server" .= remoteImageServer
      ] ++ catMaybes [
        (.=) <$> pure "secret" <*> remoteImageSecret
      , (.=) <$> pure "certificate" <*> remoteImageCertificate
      , (.=) <$> pure "alias" <*> remoteImageAlias
      , (.=) <$> pure "fingerprint" <*> remoteImageFingerprint
      ]
      where
        remoteImageAlias = either Just (const Nothing) remoteImageAliasOrFingerprint :: Maybe ImageAliasName
        remoteImageFingerprint = either (const Nothing) Just remoteImageAliasOrFingerprint :: Maybe ImageId

-- | LXD image delete request object.
--
-- Used when querying @DELETE \/1.0\/images\/\<fingerprint\>@.
data ImageDeleteRequest = ImageDeleteRequest

instance Default ImageDeleteRequest where
    def = ImageDeleteRequest

instance ToJSON ImageDeleteRequest where
    toJSON _ = object []

-- | LXD network name.
newtype NetworkName = NetworkName String deriving (Eq, Show)

instance FromJSON NetworkName where
    parseJSON = withText "NetworkName" $ \text ->
        let prefix = "/1.0/networks/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse netwokr name: no prefix " ++ prefix
            Just name -> return $ NetworkName name

instance ToJSON NetworkName where
    toJSON (NetworkName name) = toJSON name

instance IsString NetworkName where
    fromString = NetworkName

instance ToHttpApiData NetworkName where
    toUrlPiece (NetworkName name) = pack name

-- | LXD network.
--
-- Returned when querying @GET \/1.0\/networks\/\<name\>@.
data Network = Network {
    networkName :: String
  , networkConfig :: Map String String
  , networkManaged :: Bool
  , networkType :: String
  , networkUsedBy :: [ContainerName]
} deriving (Show)

instance FromJSON Network where
    parseJSON = withObject "Network" $ \v -> do
        networkName    <- v .: "name"
        networkConfig  <- v .: "config"
        networkManaged <- v .: "managed"
        networkType    <- v .: "type"
        networkUsedBy  <- v .: "used_by"
        return Network{..}

-- | LXD network create request.
--
-- Used when querying @POST \/1.0\/networks@.
data NetworkCreateRequest = NetworkCreateRequest {
    networkCreateRequestName :: NetworkName
  , networkCreateRequestDescription :: String
  , networkCreateRequestConfig :: Map String String
} deriving (Show)

instance ToJSON NetworkCreateRequest where
    toJSON NetworkCreateRequest{..} = object [
        "name"        .= networkCreateRequestName
      , "description" .= networkCreateRequestDescription
      , "config"      .= networkCreateRequestConfig
      ]

-- | LXD network config update request.
--
-- Used when querying @PUT/PATCH \/1.0\/networks\/\<name\>@.
newtype NetworkConfigRequest = NetworkConfigRequest {
    networkConfigRequestConfig :: Map String String
} deriving (Show)

instance ToJSON NetworkConfigRequest where
    toJSON NetworkConfigRequest{..} = object [ "config" .= networkConfigRequestConfig ]

-- | LXD profile name.
--
-- Returned by @GET \/1.0\/profiles@.
newtype ProfileName = ProfileName String deriving (Eq, Show)

instance FromJSON ProfileName where
    parseJSON = withText "ProfileName" $ \text ->
        let prefix = "/1.0/profiles/" in
        case stripPrefix prefix (unpack text) of
            Nothing -> fail $ "could not parse profile name: no prefix " ++ prefix
            Just operation -> return $ ProfileName operation

instance IsString ProfileName where
    fromString = ProfileName

instance ToHttpApiData ProfileName where
    toUrlPiece (ProfileName name) = pack name

-- | LXD profile.
--
-- Returned by @GET \/1.0\/profiles\/\<name\>@.
data Profile = Profile {
    profileName :: String
  , profileDescription :: String
  , profileConfig :: Map String String
  , profileDevices :: Map String (Map String String)
  , profileUsedBy :: [ContainerName]
} deriving (Show)

instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \v -> do
        profileName        <- v .: "name"
        profileDescription <- v .: "description"
        profileConfig      <- v .: "config"
        profileDevices     <- v .: "devices"
        profileUsedBy      <- v .: "use_by"
        return Profile{..}

-- | LXD profile create request.
--
-- Used when querying @POST \/1.0\/profiles@.
data ProfileCreateRequest = ProfileCreateRequest {
    profileCreateRequestName :: String
  , profileCreateRequestDescription :: String
  , profileCreateRequestConfig :: Map String String
  , profileCreateRequestDevices :: Map String (Map String String)
} deriving (Show)

instance ToJSON ProfileCreateRequest where
    toJSON ProfileCreateRequest{..} = object [
        "name"        .= profileCreateRequestName
      , "description" .= profileCreateRequestDescription
      , "config"      .= profileCreateRequestConfig
      , "devices"     .= profileCreateRequestDevices
      ]

-- | LXD profile config request.
--
-- Used when querying @PUT/PATCH \/1.0\/profiles\/\<name\>@.
data ProfileConfigRequest = ProfileConfigRequest {
    profileConfigRequestConfig :: Maybe (Map String String)
  , profileConfigRequestDescription :: Maybe String
  , profileConfigRequestDevices :: Maybe (Map String (Map String String))
} deriving (Show)

instance ToJSON ProfileConfigRequest where
    toJSON ProfileConfigRequest{..} = object $ catMaybes [
        (.=) <$> pure "config"      <*> profileConfigRequestConfig
      , (.=) <$> pure "description" <*> profileConfigRequestDescription
      , (.=) <$> pure "devices"     <*> profileConfigRequestDevices
      ]
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
  , operationStatusCode :: StatusCode
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

-- | Progress of an LXD operation.
--
-- You can try to decode 'operationMetadata' if the 'operationStatusCode' is
-- 'SRunning' to see of the operation contains progress information.
--
-- The embedded 'String' value is in the format @87% (12.04 MB/s)@.
newtype OperationProgress = OperationProgress String deriving (Show)

instance FromJSON OperationProgress where
    parseJSON = withObject "OperationProgress" $ \v ->
        OperationProgress <$> (v .: "download_progress")

-- | Type of an LXD event from the @\/1.0\/events@ handle.
data EventType = EventTypeLogging
               | EventTypeOperation
               deriving (Eq, Show)

instance ToHttpApiData EventType where
    toUrlPiece EventTypeLogging   = "logging"
    toUrlPiece EventTypeOperation = "operation"

instance FromHttpApiData EventType where
    parseUrlPiece "logging"   = Right EventTypeLogging
    parseUrlPiece "operation" = Right EventTypeOperation
    parseUrlPiece t           = Left $ "unknown event type: " <> t

instance FromJSON EventType where
    parseJSON = withText "EventType" $ \t -> case t of
        "logging"   -> return EventTypeLogging
        "operation" -> return EventTypeOperation
        t'          -> fail $ "unknown event type: " ++ show t'

-- | An event received from @\/1.0\/events@.
data Event = Event {
    eventTimestamp :: String
  , eventType :: EventType
  , eventMetadata :: EventMetadata
} deriving (Show)

instance FromJSON Event where
    parseJSON = withObject "Event" $ \v -> do
        eventTimestamp <- v .: "timestamp"
        eventType <- (v .: "type") >>= parseJSON
        eventMetadata <- case eventType of
            EventTypeLogging   -> EventLoggingMetadata   <$> (v .: "metadata")
            EventTypeOperation -> EventOperationMetadata <$> (v .: "metadata")
        return Event{..}

-- | Metadata of an event.
data EventMetadata = EventLoggingMetadata Value
                   | EventOperationMetadata Operation
                   deriving (Show)

-- | The type of a generic response object.
data ResponseType = Sync | Async deriving (Eq, Show)

instance FromJSON ResponseType where
    parseJSON = withText "ResponseType" $ \case
        "sync"  -> pure Sync
        "async" -> pure Async
        v       -> fail $ "Unknown value: " ++ show v

data StatusCode = SCreated
                | SStopped
                | SRunning
                | SSuccess
                | SFailure
                | SCancelled
                | SOther Int
                deriving (Eq, Ord, Show)

statusCodeMap :: Bimap Int StatusCode
statusCodeMap = Bimap.fromList [
    (100, SCreated)
  , (102, SStopped)
  , (103, SRunning)
  , (200, SSuccess)
  , (400, SFailure)
  , (401, SCancelled)
  ]

statusCodeFromInt :: Int -> StatusCode
statusCodeFromInt v = fromMaybe (SOther v) $ Bimap.lookup v statusCodeMap

statusCodeToInt :: StatusCode -> Int
statusCodeToInt (SOther v) = v
statusCodeToInt c = fromMaybe (P.error $ "unindexed status code: " ++ show c) $ Bimap.lookupR c statusCodeMap

instance FromJSON StatusCode where
    parseJSON v = statusCodeFromInt <$> parseJSON v

instance ToJSON StatusCode where
    toJSON = toJSON . statusCodeToInt

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

data JsonOrBinary

instance Accept JsonOrBinary where
    contentTypes _ = ("application" // "octet-stream") :| ["application" // "json"]
