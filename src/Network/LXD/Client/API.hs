{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
-- | This module exposes a low-level Servant-built API for the LXD daemon.
--
-- You can query this API using Servant functions and a client created
-- from "Network.LXD.Client".
--
-- You probably want to use the commands exposed in
-- "Network.LXD.Client.Commands" instead.
module Network.LXD.Client.API (
  -- * API
  FailureResponse(..)
  -- ** Information
, supportedVersions
, apiConfig
, trustedCertificates

  -- ** Containers
  -- *** Querying informaiton
, containerNames
, containerCreate
, container
, containerDelete
  -- *** Configuration
, containerPut
, containerPatch
, containerRename
  -- *** State
, containerState
, containerPutState
  -- *** Executing commands
, containerExecImmediate
, containerExecWebsocketInteractive
, containerExecWebsocketNonInteractive
  -- *** Working with files
, WriteMode(..)
, containerGetPath
, containerPostPath
, containerDeletePath

  -- ** Images
, imageIds
, imageCreate
, imageAliases
, imageAlias
, image
, imageDelete

  -- ** Networks
, networkList
, networkCreate
, network
, networkPut
, networkPatch
, networkDelete

  -- ** Profiles
, profileList
, profileCreate
, profile
, profilePut
, profilePatch
, profileDelete

  -- ** Storage
, poolList
, poolCreate
, pool
, poolPut
, poolPatch
, poolDelete

  -- * Volumes
, volumeList
, volumeCreate
, volume
, volumePut
, volumePatch
, volumeDelete

  -- ** Operations
, operationIds
, operation
, operationCancel
, operationWait
, operationWebSocket

  -- ** WebSocket communciations
, readAllWebSocket
, writeAllWebSocket

  -- * Helpers
, ExecClient
) where

import Network.LXD.Client.Internal.Prelude

import Control.Concurrent (MVar, takeMVar)
import Control.Exception (Exception, catch, throwIO)
import Control.Monad.Reader (asks)

import Data.Aeson (FromJSON, Value, eitherDecode)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.WebSockets as WS

import Servant.API
import Servant.Client hiding (FailureResponse)

import Web.HttpApiData (FromHttpApiData, ToHttpApiData, toHeader, parseHeader)

import Network.LXD.Client.Types

type API = Get '[JSON] (Response [ApiVersion])
      :<|> "1.0" :> Get '[JSON] (Response ApiConfig)
      :<|> "1.0" :> "certificates" :> Get '[JSON] (Response [CertificateHash])
      :<|> "1.0" :> "containers" :> Get '[JSON] (Response [ContainerName])
      :<|> "1.0" :> "containers" :> ReqBody '[JSON] ContainerCreateRequest :> Post '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> Get '[JSON] (Response Container)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerDeleteRequest :> Delete '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerPut :> Put '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerPatch :> Patch '[JSON] (Response Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerRename :> Post '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> "state" :> Get '[JSON] (Response ContainerState)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> "state" :> ReqBody '[JSON] ContainerPutState :> Put '[JSON] (AsyncResponse Value)
      :<|> ExecAPI 'ExecImmediate
      :<|> ExecAPI 'ExecWebsocketInteractive
      :<|> ExecAPI 'ExecWebsocketNonInteractive
      :<|> "1.0" :> "images" :> Get '[JSON] (Response [ImageId])
      :<|> "1.0" :> "images" :> ReqBody '[JSON] ImageCreateRequest :> Post '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "images" :> "aliases" :> Get '[JSON] (Response [ImageAliasName])
      :<|> "1.0" :> "images" :> "aliases" :> Capture "name" ImageAliasName :> Get '[JSON] (Response ImageAlias)
      :<|> "1.0" :> "images" :> Capture "id" ImageId :> Get '[JSON] (Response Image)
      :<|> "1.0" :> "images" :> Capture "id" ImageId :> ReqBody '[JSON] ImageDeleteRequest :> Delete '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "networks" :> Get '[JSON] (Response [NetworkName])
      :<|> "1.0" :> "networks" :> ReqBody '[JSON] NetworkCreateRequest :> Post '[JSON] (Response Value)
      :<|> "1.0" :> "networks" :> Capture "name" NetworkName :> Get '[JSON] (Response Network)
      :<|> "1.0" :> "networks" :> Capture "name" NetworkName :> ReqBody '[JSON] NetworkConfigRequest :> Put '[JSON] (Response Value)
      :<|> "1.0" :> "networks" :> Capture "name" NetworkName :> ReqBody '[JSON] NetworkConfigRequest :> Patch '[JSON] (Response Value)
      :<|> "1.0" :> "networks" :> Capture "name" NetworkName :> Delete '[JSON] (Response Value)
      :<|> "1.0" :> "profiles" :> Get '[JSON] (Response [ProfileName])
      :<|> "1.0" :> "profiles" :> ReqBody '[JSON] ProfileCreateRequest :> Post '[JSON] (Response Value)
      :<|> "1.0" :> "profiles" :> Capture "name" ProfileName :> Get '[JSON] (Response Profile)
      :<|> "1.0" :> "profiles" :> Capture "name" ProfileName :> ReqBody '[JSON] ProfileConfigRequest :> Put '[JSON] (Response Value)
      :<|> "1.0" :> "profiles" :> Capture "name" ProfileName :> ReqBody '[JSON] ProfileConfigRequest :> Patch '[JSON] (Response Value)
      :<|> "1.0" :> "profiles" :> Capture "name" ProfileName :> Delete '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Get '[JSON] (Response [PoolName])
      :<|> "1.0" :> "storage-pools" :> ReqBody '[JSON] PoolCreateRequest :> Post '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Capture "name" PoolName :> Get '[JSON] (Response Pool)
      :<|> "1.0" :> "storage-pools" :> Capture "name" PoolName :> ReqBody '[JSON] PoolConfigRequest :> Put '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Capture "name" PoolName :> ReqBody '[JSON] PoolConfigRequest :> Patch '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Capture "name" PoolName :> Delete '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Capture "pool" PoolName :> "volumes" :> Get '[JSON] (Response [VolumeName])
      :<|> "1.0" :> "storage-pools" :> Capture "pool" PoolName :> "volumes" :> ReqBody '[JSON] VolumeCreateRequest :> Post '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Capture "pool" PoolName :> "volumes" :> Capture "type" VolumeType :> Capture "volume" VolumeName :> Get '[JSON] (Response Volume)
      :<|> "1.0" :> "storage-pools" :> Capture "pool" PoolName :> "volumes" :> Capture "type" VolumeType :> Capture "volume" VolumeName :> ReqBody '[JSON] VolumeConfigRequest :> Put '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Capture "pool" PoolName :> "volumes" :> Capture "type" VolumeType :> Capture "volume" VolumeName :> ReqBody '[JSON] VolumeConfigRequest :> Patch '[JSON] (Response Value)
      :<|> "1.0" :> "storage-pools" :> Capture "pool" PoolName :> "volumes" :> Capture "type" VolumeType :> Capture "volume" VolumeName :> Delete '[JSON] (Response Value)
      :<|> "1.0" :> "operations" :> Get '[JSON] (Response AllOperations)
      :<|> "1.0" :> "operations" :> Capture "uuid" OperationId :> Get '[JSON] (Response Operation)
      :<|> "1.0" :> "operations" :> Capture "uuid" OperationId :> Delete '[JSON] (Response Value)
      :<|> "1.0" :> "operations" :> Capture "uuid" OperationId :> "wait" :> Get '[JSON] (Response Operation)


api :: Proxy API
api = Proxy

supportedVersions                    :: ClientM (Response [ApiVersion])
apiConfig                            :: ClientM (Response ApiConfig)
trustedCertificates                  :: ClientM (Response [CertificateHash])
containerNames                       :: ClientM (Response [ContainerName])
containerCreate                      :: ContainerCreateRequest -> ClientM (AsyncResponse Value)
container                            :: ContainerName -> ClientM (Response Container)
containerDelete                      :: ContainerName -> ContainerDeleteRequest -> ClientM (AsyncResponse Value)
containerPut                         :: ContainerName -> ContainerPut -> ClientM (AsyncResponse Value)
containerPatch                       :: ContainerName -> ContainerPatch -> ClientM (Response Value)
containerRename                      :: ContainerName -> ContainerRename -> ClientM (AsyncResponse Value)
containerState                       :: ContainerName -> ClientM (Response ContainerState)
containerPutState                    :: ContainerName -> ContainerPutState -> ClientM (AsyncResponse Value)
containerExecImmediate               :: ExecClient 'ExecImmediate
containerExecWebsocketInteractive    :: ExecClient 'ExecWebsocketInteractive
containerExecWebsocketNonInteractive :: ExecClient 'ExecWebsocketNonInteractive
imageIds                             :: ClientM (Response [ImageId])
imageCreate                          :: ImageCreateRequest -> ClientM (AsyncResponse Value)
imageAliases                         :: ClientM (Response [ImageAliasName])
imageAlias                           :: ImageAliasName -> ClientM (Response ImageAlias)
image                                :: ImageId -> ClientM (Response Image)
imageDelete                          :: ImageId -> ImageDeleteRequest -> ClientM (AsyncResponse Value)
networkList                          :: ClientM (Response [NetworkName])
networkCreate                        :: NetworkCreateRequest -> ClientM (Response Value)
network                              :: NetworkName -> ClientM (Response Network)
networkPut                           :: NetworkName -> NetworkConfigRequest -> ClientM (Response Value)
networkPatch                         :: NetworkName -> NetworkConfigRequest -> ClientM (Response Value)
networkDelete                        :: NetworkName -> ClientM (Response Value)
profileList                          :: ClientM (Response [ProfileName])
profileCreate                        :: ProfileCreateRequest -> ClientM (Response Value)
profile                              :: ProfileName -> ClientM (Response Profile)
profilePut                           :: ProfileName -> ProfileConfigRequest -> ClientM (Response Value)
profilePatch                         :: ProfileName -> ProfileConfigRequest -> ClientM (Response Value)
profileDelete                        :: ProfileName -> ClientM (Response Value)
poolList                             :: ClientM (Response [PoolName])
poolCreate                           :: PoolCreateRequest -> ClientM (Response Value)
pool                                 :: PoolName -> ClientM (Response Pool)
poolPut                              :: PoolName -> PoolConfigRequest -> ClientM (Response Value)
poolPatch                            :: PoolName -> PoolConfigRequest -> ClientM (Response Value)
poolDelete                           :: PoolName -> ClientM (Response Value)
volumeList                           :: PoolName -> ClientM (Response [VolumeName])
volumeCreate                         :: PoolName -> VolumeCreateRequest -> ClientM (Response Value)
volume'                              :: PoolName -> VolumeType -> VolumeName -> ClientM (Response Volume)
volumePut'                           :: PoolName -> VolumeType -> VolumeName -> VolumeConfigRequest -> ClientM (Response Value)
volumePatch'                         :: PoolName -> VolumeType -> VolumeName -> VolumeConfigRequest -> ClientM (Response Value)
volumeDelete'                        :: PoolName -> VolumeType -> VolumeName -> ClientM (Response Value)
operationIds                         :: ClientM (Response AllOperations)
operation                            :: OperationId -> ClientM (Response Operation)
operationCancel                      :: OperationId -> ClientM (Response Value)
operationWait                        :: OperationId -> ClientM (Response Operation)

supportedVersions                        :<|>
    apiConfig                            :<|>
    trustedCertificates                  :<|>
    containerNames                       :<|>
    containerCreate                      :<|>
    container                            :<|>
    containerDelete                      :<|>
    containerPut                         :<|>
    containerPatch                       :<|>
    containerRename                      :<|>
    containerState                       :<|>
    containerPutState                    :<|>
    containerExecImmediate               :<|>
    containerExecWebsocketInteractive    :<|>
    containerExecWebsocketNonInteractive :<|>
    imageIds                             :<|>
    imageCreate                          :<|>
    imageAliases                         :<|>
    imageAlias                           :<|>
    image                                :<|>
    imageDelete                          :<|>
    networkList                          :<|>
    networkCreate                        :<|>
    network                              :<|>
    networkPut                           :<|>
    networkPatch                         :<|>
    networkDelete                        :<|>
    profileList                          :<|>
    profileCreate                        :<|>
    profile                              :<|>
    profilePut                           :<|>
    profilePatch                         :<|>
    profileDelete                        :<|>
    poolList                             :<|>
    poolCreate                           :<|>
    pool                                 :<|>
    poolPut                              :<|>
    poolPatch                            :<|>
    poolDelete                           :<|>
    volumeList                           :<|>
    volumeCreate                         :<|>
    volume'                              :<|>
    volumePut'                           :<|>
    volumePatch'                         :<|>
    volumeDelete'                        :<|>
    operationIds                         :<|>
    operation                            :<|>
    operationCancel                      :<|>
    operationWait
    = client api

volume :: PoolName -> VolumeName -> ClientM (Response Volume)
volume p v@(VolumeName t _) = volume' p t v

volumePut :: PoolName -> VolumeName -> VolumeConfigRequest -> ClientM (Response Value)
volumePut p v@(VolumeName t _) = volumePut' p t v

volumePatch :: PoolName -> VolumeName -> VolumeConfigRequest -> ClientM (Response Value)
volumePatch p v@(VolumeName t _) = volumePatch' p t v

volumeDelete :: PoolName -> VolumeName -> ClientM (Response Value)
volumeDelete p v@(VolumeName t _) = volumeDelete' p t v

containerGetPath :: ContainerName -> FilePath -> ClientM PathResponse
containerGetPath name fp = do
    req <- pathRequest name fp
    res <- performRequest req
    let lookupHdr :: FromHttpApiData a => HTTP.HeaderName -> ClientM a
        lookupHdr n = lookupHdr' n (Client.responseHeaders res)

    hType <- lookupHdr "X-LXD-Type"
    hUid  <- lookupHdr "X-LXD-Uid"
    hGid  <- lookupHdr "X-LXD-Gid"
    hMode <- lookupHdr "X-LXD-Mode"
    case fileResponse hType (Client.responseBody res) of
        Left err -> error' $ "Could not decode " ++ show hType ++ ": " ++ err
        Right file -> return PathResponse {
                                  pathUid = hUid
                                , pathGid = hGid
                                , pathMode = hMode
                                , pathType = hType
                                , getFile = file
                                }
  where
    lookupHdr' :: FromHttpApiData a => HTTP.HeaderName -> [HTTP.Header] -> ClientM a
    lookupHdr' n xs = case find ((== n) . fst) xs of
        Nothing -> error' $ "Missing header in response: " ++ show n
        Just (_, v) -> case parseHeader v of
            Left err -> error' $ "Could not decode header " ++ show n
                              ++ " with value " ++ show v ++ ": " ++ show err
            Right v' -> return v'

    error' = liftIO . throwIO . DecodeError

data WriteMode = ModeOverwrite | ModeAppend deriving (Show)

containerPostPath :: ContainerName
                  -> FilePath
                  -> Maybe Uid
                  -> Maybe Gid
                  -> Maybe FileMode
                  -> FileType
                  -> Maybe WriteMode
                  -> ByteString
                  -> ClientM (Response Value)
containerPostPath name fp uid gid perm ftype mode body = do
    req <- pathRequest name fp
    let hdrs = catMaybes [ hdr "X-LXD-Uid" <$> uid
                         , hdr "X-LXD-Gid" <$> gid
                         , hdr "X-LXD-Mode" <$> perm
                         , hdr "X-LXD-Type" <$> Just ftype
                         , hdr "X-LXD-Write" . mode' <$> mode ]
    let req' = req { Client.requestHeaders = hdrs
                   , Client.requestBody = Client.RequestBodyLBS body }

    performJsonRequest req'
  where
    mode' ModeOverwrite = "overwrite" :: Text
    mode' ModeAppend    = "append"

    hdr :: ToHttpApiData v => HTTP.HeaderName -> v -> HTTP.Header
    hdr n v = (n, toHeader v)

containerDeletePath :: ContainerName -> FilePath -> ClientM (Response Value)
containerDeletePath name fp = do
    res <- pathRequest name fp
    performJsonRequest $ res { Client.method = "DELETE" }

operationWebSocket :: OperationId -> Secret -> String
operationWebSocket (OperationId oid) (Secret secret) =
    "/1.0/operations/" ++ oid ++ "/websocket?secret=" ++ secret

readAllWebSocket :: (ByteString -> IO ()) -> WS.ClientApp ()
readAllWebSocket f con = do
    m <- (Just <$> WS.receiveDataMessage con) `catch` handle'
    case m of Nothing             -> return ()
              Just (WS.Text _)    -> WS.sendClose con BL.empty
              Just (WS.Binary bs) -> f bs
                                  >> readAllWebSocket f con
  where
    handle' (WS.CloseRequest _ _) = return Nothing
    handle' e                     = throwIO e

writeAllWebSocket :: MVar (Maybe ByteString) -> WS.ClientApp ()
writeAllWebSocket input con = do
    i <- takeMVar input
    case i of
        Nothing -> WS.sendClose con BL.empty
        Just bs -> WS.sendBinaryData con bs
                >> writeAllWebSocket input con

type ExecAPI a = "1.0" :> "containers" :> Capture "name" ContainerName :> "exec" :> ReqBody '[JSON] (ExecRequest a) :> Post '[JSON] (AsyncResponse (ExecResponseMetadata a))

type ExecClient a = ContainerName -> ExecRequest a -> ClientM (AsyncResponse (ExecResponseMetadata a))

pathRequest :: ContainerName -> FilePath -> ClientM Client.Request
pathRequest (ContainerName name) fp = do
    BaseUrl{..} <- askBaseUrl
    return Client.defaultRequest {
        Client.method = "GET"
      , Client.host = fromString baseUrlHost
      , Client.port = baseUrlPort
      , Client.path = toStrict $ fromString baseUrlPath <> "/1.0/containers/" <> Char8.pack name <> "/files"
      , Client.queryString = toStrict $ "?path=" <> Char8.pack fp
      }

performRequest :: Client.Request -> ClientM (Client.Response ByteString)
performRequest req = do
    m <- askManager
    r <- liftIO $ Client.httpLbs req m

    let status = Client.responseStatus r
        statusCode' = HTTP.statusCode status
    unless (statusCode' >= 200 && statusCode' < 300) $
        liftIO . throwIO $ FailureResponse req r
    return r

performJsonRequest :: FromJSON a => Client.Request -> ClientM a
performJsonRequest req = do
    res <- performRequest req
    case eitherDecode (Client.responseBody res) of
        Left err -> liftIO . throwIO . DecodeError $ "Could not decode JSON body: " ++ err
        Right v -> return v

-- | Exception thrown in 'containerGetPath', 'containerDeletePath' and
-- 'containerPostPath'.
data FailureResponse = FailureResponse Client.Request (Client.Response ByteString) deriving (Show)

instance Exception FailureResponse where

-- | Exception thrown in 'containerGetPath', 'containerDeletePath' and
-- 'containerPostPath'.
newtype DecodeError = DecodeError String deriving (Show)

instance Exception DecodeError where

askBaseUrl :: ClientM BaseUrl
askBaseUrl = asks $ \(ClientEnv _ url) -> url

askManager :: ClientM Client.Manager
askManager = asks $ \(ClientEnv mgr _) -> mgr
