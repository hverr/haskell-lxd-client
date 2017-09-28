{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Network.LXD.Client.API (
  -- * API
  -- ** Information
  supportedVersions
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

import Network.LXD.Prelude

import Control.Concurrent (MVar, takeMVar)
import Control.Exception (catch, throwIO)

import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import qualified Data.ByteString.Lazy as BL

import Servant.API
import Servant.Client

import Network.LXD.Client.Types
import qualified Network.WebSockets as WS

type API = Get '[JSON] (Response [ApiVersion])
      :<|> "1.0" :> Get '[JSON] (Response ApiConfig)
      :<|> "1.0" :> "certificates" :> Get '[JSON] (Response [CertificateHash])
      :<|> "1.0" :> "containers" :> Get '[JSON] (Response [ContainerName])
      :<|> "1.0" :> "containers" :> ReqBody '[JSON] ContainerCreateRequest :> Post '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> Get '[JSON] (Response Container)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerDeleteRequest :> Delete '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerPut :> Put '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerPatch :> Patch '[JSON] (Response Value)
      :<|> ExecAPI 'ExecImmediate
      :<|> ExecAPI 'ExecWebsocketInteractive
      :<|> ExecAPI 'ExecWebsocketNonInteractive
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> "files" :> QueryParam "path" FilePath :> Get '[JsonOrBinary] (Headers '[Header "X-LXD-Uid" Uid, Header "X-LXD-Gid" Gid, Header "X-LXD-Mode" FileMode, Header "X-LXD-Type" FileType] RawFileResponse)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> "files" :> QueryParam "path" FilePath :> Header "X-LXD-Uid" Uid :> Header "X-LXD-Gid" Gid :> Header "X-LXD-Mode" FileMode :> Header "X-LXD-Type" FileType :> Header "X-LXD-Write" String :> ReqBody '[OctetStream] ByteString :> Post '[JSON] (Response Value)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> "files" :> QueryParam "path" FilePath :> Delete '[JSON] (Response Value)
      :<|> "1.0" :> "images" :> Get '[JSON] (Response [ImageId])
      :<|> "1.0" :> "images" :> ReqBody '[JSON] ImageCreateRequest :> Post '[JSON] (AsyncResponse Value)
      :<|> "1.0" :> "images" :> "aliases" :> Get '[JSON] (Response [ImageAliasName])
      :<|> "1.0" :> "images" :> "aliases" :> Capture "name" ImageAliasName :> Get '[JSON] (Response ImageAlias)
      :<|> "1.0" :> "images" :> Capture "id" ImageId :> Get '[JSON] (Response Image)
      :<|> "1.0" :> "images" :> Capture "id" ImageId :> ReqBody '[JSON] ImageDeleteRequest :> Delete '[JSON] (AsyncResponse Value)
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
containerExecImmediate               :: ExecClient 'ExecImmediate
containerExecWebsocketInteractive    :: ExecClient 'ExecWebsocketInteractive
containerExecWebsocketNonInteractive :: ExecClient 'ExecWebsocketNonInteractive
containerGetPath'                    :: ContainerName -> Maybe FilePath -> ClientM (Headers '[Header "X-LXD-Uid" Uid, Header "X-LXD-Gid" Gid, Header "X-LXD-Mode" FileMode, Header "X-LXD-Type" FileType] RawFileResponse)
containerPostPath'                   :: ContainerName -> Maybe FilePath -> Maybe Uid -> Maybe Gid -> Maybe FileMode -> Maybe FileType -> Maybe String -> ByteString -> ClientM (Response Value)
containerDeletePath'                 :: ContainerName -> Maybe FilePath -> ClientM (Response Value)
imageIds                             :: ClientM (Response [ImageId])
imageCreate                          :: ImageCreateRequest -> ClientM (AsyncResponse Value)
imageAliases                         :: ClientM (Response [ImageAliasName])
imageAlias                           :: ImageAliasName -> ClientM (Response ImageAlias)
image                                :: ImageId -> ClientM (Response Image)
imageDelete                          :: ImageId -> ImageDeleteRequest -> ClientM (AsyncResponse Value)
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
    containerExecImmediate               :<|>
    containerExecWebsocketInteractive    :<|>
    containerExecWebsocketNonInteractive :<|>
    containerGetPath'                    :<|>
    containerPostPath'                   :<|>
    containerDeletePath'                 :<|>
    imageIds                             :<|>
    imageCreate                          :<|>
    imageAliases                         :<|>
    imageAlias                           :<|>
    image                                :<|>
    imageDelete                          :<|>
    operationIds                         :<|>
    operation                            :<|>
    operationCancel                      :<|>
    operationWait
    = client api

containerGetPath :: ContainerName -> FilePath -> ClientM PathResponse
containerGetPath name fp = do
    headers <- containerGetPath' name (Just fp)
    let raw = getResponse headers
    case getHeadersHList headers of
        hUid `HCons` (hGid `HCons` (hMode `HCons` (hType `HCons` HNil))) -> do
            fileType <- getType hType raw
            case fileResponse fileType (rawFileResponseBody raw) of
                Left err -> error' raw $ "Could not decode " ++ show fileType ++ ": " ++ err
                Right file -> return PathResponse {
                                  pathUid = getValueDef hUid 0
                                , pathGid = getValueDef hGid 0
                                , pathMode = getValueDef hMode "0700"
                                , pathType = fileType
                                , getFile = file
                                }
 where
    getValueDef :: Header sym a -> a -> a
    getValueDef (Header v) _   = v
    getValueDef _          def = def

    getType :: Header sym FileType -> RawFileResponse -> ClientM FileType
    getType (Header v) _   = return v
    getType _          raw = error' raw "No X-LXD-Type header present"

    error' (RawFileResponse mt bs) m = throwError DecodeFailure {
        decodeError = m
      , responseContentType = mt
      , responseBody = bs
      }

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
containerPostPath name fp uid gid perm ftype mode =
    containerPostPath' name (Just fp) uid gid perm (Just ftype) (mode' <$> mode)
  where
    mode' ModeOverwrite = "overwrite"
    mode' ModeAppend    = "append"

containerDeletePath :: ContainerName -> FilePath -> ClientM (Response Value)
containerDeletePath name fp = containerDeletePath' name (Just fp)

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
