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
  -- *** Executing commands
, containerExecImmediate
, containerExecWebsocketInteractive
, containerExecWebsocketNonInteractive
  -- *** Working with files
, containerGetPath

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
      :<|> "1.0" :> "containers" :> ReqBody '[JSON] ContainerCreateRequest :> Post '[JSON] (ResponseOp (BackgroundOperation Value))
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> Get '[JSON] (Response Container)
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> ReqBody '[JSON] ContainerDeleteRequest :> Delete '[JSON] (ResponseOp (BackgroundOperation Value))
      :<|> ExecAPI 'ExecImmediate
      :<|> ExecAPI 'ExecWebsocketInteractive
      :<|> ExecAPI 'ExecWebsocketNonInteractive
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> "files" :> QueryParam "path" FilePath :> Get '[JsonOrBinary] (Headers '[Header "X-LXD-Uid" Int, Header "X-LXD-Gid" Int, Header "X-LXD-Mode" String, Header "X-LXD-Type" String] RawFileResponse)
      :<|> "1.0" :> "images" :> Get '[JSON] (Response [ImageId])
      :<|> "1.0" :> "images" :> ReqBody '[JSON] ImageCreateRequest :> Post '[JSON] (ResponseOp (BackgroundOperation Value))
      :<|> "1.0" :> "images" :> "aliases" :> Get '[JSON] (Response [ImageAliasName])
      :<|> "1.0" :> "images" :> "aliases" :> Capture "name" ImageAliasName :> Get '[JSON] (Response ImageAlias)
      :<|> "1.0" :> "images" :> Capture "id" ImageId :> Get '[JSON] (Response Image)
      :<|> "1.0" :> "images" :> Capture "id" ImageId :> ReqBody '[JSON] ImageDeleteRequest :> Delete '[JSON] (ResponseOp (BackgroundOperation Value))
      :<|> "1.0" :> "operations" :> Get '[JSON] (Response AllOperations)
      :<|> "1.0" :> "operations" :> Capture "uuid" OperationId :> Get '[JSON] (Response Operation)
      :<|> "1.0" :> "operations" :> Capture "uuid" OperationId :> Delete '[JSON] (Response Value)
      :<|> "1.0" :> "operations" :> Capture "uuid" OperationId :> "wait" :> Get '[JSON] (Response Value)


api :: Proxy API
api = Proxy

supportedVersions                    :: ClientM (Response [ApiVersion])
apiConfig                            :: ClientM (Response ApiConfig)
trustedCertificates                  :: ClientM (Response [CertificateHash])
containerNames                       :: ClientM (Response [ContainerName])
containerCreate                      :: ContainerCreateRequest -> ClientM (ResponseOp (BackgroundOperation Value))
container                            :: ContainerName -> ClientM (Response Container)
containerDelete                      :: ContainerName -> ContainerDeleteRequest -> ClientM (ResponseOp (BackgroundOperation Value))
containerExecImmediate               :: ExecClient 'ExecImmediate
containerExecWebsocketInteractive    :: ExecClient 'ExecWebsocketInteractive
containerExecWebsocketNonInteractive :: ExecClient 'ExecWebsocketNonInteractive
containerGetPath'                    :: ContainerName -> Maybe FilePath -> ClientM (Headers '[Header "X-LXD-Uid" Int, Header "X-LXD-Gid" Int, Header "X-LXD-Mode" String, Header "X-LXD-Type" String] RawFileResponse)
imageIds                             :: ClientM (Response [ImageId])
imageCreate                          :: ImageCreateRequest -> ClientM (ResponseOp (BackgroundOperation Value))
imageAliases                         :: ClientM (Response [ImageAliasName])
imageAlias                           :: ImageAliasName -> ClientM (Response ImageAlias)
image                                :: ImageId -> ClientM (Response Image)
imageDelete                          :: ImageId -> ImageDeleteRequest -> ClientM (ResponseOp (BackgroundOperation Value))
operationIds                         :: ClientM (Response AllOperations)
operation                            :: OperationId -> ClientM (Response Operation)
operationCancel                      :: OperationId -> ClientM (Response Value)
operationWait                        :: OperationId -> ClientM (Response Value)

supportedVersions                        :<|>
    apiConfig                            :<|>
    trustedCertificates                  :<|>
    containerNames                       :<|>
    containerCreate                      :<|>
    container                            :<|>
    containerDelete                      :<|>
    containerExecImmediate               :<|>
    containerExecWebsocketInteractive    :<|>
    containerExecWebsocketNonInteractive :<|>
    containerGetPath'                    :<|>
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
                Left err -> error' raw $ "Could not decode " ++ fileType ++ ": " ++ err
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

    getType :: Header sym String -> RawFileResponse -> ClientM String
    getType (Header v) _   = return v
    getType _          raw = error' raw "No X-LXD-Type header present"

    error' (RawFileResponse mt bs) m = throwError DecodeFailure {
        decodeError = m
      , responseContentType = mt
      , responseBody = bs
      }

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

type ExecAPI a = "1.0" :> "containers" :> Capture "name" ContainerName :> "exec" :> ReqBody '[JSON] (ExecRequest a) :> Post '[JSON] (ResponseOp (ExecResponse a))

type ExecClient a = ContainerName -> ExecRequest a -> ClientM (ResponseOp (ExecResponse a))
