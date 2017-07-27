{-# LANGUAGE DataKinds #-}
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
, container
  -- *** Executing commands
, containerExecImmediate
, containerExecWebsocketInteractive
, containerExecWebsocketNonInteractive

  -- ** Images
, imageIds

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
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> Get '[JSON] (Response Container)
      :<|> ExecAPI 'ExecImmediate
      :<|> ExecAPI 'ExecWebsocketInteractive
      :<|> ExecAPI 'ExecWebsocketNonInteractive
      :<|> "1.0" :> "images" :> Get '[JSON] (Response [ImageId])
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
container                            :: ContainerName -> ClientM (Response Container)
containerExecImmediate               :: ExecClient 'ExecImmediate
containerExecWebsocketInteractive    :: ExecClient 'ExecWebsocketInteractive
containerExecWebsocketNonInteractive :: ExecClient 'ExecWebsocketNonInteractive
imageIds                             :: ClientM (Response [ImageId])
operationIds                         :: ClientM (Response AllOperations)
operation                            :: OperationId -> ClientM (Response Operation)
operationCancel                      :: OperationId -> ClientM (Response Value)
operationWait                        :: OperationId -> ClientM (Response Value)

supportedVersions                        :<|>
    apiConfig                            :<|>
    trustedCertificates                  :<|>
    containerNames                       :<|>
    container                            :<|>
    containerExecImmediate               :<|>
    containerExecWebsocketInteractive    :<|>
    containerExecWebsocketNonInteractive :<|>
    imageIds                             :<|>
    operationIds                         :<|>
    operation                            :<|>
    operationCancel                      :<|>
    operationWait
    = client api

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
