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

  -- ** Operations
, operationIds
, operation
, operationCancel
, operationWait

  -- * Helpers
, ExecClient
) where

import Data.Aeson (Value)
import Data.Proxy

import Servant.API
import Servant.Client

import Network.LXD.Client.Types

type API = Get '[JSON] (Response [ApiVersion])
      :<|> "1.0" :> Get '[JSON] (Response ApiConfig)
      :<|> "1.0" :> "certificates" :> Get '[JSON] (Response [CertificateHash])
      :<|> "1.0" :> "containers" :> Get '[JSON] (Response [ContainerName])
      :<|> "1.0" :> "containers" :> Capture "name" ContainerName :> Get '[JSON] (Response Container)
      :<|> ExecAPI 'ExecImmediate
      :<|> ExecAPI 'ExecWebsocketInteractive
      :<|> ExecAPI 'ExecWebsocketNonInteractive
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
    operationIds                         :<|>
    operation                            :<|>
    operationCancel                      :<|>
    operationWait
    = client api

type ExecAPI a = "1.0" :> "containers" :> Capture "name" ContainerName :> "exec" :> ReqBody '[JSON] (ExecRequest a) :> Post '[JSON] (ResponseOp (ExecResponse a))

type ExecClient a = ContainerName -> ExecRequest a -> ClientM (ResponseOp (ExecResponse a))
