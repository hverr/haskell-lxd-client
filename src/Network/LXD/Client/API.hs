{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Network.LXD.Client.API where

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

supportedVersions                        :<|>
    apiConfig                            :<|>
    trustedCertificates                  :<|>
    containerNames                       :<|>
    container                            :<|>
    containerExecImmediate               :<|>
    containerExecWebsocketInteractive    :<|>
    containerExecWebsocketNonInteractive
    = client api

type ExecAPI a = "1.0" :> "containers" :> Capture "name" ContainerName :> "exec" :> ReqBody '[JSON] (ExecRequest a) :> Post '[JSON] (Response (ExecResponse a))

type ExecClient a = ContainerName -> ExecRequest a -> ClientM (Response (ExecResponse a))
