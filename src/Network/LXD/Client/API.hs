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

api :: Proxy API
api = Proxy

supportedVersions   :: ClientM (Response [ApiVersion])
apiConfig           :: ClientM (Response ApiConfig)
trustedCertificates :: ClientM (Response [CertificateHash])

supportedVersions               :<|>
    apiConfig                   :<|>
    trustedCertificates
    = client api
