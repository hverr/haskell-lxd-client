{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Network.LXD.Client.API where

import Data.Proxy

import Servant.API
import Servant.Client

import Network.LXD.Client.Types

type API = Get '[JSON] (Response [ApiVersion])
      :<|> "1.0" :> Get '[JSON] (Response ApiConfig)

api :: Proxy API
api = Proxy

supportedVersions :: ClientM (Response [ApiVersion])
apiConfig :: ClientM (Response ApiConfig)
supportedVersions :<|> apiConfig = client api
