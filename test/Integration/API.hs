{-# LANGUAGE RecordWildCards #-}
module Integration.API where

import Network.LXD.Prelude
import Testing

import Data.Default (def)

import Servant.Client (runClientM)

import Network.LXD.Client

apiTester :: Tester IO ()
apiTester = do
    testShow "connectToRemote guest"   $ connectToRemote NoClientAuth
    testShow "connectToRemote trusted" $ connectToRemote DefaultClientAuth

connectToRemote :: MonadIO m => ClientAuth -> Test m ApiConfig
connectToRemote auth = do
    client <- remoteHostClient host
    resp <- assertEitherShow =<< (lift . liftIO $ runClientM apiConfig client)
    dat <- assertResponseOK resp
    case auth of NoClientAuth      -> assertEq Untrusted (authStatus dat)
                 DefaultClientAuth -> assertEq Trusted   (authStatus dat)
                 ClientAuthKey _   -> assertEq Untrusted (authStatus dat)
    return dat
  where
    host = def { remoteHostHost = "127.0.0.1"
               , remoteHostClientKey = auth }

assertResponseOK :: Monad m => Response a -> Test m a
assertResponseOK Response{..}
    | 200 <- statusCode = return metadata
    | otherwise = throwError $ "Expected response with code 200 but got " ++ show statusCode ++ " with error code " ++ show errorCode ++ "(" ++ show error ++ ")"
