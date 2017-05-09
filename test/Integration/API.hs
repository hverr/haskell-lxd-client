{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Integration.API where

import Network.LXD.Prelude
import Testing

import Data.Default (def)

import Servant.Client (ClientEnv, ClientM, runClientM)

import Network.LXD.Client

apiTester :: Tester IO ()
apiTester = do
    testShow "connectToRemote trusted"   $ connectToRemote True
    testShow "connectToRemote untrusted" $ connectToRemote False

connectToRemote :: MonadIO m => Bool -> Test m ApiConfig
connectToRemote trusted = do
    resp <- runClientM' apiConfig =<< client
    dat <- assertResponseOK resp
    if trusted then assertEq Trusted   (authStatus dat)
               else assertEq Untrusted (authStatus dat)
    return dat
  where
    client | True  <- trusted = trustedClient
           | False <- trusted = untrustedClient

trustedClient :: (MonadError String m, MonadIO m) => m ClientEnv
trustedClient = remoteHostClient host
  where host = def { remoteHostHost = "127.0.0.1"
                   , remoteHostClientKey = DefaultClientAuth }

untrustedClient :: (MonadError String m, MonadIO m) => m ClientEnv
untrustedClient = remoteHostClient host
  where host = def { remoteHostHost = "127.0.0.1"
                   , remoteHostClientKey = NoClientAuth }

runClientM' :: MonadIO m => ClientM a -> ClientEnv -> Test m a
runClientM' action client = liftIO (runClientM action client) >>= assertEitherShow

assertResponseOK :: Monad m => Response a -> Test m a
assertResponseOK Response{..}
    | 200 <- statusCode = return metadata
    | otherwise = throwError $ "Expected response with code 200 but got " ++ show statusCode ++ " with error code " ++ show errorCode ++ "(" ++ show error ++ ")"
