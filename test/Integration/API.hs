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

    testShow "testSupportedVersions"   testSupportedVersions
    testShow "testTrustedCertificates" testTrustedCertificates
    testShow "testContainerNames"      testContainerNames
    testShow "testContainer"           testContainer

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

testSupportedVersions :: MonadIO m => Test m [ApiVersion]
testSupportedVersions = do
    resp <- runTrusted supportedVersions
    dat <- assertResponseOK resp
    assertEq dat [ApiVersion "/1.0"]
    return dat

testTrustedCertificates :: MonadIO m => Test m [CertificateHash]
testTrustedCertificates = do
    resp <- runTrusted trustedCertificates
    certs <- assertResponseOK resp
    assertNotEq certs []
    return certs

testContainerNames :: MonadIO m => Test m [ContainerName]
testContainerNames = do
    resp <- runTrusted containerNames
    names <- assertResponseOK resp
    assertNotEq names []
    return names

testContainer :: MonadIO m => Test m Container
testContainer =
    runTrusted (container "test") >>= assertResponseOK

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

runTrusted :: MonadIO m => ClientM a -> Test m a
runTrusted action = do
    client <- trustedClient
    liftIO (runClientM action client) >>= assertEitherShow

assertResponseOK :: Monad m => Response a -> Test m a
assertResponseOK Response{..}
    | 200 <- statusCode = return metadata
    | otherwise = throwError $ "Expected response with code 200 but got " ++ show statusCode ++ " with error code " ++ show errorCode ++ "(" ++ show error ++ ")"
