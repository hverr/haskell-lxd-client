{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Integration.API where

import Network.LXD.Prelude
import Testing

import Control.Monad.Trans.Maybe

import Data.Default (def)
import qualified Data.Map.Strict as Map

import Servant.Client (ClientEnv, ClientM, runClientM)

import Network.LXD.Client

apiTester :: Tester IO ()
apiTester = do
    testShow "connectToRemote trusted"   $ connectToRemote True
    testShow "connectToRemote untrusted" $ connectToRemote False

    testShow "testSupportedVersions"   testSupportedVersions
    testShow "testTrustedCertificates" testTrustedCertificates

    testShow "testContainerNames" testContainerNames
    testShow "testContainer"      testContainer

    testShow "testContainerExecImmediate" testContainerExecImmediate

    testShow "testOperationIds"    testOperationIds
    testShow "testOperation"       testOperation


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

testContainerExecImmediate :: MonadIO m => Test m (ExecResponse 'ExecImmediate)
testContainerExecImmediate =
    runTrusted (containerExecImmediate "test" req) >>= assertResponseCreated
  where
    req = def { execRequestCommand = ["/bin/echo", "Hello, World!"] }

testOperationIds :: MonadIO m => Test m AllOperations
testOperationIds =
    runTrusted operationIds >>= assertResponseOK

testOperation :: MonadIO m => Test m (Maybe Operation)
testOperation = runMaybeT $
    lift (runTrusted operationIds)
    >>= lift . assertResponseOK
    >>= head'
    >>= lift . runTrusted . operation
    >>= lift . assertResponseOK
 where
   head' (AllOperations m) =
     case fst <$> Map.minView m of
       Nothing    -> MaybeT (return Nothing)
       Just []    -> MaybeT (return Nothing)
       Just (x:_) -> return x

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

assertResponseOK :: Monad m => GenericResponse op a -> Test m a
assertResponseOK Response{..}
    | 200 <- statusCode = return metadata
    | otherwise = throwError $ "Expected response with code 200 but got " ++ show statusCode ++ " with error code " ++ show errorCode ++ " (" ++ show error ++ ")"

assertResponseCreated :: Monad m => GenericResponse op a -> Test m a
assertResponseCreated Response{..}
    | 100 <- statusCode = return metadata
    | otherwise = throwError $ "Exepected response with code 100 but got " ++ show statusCode

assertResponseAccepted :: Monad m => GenericResponse op a -> Test m a
assertResponseAccepted Response{..}
    | 202 <- statusCode = return metadata
    | otherwise = throwError $ "Exepected response with code 202 but got " ++ show statusCode
