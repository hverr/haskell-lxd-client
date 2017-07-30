{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Integration.API where

import Network.LXD.Prelude
import Testing

import Control.Concurrent (newEmptyMVar, newMVar, putMVar, takeMVar, modifyMVar_)
import Control.Concurrent.Async (async, wait)
import Control.Monad ((>=>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Except (runExceptT)

import Data.Aeson (object)
import Data.Default (def)
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

import Servant.Client (ClientEnv, ClientM, runClientM)

import Network.LXD.Client

apiTester :: Tester IO ()
apiTester = do
    testShow "connectToRemote trusted"   $ connectToRemote True
    testShow "connectToRemote untrusted" $ connectToRemote False

    testShow "testSupportedVersions"   testSupportedVersions
    testShow "testTrustedCertificates" testTrustedCertificates

    testShow "testContainerNames"        testContainerNames
    testShow "testContainer"             testContainer
    testShow "testContainerCreateWaitDelete" testContainerCreateWaitDelete

    testShow "testContainerExecImmediate" testContainerExecImmediate

    testShow "testImageIds"                   testImageIds
    testShow "testImageLocalCreateWaitDelete" testImageLocalCreateWaitDelete
    testShow "testImageAliases"               testImageAliases
    testShow "testImageAlias"                 testImageAlias
    testShow "testImage"                      testImage

    testShow "testOperationIds"    testOperationIds
    testShow "testOperation"       testOperation

    testShow "testExecHelloWorld" testExecHelloWorld


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

testContainerCreateWaitDelete :: MonadIO m => Test m ()
testContainerCreateWaitDelete = do
    resp <- runTrusted (containerCreate req)
    _    <- assertResponseCreated resp
    _    <- runTrusted (operationWait $ responseOperation resp) >>= assertResponseOK
    del  <- runTrusted (containerDelete "create-test" def)
    _    <- assertResponseCreated del
    _    <- runTrusted (operationWait $ responseOperation del) >>= assertResponseOK
    return ()
  where
    req = ContainerCreateRequest {
        containerCreateRequestName = "create-test"
      , containerCreateRequestArchitecture = "x86_64"
      , containerCreateRequestEphemeral = True
      , containerCreateRequestProfiles = ["default"]
      , containerCreateRequestConfig = object []
      , containerCreateRequestDevices = object []
      , containerCreateRequestInstanceType = Nothing
      , containerCreateRequestSource = ContainerSourceLocalByAlias "test-image"
      }


testContainerExecImmediate :: MonadIO m => Test m (ExecResponse 'ExecImmediate)
testContainerExecImmediate =
    runTrusted (containerExecImmediate "test" req) >>= assertResponseCreated
  where
    req = def { execRequestCommand = ["/bin/echo", "Hello, World!"] }

testImageIds :: MonadIO m => Test m [ImageId]
testImageIds = do
    ids <- runTrusted imageIds >>= assertResponseOK
    assertNotEq ids []
    return ids

testImageLocalCreateWaitDelete :: MonadIO m => Test m ()
testImageLocalCreateWaitDelete = do
    op1 <- runTrusted (containerCreate contReq)
    _   <- assertResponseCreated op1
    _   <- runTrusted (operationWait $ responseOperation op1)
    op2 <- runTrusted (imageCreate imgReq)
    _   <- assertResponseCreated op2
    _   <- runTrusted (operationWait $ responseOperation op2)
    img <- runTrusted (imageAlias "create-image-test") >>= assertResponseOK
    op3 <- runTrusted (imageDelete (ImageId . fromJust $ imageAliasTarget img) def)
    _   <- assertResponseCreated op3
    _   <- runTrusted (operationWait $ responseOperation op3)
    op4 <- runTrusted (containerDelete "create-image-container" def)
    _   <- assertResponseCreated op4
    _   <- runTrusted (operationWait $ responseOperation op4)
    return ()
  where
    contReq = containerCreateRequest "create-image-container" $ ContainerSourceLocalByAlias "test-image"
    imgReq = (imageCreateRequest $ ImageSourceLocalContainer "create-image-container") {
        imageCreateRequestAliases = [defaultImageAlias "create-image-test"]
      }

testImageAliases :: MonadIO m => Test m [ImageAliasName]
testImageAliases = do
    v <- runTrusted imageAliases >>= assertResponseOK
    assertNotEq v []
    return v

testImageAlias :: MonadIO m => Test m [ImageAlias]
testImageAlias = do
    v       <- runTrusted imageAliases >>= assertResponseOK
    aliases <- mapM (runTrusted . imageAlias >=> assertResponseOK) v
    assertEq (length v) (length aliases)
    return aliases

testImage :: MonadIO m => Test m [Image]
testImage = do
    ids    <- runTrusted imageIds >>= assertResponseOK
    images <- mapM (runTrusted . image >=> assertResponseOK) ids
    assertEq (length ids) (length images)
    return images

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

testExecHelloWorld :: MonadIO m => Test m String
testExecHelloWorld = do
    resp <- runTrusted (containerExecWebsocketNonInteractive "test" req)
    md   <- assertResponseCreated resp
    let oid = responseOperation resp
    let fds = execResponseMetadataWebsocketFds (execResponseMetadata md)
    let stdout = operationWebSocket oid (fdsAllStdout fds)
    let stderr = operationWebSocket oid (fdsAllStderr fds)
    let stdin  = operationWebSocket oid (fdsAllStdin fds)

    input <- liftIO newEmptyMVar
    output <- liftIO $ newMVar BL.empty

    stdoutThread <- async' $ runWebSockets trustedHost stdout (readAllWebSocket (save' output))
    stderrThread <- async' $ runWebSockets trustedHost stderr (readAllWebSocket BL.putStr)
    stdinThread  <- async' $ runWebSockets trustedHost stdin  (writeAllWebSocket input)

    wait' stdoutThread >>= assertEither
    wait' stderrThread >>= assertEither

    liftIO $ putMVar input Nothing
    wait' stdinThread  >>= assertEither

    output' <- liftIO $ BL.toStrict <$> takeMVar output
    assertTrue ("Hello World" `BS.isInfixOf` output') "\"Hello World\" not present in output"
    return $ show output'
  where
    req = def { execRequestCommand = ["/bin/echo", "Hello World"] }

    save' output bs = do
        BL.putStr bs
        modifyMVar_ output $ \bs' -> return (bs' `BL.append` bs)

    async' = liftIO . async . runExceptT
    wait' = liftIO . wait


trustedClient :: (MonadError String m, MonadIO m) => m ClientEnv
trustedClient = remoteHostClient trustedHost

trustedHost :: RemoteHost
trustedHost = def { remoteHostHost = "127.0.0.1"
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
