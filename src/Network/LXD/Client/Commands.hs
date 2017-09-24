{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.LXD.Client.Commands (
  -- * Containers
  lxcList
, lxcCreate
, lxcDelete
, lxcInfo

  -- * Exec
, lxcExec
, lxcExecEnv
, lxcExecRaw

  -- * Files and directories
  -- ** Files
, lxcFilePull
, lxcFilePullRaw
, lxcFilePush
, lxcFilePushAttrs
, lxcFilePushRaw
, lxcFilePushRawAttrs
  -- ** Directories
, lxcFileMkdir
, lxcFileMkdirTemplate
, lxcFileMkdirAttrs
  -- ** Recursive
, lxcFilePullRecursive
, lxcFilePushRecursive
, lxcFilePushRecursiveAttrs

  -- * Images
, lxcImageList
, lxcImageAliases
, lxcImageInfo
, lxcImageAlias
, lxcImageCreate
, lxcImageDelete
) where

import Network.LXD.Prelude

import Control.Concurrent.Async (Async, async, withAsync, wait)
import Control.Concurrent.MVar
import Control.Monad ((>=>))
import Control.Monad.Catch (Exception, SomeException, MonadThrow, throwM, catch)

import Data.ByteString.Lazy (ByteString)
import Data.Default (def)
import Data.List (inits)
import Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy as BL

import Numeric (readOct, showOct)

import Servant.Client (ClientM, ClientEnv, runClientM)
import System.Directory (createDirectory, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), splitPath)
import System.Posix.Files (getFileStatus, fileMode, setFileMode)
import qualified System.IO as IO

import Network.LXD.Client

-- | Monad with access to a 'RemoteHost'.
class (MonadIO m, MonadThrow m) => HasClient m where
    -- | Return the LXD remote host to connect to.
    askRemoteHost :: m RemoteHost

    -- | Return the 'ClientEnv' to use when connecting to the LXD host.
    askClientEnv :: m ClientEnv
    askClientEnv = do
        remoteHost <- askRemoteHost
        liftIO (runExceptT $ remoteHostClient remoteHost) >>= \case
            Left err -> throwM $ ClientError err
            Right env -> return env

-- | List all container names.
lxcList :: HasClient m => m [ContainerName]
lxcList = runClient $ containerNames >>= checkResponseOK

-- | Create a new container.
lxcCreate :: HasClient m => ContainerCreateRequest -> m ()
lxcCreate req = runClient $ do
    op <- containerCreate req >>= checkResponseCreated
    _  <- operationWait op >>= checkResponseOK
    return ()

-- | Delete a container.
lxcDelete :: HasClient m => ContainerName -> m ()
lxcDelete n = runClient $ do
    op <- containerDelete n ContainerDeleteRequest >>= checkResponseCreated
    _  <- operationWait op >>= checkResponseOK
    return ()

-- | Get information about a container.
lxcInfo :: HasClient m => ContainerName -> m Container
lxcInfo = runClient . (container >=> checkResponseOK)

-- | Execute a command, catch standard output, print stderr.
lxcExec :: HasClient m
        => ContainerName            -- ^ Container name
        -> String                   -- ^ Command name
        -> [String]                 -- ^ Command arguments
        -> ByteString               -- ^ Standard input
        -> m ByteString
lxcExec name cmd args = lxcExecEnv name cmd args mempty

-- | Execute a command, provide environment variables, catch standard output,
-- print stderr.
lxcExecEnv :: HasClient m
           => ContainerName            -- ^ Container name
           -> String                   -- ^ Command name
           -> [String]                 -- ^ Command arguments
           -> Map String String        -- ^ Environment variables
           -> ByteString               -- ^ Standard input
           -> m ByteString
lxcExecEnv name cmd args env stdin' = do
    stdin  <- liftIO newEmptyMVar
    stdout <- liftIO newEmptyMVar
    stderr <- liftIO newEmptyMVar
    bs     <- liftIO $ newMVar mempty

    let printStderr :: IO ()
        printStderr = takeMVar stderr
                      >>= BL.hPut IO.stdout
                      >>  IO.hFlush IO.stdout
                      >>  printStderr

    let saveStdout :: IO ()
        saveStdout = do bs' <- takeMVar stdout
                        modifyMVar_ bs $ return . (<> bs')

    exec <- lxcExecRaw name cmd args env stdin stdout stderr
    liftIO $
        withAsync printStderr $ \_ ->
        withAsync saveStdout  $ \_ -> do
            unless (BL.null stdin') $ putMVar stdin (Just stdin')
            putMVar stdin Nothing
            wait exec

    liftIO $ takeMVar bs

-- | Execute a command, with given environment variables.
lxcExecRaw :: HasClient m
           => ContainerName            -- ^ Container name
           -> String                   -- ^ Command name
           -> [String]                 -- ^ Command arguments
           -> Map String String        -- ^ Environment variables
           -> MVar (Maybe ByteString)  -- ^ Stream of standard input, pass 'Nothing' to end the stream.
           -> MVar ByteString          -- ^ Standard output
           -> MVar ByteString          -- ^ Standard error
           -> m (Async ())
lxcExecRaw name cmd args env stdin stdout stderr = do
    resp <- runClient $ containerExecWebsocketNonInteractive name req
    md   <- checkResponseCreatedMetadata resp
    let oid     = responseOperation resp
        fds     = execResponseMetadataWebsocketFds md
        stdout' = operationWebSocket oid (fdsAllStdout fds)
        stderr' = operationWebSocket oid (fdsAllStderr fds)
        stdin'  = operationWebSocket oid (fdsAllStdin  fds)

    remoteHost <- askRemoteHost
    let runWS path app = do e <- runExceptT (runWebSockets remoteHost path app)
                            case e of Left err -> throwM $ ClientError err
                                      Right v -> return v

    liftIO . async $
        withAsync (runWS stdout' $ readAllWebSocket (putMVar stdout)) $ \stdoutThread ->
        withAsync (runWS stderr' $ readAllWebSocket (putMVar stderr)) $ \stderrThread ->
        withAsync (runWS stdin'  $ writeAllWebSocket stdin)           $ \stdinThread -> do
            wait stdoutThread
            wait stderrThread
            wait stdinThread
  where
    req = def { execRequestCommand = cmd:args
              , execRequestEnvironment = env }

-- | Pull the file contents from an LXD container.
lxcFilePull :: HasClient m
            => ContainerName  -- ^ Container name
            -> FilePath       -- ^ Source path, in the container
            -> FilePath       -- ^ Destination path, in the host
            -> m ()
lxcFilePull name src dst = do
    path <- runClient $ containerGetPath name src
    case getFile path of
        Directory _ -> throwM $ ClientError "expected a file, but got a directory"
        File bs -> do
            m' <- convFileMode' $ pathMode path
            liftIO $ BL.writeFile dst bs
            liftIO $ setFileMode dst m'

    lxcFilePullRaw name src >>= liftIO . BL.writeFile dst

-- | Pull the file contents from an LXD container, return the lazy bytestring.
lxcFilePullRaw :: HasClient m => ContainerName -> FilePath -> m ByteString
lxcFilePullRaw name src = do
    path <- runClient $ containerGetPath name src
    case getFile path of
        File bs -> return bs
        Directory _ -> throwM $ ClientError "expected a file, but got a directory"

-- | Push the file contents to an LXD container.
lxcFilePush :: HasClient m
            => ContainerName  -- ^ Container name
            -> FilePath       -- ^ Source path, in the host
            -> FilePath       -- ^ Destination path, in the container
            -> m ()
lxcFilePush name src dst = lxcFilePushAttrs name src dst Nothing Nothing

-- | Push the fole contents to an LXD container, with the given attributes.
lxcFilePushAttrs :: HasClient m
                 => ContainerName  -- ^ Container name
                 -> FilePath       -- ^ Source path, in the host
                 -> FilePath       -- ^ Destination path, in the container
                 -> Maybe Uid
                 -> Maybe Gid
                 -> m ()
lxcFilePushAttrs name src dst uid gid = do
    mode <- liftIO $ fileMode <$> getFileStatus src
    bs   <- liftIO $ BL.readFile src
    lxcFilePushRawAttrs name dst
                        uid gid
                        (Just $ convFileMode mode)
                        "file"
                        Nothing
                        bs

-- | Write the lazy bytestring to a file in an LXD container.
lxcFilePushRaw :: HasClient m => ContainerName -> FilePath -> ByteString -> m ()
lxcFilePushRaw name src = lxcFilePushRawAttrs name src n n n "file" n where n = Nothing

-- | Write the lazy bytestring to a file in an LXD container, with given file
-- attributes.
lxcFilePushRawAttrs :: HasClient m
                    => ContainerName
                    -> FilePath
                    -> Maybe Uid
                    -> Maybe Gid
                    -> Maybe FileMode
                    -> FileType
                    -> Maybe WriteMode
                    -> ByteString
                    -> m ()
lxcFilePushRawAttrs name src uid gid fm ft wm bs = void . runClient $
    containerPostPath name src uid gid fm ft wm bs >>= checkResponseOK

-- | Create a directory using a host directory as a template.
--
-- Note that this function doesn't copy the directory contents. Use
-- 'lxcFilePushRecursive' if you want to copy the directory contents as well.
lxcFileMkdirTemplate :: HasClient m
                     => ContainerName  -- ^ Container name
                     -> FilePath       -- ^ Source path, in the host
                     -> FilePath       -- ^ Destination path, in the container
                     -> m ()
lxcFileMkdirTemplate name src dst = do
    mode <- liftIO $ fileMode <$> getFileStatus src
    lxcFileMkdirAttrs name dst False Nothing Nothing (Just $ convFileMode mode)

-- | Create a directory.
lxcFileMkdir :: HasClient m
             => ContainerName
             -> String
             -> Bool          -- ^ Create parent directories
             -> m ()
lxcFileMkdir name dir p = lxcFileMkdirAttrs name dir p Nothing Nothing Nothing

-- | Create a directory, with given attributes.
lxcFileMkdirAttrs :: HasClient m
                  => ContainerName
                  -> String
                  -> Bool           -- ^ Create parent directories
                  -> Maybe Uid
                  -> Maybe Gid
                  -> Maybe FileMode
                  -> m ()
lxcFileMkdirAttrs name dir False uid gid fm = void . runClient $
    containerPostPath name dir uid gid fm "directory" Nothing mempty
lxcFileMkdirAttrs name dir True uid gid fm =
    mapM_ mkdir $ inits (splitPath dir)
  where
    mkdir xs = case concat xs of
        "/" -> return ()
        path -> do
            pathInfo <- runClient $ (Just <$> containerGetPath name path) `catch` ignoreExc
            case getFile <$> pathInfo of
                Just (File _) -> throwM $ ClientError $ "couldn't make dir " ++ dir ++ ": " ++ path ++ " is not a directory"
                Just (Directory _) -> return ()
                Nothing -> lxcFileMkdirAttrs name dir False uid gid fm

    ignoreExc :: Monad m => SomeException -> m (Maybe a)
    ignoreExc _ = return Nothing

-- | Recursively pull a directory (or file) from a container.
lxcFilePullRecursive :: HasClient m
                     => ContainerName  -- ^ Container name
                     -> FilePath       -- ^ Source path, in the container
                     -> FilePath       -- ^ Destination path, in the host
                     -> m ()
lxcFilePullRecursive name src dst = do
    path <- runClient $ containerGetPath name src
    m'   <- convFileMode' $ pathMode path
    case getFile path of
        File bs -> do
            liftIO $ BL.writeFile dst bs
            liftIO $ setFileMode dst m'
        Directory resp -> do
            contents <- checkResponseOK resp
            liftIO $ createDirectory dst
            mapM_ go contents
            liftIO $ setFileMode dst m'
  where
    go file = lxcFilePullRecursive name src' dst'
      where src' = src </> file
            dst' = dst </> file

-- | Recursively push a directory (or file) to a container.
lxcFilePushRecursive :: HasClient m
                     => ContainerName  -- ^ Container name
                     -> FilePath       -- ^ Source path, in the host
                     -> FilePath       -- ^ Destination path, in the container
                     -> m ()
lxcFilePushRecursive name src dst = lxcFilePushRecursiveAttrs name src dst Nothing Nothing

-- | Recursively push a directory (or file) to a container, with given file
-- attributes.
lxcFilePushRecursiveAttrs :: HasClient m
                          => ContainerName
                          -> FilePath       -- ^ Souce path, in the host
                          -> FilePath       -- ^ Destination path, in the container
                          -> Maybe Uid
                          -> Maybe Gid
                          -> m ()
lxcFilePushRecursiveAttrs name src dst uid gid = do
    isDir <- liftIO $ doesDirectoryExist src
    if not isDir
        then lxcFilePushAttrs name src dst uid gid
        else do
            lxcFileMkdirTemplate name src dst
            files <- liftIO $ listDirectory src
            mapM_ go files
  where
    go file = lxcFilePushRecursiveAttrs name src' dst' uid gid
      where src' = src </> file
            dst' = dst </> file

-- | List all image IDs.
lxcImageList :: HasClient m => m [ImageId]
lxcImageList = runClient $ imageIds >>= checkResponseOK

-- | List al image aliases.
lxcImageAliases :: HasClient m => m [ImageAliasName]
lxcImageAliases = runClient $ imageAliases >>= checkResponseOK

-- | Get image information.
lxcImageInfo :: HasClient m => ImageId -> m Image
lxcImageInfo = runClient . image >=> checkResponseOK

-- | Get image alias information.
lxcImageAlias :: HasClient m => ImageAliasName -> m ImageAlias
lxcImageAlias = runClient . imageAlias >=> checkResponseOK

-- | Create an image.
lxcImageCreate :: HasClient m => ImageCreateRequest -> m ()
lxcImageCreate req = runClient $ do
    op <- imageCreate req >>= checkResponseCreated
    _  <- operationWait op >>= checkResponseOK
    return ()

-- | Delete an image.
lxcImageDelete :: HasClient m => ImageId -> m ()
lxcImageDelete img = runClient $ do
    op <- imageDelete img def >>= checkResponseCreated
    _  <- operationWait op >>= checkResponseOK
    return ()

-- | Run a client operation.
runClient :: HasClient m => ClientM a -> m a
runClient action = do
    clientEnv <- askClientEnv
    liftIO (runClientM action clientEnv) >>= \case
        Left err -> throwM $ ClientError (show err)
        Right a -> return a

convFileMode :: (Integral a, Show a) => a -> FileMode
convFileMode m = FileMode . pad $ showOct m ""
  where pad s | n <- length s, n < 4 = replicate (4 - n) '0' ++ s
              | otherwise            = s

convFileMode' :: (MonadThrow m, Eq a, Num a) => FileMode -> m a
convFileMode' (FileMode fm) = case readOct fm of
    [(m, "")] -> return m
    _ -> throwM $ ClientError $ "received invalid file mode: " ++ show fm

-- | Exception raised when the remote host client couldn't be reached.
newtype ClientError = ClientError String

instance Show ClientError where
    show (ClientError err) = "Could not connect to the LXD host: " ++ err

instance Exception ClientError where

-- | Exception raised when the status of a response was unexpected.
data StatusError = StatusError {
    statusErrorExpected :: Int
  , statusErrorReceived :: Int }

instance Show StatusError where
    show StatusError{..} = "Unexpected response with code "
                        ++ show statusErrorReceived ++ ", expected "
                        ++ show statusErrorExpected

instance Exception StatusError where

-- | Check the validity of a synchronous response.
checkResponseOK :: MonadThrow m => Response a -> m a
checkResponseOK Response{..}
    | 200 <- statusCode = return metadata
    | otherwise = throwM $ StatusError 200 statusCode

-- | Check the validity of an asynchronous response.
checkResponseCreated :: MonadThrow m => AsyncResponse a -> m OperationId
checkResponseCreated Response{..}
    | 100 <- statusCode = return responseOperation
    | otherwise = throwM $ StatusError 100 statusCode

-- | Check the validity of an asynchronous response and return the metadata.
checkResponseCreatedMetadata :: MonadThrow m => AsyncResponse a -> m a
checkResponseCreatedMetadata Response{..}
    | 100 <- statusCode = return $ backgroundOperationMetadata metadata
    | otherwise = throwM $ StatusError 100 statusCode
