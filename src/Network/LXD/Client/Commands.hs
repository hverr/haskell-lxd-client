{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.LXD.Client.Commands (
  -- * Re-exports
  def
, module Network.LXD.Client.Remotes
, module Network.LXD.Client.Types

  -- * Running commands
, HasClient(..)
, defaultClientEnv
, WithLocalHost, runWithLocalHost
, WithRemoteHost, runWithRemoteHost

  -- * Containers
, ContainerName(..)
, lxcList
, lxcCreate
, lxcDelete
, lxcInfo
, lxcStart
, lxcStop
, lxcRestart
, lxcFreeze
, lxcUnfreeze

  -- * Exec
, lxcExec
, lxcExecEnv
, lxcExecRaw

  -- * Files and directories
  -- ** Deletion
, lxcFileDelete
  -- ** Files
, lxcFilePull
, lxcFilePullRaw
, lxcFilePush
, lxcFilePushAttrs
, lxcFilePushRaw
, lxcFilePushRawAttrs
  -- ** Directories
, lxcFileListDir
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

  -- * Networks
, lxcNetworkList
, lxcNetworkCreate
, lxcNetworkInfo
, lxcNetworkConfig
, lxcNetworkDelete

  -- * Profiles
, lxcProfileList
, lxcProfileCreate
, lxcProfileInfo
, lxcProfileConfig
, lxcProfileDelete

  -- * Storage
, lxcStorageList
, lxcStorageCreate
, lxcStorageInfo
, lxcStorageConfig
, lxcStorageDelete

  -- * Volume
, lxcVolumeList
, lxcVolumeCreate
, lxcVolumeInfo
, lxcVolumeConfig
, lxcVolumeDelete
) where

import Network.LXD.Prelude

import Control.Concurrent.Async (Async, async, withAsync, wait, uninterruptibleCancel)
import Control.Concurrent.MVar
import Control.Monad ((>=>))
import Control.Monad.Catch (Exception, SomeException, MonadThrow, throwM, MonadCatch, catch, MonadMask, bracket)
import Control.Monad.State (StateT, evalStateT, gets, modify')

import Data.ByteString.Lazy (ByteString)
import Data.Default (def)
import Data.List (inits)
import Data.Map.Strict (Map)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL

import Numeric (readOct, showOct)
import qualified Network.WebSockets as WS

import Servant.Client (ClientM, ClientEnv, runClientM)
import System.Directory (createDirectory, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), splitPath)
import System.Posix.Files (getFileStatus, fileMode, setFileMode)
import qualified System.IO as IO

import Network.LXD.Client
import Network.LXD.Client.Types
import Network.LXD.Client.Events
import Network.LXD.Client.Remotes

-- | A host that can be connected to.
data Host = HLocalHost LocalHost
          | HRemoteHost RemoteHost

-- | Monad with access to a 'ClientEnv'.
class (MonadIO m, MonadMask m) => HasClient m where
    -- | Return the LXD remote host to connect to.
    askHost :: m Host

    -- | Return the 'ClientEnv' to use when connecting to the LXD host.
    --
    -- Returns 'defaultClientEnv' by default.
    askClientEnv :: m ClientEnv
    askClientEnv = defaultClientEnv

-- | Create a default 'ClientEnv'.
defaultClientEnv :: HasClient m => m ClientEnv
defaultClientEnv = do
    host <- askHost
    liftIO (runExceptT $ client host) >>= \case
        Left err -> throwM $ ClientError err
        Right env -> return env
 where
   client (HLocalHost host) = localHostClient host
   client (HRemoteHost host) = remoteHostClient host

-- | Run a web sockets application using.
runWebSockets :: Host -> String -> WS.ClientApp a -> IO a
runWebSockets host' path app =
    runExceptT runWS >>= \case
        Left err -> throwM $ ClientError err
        Right env -> return env
  where
    runWS | HLocalHost host <- host' = runWebSocketsLocal host path app
          | HRemoteHost host <- host' = runWebSocketsRemote host path app

-- | Monad with access to a local host.
newtype WithLocalHost a = WithLocalHost (StateT (LocalHost, Maybe ClientEnv) IO a)
                        deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask)

instance HasClient WithLocalHost where
    askHost = WithLocalHost . gets $ HLocalHost . fst
    askClientEnv = do
        env <- WithLocalHost . gets $ snd
        case env of
            Just env' -> return env'
            Nothing -> do
                env' <- defaultClientEnv
                WithLocalHost . modify' $ \(x, _) -> (x, Just env')
                return env'

-- | Run a 'WithLocalHost' monad
runWithLocalHost :: LocalHost -> WithLocalHost a -> IO a
runWithLocalHost host (WithLocalHost m) = evalStateT m (host, Nothing)

-- | Monad with access to a remote host.
newtype WithRemoteHost a = WithRemoteHost (StateT (RemoteHost, Maybe ClientEnv) IO a)
                        deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask)

instance HasClient WithRemoteHost where
    askHost = WithRemoteHost . gets $ HRemoteHost . fst
    askClientEnv = do
        env <- WithRemoteHost . gets $ snd
        case env of
            Just env' -> return env'
            Nothing -> do
                env' <- defaultClientEnv
                WithRemoteHost . modify' $ \(x, _) -> (x, Just env')
                return env'

-- | Run a 'WithRemoteHost' monad
runWithRemoteHost :: RemoteHost -> WithRemoteHost a -> IO a
runWithRemoteHost host (WithRemoteHost m) = evalStateT m (host, Nothing)

-- | List all container names.
lxcList :: HasClient m => m [ContainerName]
lxcList = runClient $ containerNames >>= checkResponseOK

-- | Create a new container.
lxcCreate :: HasClient m => ContainerCreateRequest -> m ()
lxcCreate req = runAndWait $ containerCreate req >>= checkResponseCreated

-- | Delete a container.
lxcDelete :: HasClient m => ContainerName -> m ()
lxcDelete n = runAndWait $ containerDelete n ContainerDeleteRequest
                           >>= checkResponseCreated

-- | Get information about a container.
lxcInfo :: HasClient m => ContainerName -> m Container
lxcInfo = runClient . (container >=> checkResponseOK)

-- | Start a contianer.
lxcStart :: HasClient m => ContainerName -> m ()
lxcStart name = runAndWait $ containerPutState name s >>= checkResponseCreated
  where s = containerNewState Start False

-- | Stop a container.
--
-- The second flag forces the action.
lxcStop :: HasClient m => ContainerName -> Bool -> m ()
lxcStop name force = runAndWait $ containerPutState name s >>= checkResponseCreated
  where s = containerNewState Stop force

-- | Restart a container.
--
-- The second flag forces the action.
lxcRestart :: HasClient m => ContainerName -> Bool -> m ()
lxcRestart name force = runAndWait $ containerPutState name s >>= checkResponseCreated
  where s = containerNewState Restart force

-- | Freeze a container.
lxcFreeze :: HasClient m => ContainerName -> m ()
lxcFreeze name = runAndWait $ containerPutState name s >>= checkResponseCreated
  where s = containerNewState Freeze False

-- | Unfreeze a container.
lxcUnfreeze :: HasClient m => ContainerName -> m ()
lxcUnfreeze name = runAndWait $ containerPutState name s >>= checkResponseCreated
  where s = containerNewState Unfreeze False

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

    host <- askHost
    let runWS = runWebSockets host

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

-- | Delete a file or empty directory from an LXD container.
lxcFileDelete :: HasClient m => ContainerName -> FilePath -> m ()
lxcFileDelete name fp = void . runClient $ containerDeletePath name fp

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

-- | List all entries in a directory, without @.@ or @..@.
lxcFileListDir :: HasClient m => ContainerName -> FilePath -> m [String]
lxcFileListDir name fp = do
    path <- runClient $ containerGetPath name fp
    case getFile path of
        File _ -> throwM $ ClientError "expected a directory, but got a file"
        Directory r -> checkResponseOK r

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
lxcImageCreate req = runAndWait $ imageCreate req >>= checkResponseCreated

-- | Delete an image.
lxcImageDelete :: HasClient m => ImageId -> m ()
lxcImageDelete img = runAndWait $ imageDelete img def >>= checkResponseCreated

-- | List all networks
lxcNetworkList :: HasClient m => m [NetworkName]
lxcNetworkList = runClient $ networkList >>= checkResponseOK

-- | Create a network.
lxcNetworkCreate :: HasClient m => NetworkCreateRequest -> m ()
lxcNetworkCreate n = void . runClient $ networkCreate n >>= checkResponseOK

-- | Get network information.
lxcNetworkInfo :: HasClient m => NetworkName -> m Network
lxcNetworkInfo n = runClient $ network n >>= checkResponseOK

-- | Configure a network.
lxcNetworkConfig :: HasClient m => NetworkName -> NetworkConfigRequest -> m ()
lxcNetworkConfig n c = void . runClient $ networkPatch n c >>= checkResponseOK

-- | Delete a network
lxcNetworkDelete :: HasClient m => NetworkName -> m ()
lxcNetworkDelete n = void . runClient $ networkDelete n >>= checkResponseOK

-- | List all profiles
lxcProfileList :: HasClient m => m [ProfileName]
lxcProfileList = runClient $ profileList >>= checkResponseOK

-- | Create a profile.
lxcProfileCreate :: HasClient m => ProfileCreateRequest -> m ()
lxcProfileCreate n = void . runClient $ profileCreate n >>= checkResponseOK

-- | Get profile information.
lxcProfileInfo :: HasClient m => ProfileName -> m Profile
lxcProfileInfo n = runClient $ profile n >>= checkResponseOK

-- | Configure a profile.
lxcProfileConfig :: HasClient m => ProfileName -> ProfileConfigRequest -> m ()
lxcProfileConfig n c = void . runClient $ profilePatch n c >>= checkResponseOK

-- | Delete a profile
lxcProfileDelete :: HasClient m => ProfileName -> m ()
lxcProfileDelete n = void . runClient $ profileDelete n >>= checkResponseOK

-- | List all storage pools
lxcStorageList :: HasClient m => m [PoolName]
lxcStorageList = runClient $ poolList >>= checkResponseOK

-- | Create a storage pool.
lxcStorageCreate :: HasClient m => PoolCreateRequest -> m ()
lxcStorageCreate n = void . runClient $ poolCreate n >>= checkResponseOK

-- | Get storage pool information.
lxcStorageInfo :: HasClient m => PoolName -> m Pool
lxcStorageInfo n = runClient $ pool n >>= checkResponseOK

-- | Configure a storage pool.
lxcStorageConfig :: HasClient m => PoolName -> PoolConfigRequest -> m ()
lxcStorageConfig n c = void . runClient $ poolPatch n c >>= checkResponseOK

-- | Delete a storage pool
lxcStorageDelete :: HasClient m => PoolName -> m ()
lxcStorageDelete n = void . runClient $ poolDelete n >>= checkResponseOK

-- | List all volumes
lxcVolumeList :: HasClient m => PoolName -> m [VolumeName]
lxcVolumeList p = runClient $ volumeList p >>= checkResponseOK

-- | Create a volume.
lxcVolumeCreate :: HasClient m => PoolName -> VolumeCreateRequest -> m ()
lxcVolumeCreate p r = void . runClient $ volumeCreate p r >>= checkResponseOK

-- | Get volume information.
lxcVolumeInfo :: HasClient m => PoolName -> VolumeName -> m Volume
lxcVolumeInfo p n = runClient $ volume p n >>= checkResponseOK

-- | Configure a volume.
lxcVolumeConfig :: HasClient m => PoolName -> VolumeName -> VolumeConfigRequest -> m ()
lxcVolumeConfig p n c = void . runClient $ volumePatch p n c >>= checkResponseOK

-- | Delete a volume
lxcVolumeDelete :: HasClient m => PoolName -> VolumeName -> m ()
lxcVolumeDelete p n = void . runClient $ volumeDelete p n >>= checkResponseOK

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
    statusErrorExpected :: StatusCode
  , statusErrorReceived :: StatusCode }

instance Show StatusError where
    show StatusError{..} = "Unexpected response with code "
                        ++ show statusErrorReceived ++ ", expected "
                        ++ show statusErrorExpected

instance Exception StatusError where

-- | Check the validity of a synchronous response.
checkResponseOK :: MonadThrow m => Response a -> m a
checkResponseOK Response{..}
    | SSuccess <- statusCode = return metadata
    | otherwise = throwM $ StatusError SSuccess statusCode

-- | Check the validity of an asynchronous response.
checkResponseCreated :: MonadThrow m => AsyncResponse a -> m OperationId
checkResponseCreated Response{..}
    | SCreated <- statusCode = return responseOperation
    | otherwise = throwM $ StatusError SCreated statusCode

-- | Check the validity of an asynchronous response and return the metadata.
checkResponseCreatedMetadata :: MonadThrow m => AsyncResponse a -> m a
checkResponseCreatedMetadata Response{..}
    | SCreated <- statusCode = return $ backgroundOperationMetadata metadata
    | otherwise = throwM $ StatusError SCreated statusCode

-- | Wait for an operation, and check whether it was successfull
runAndWait :: HasClient m => ClientM OperationId -> m ()
runAndWait op = do
    oid  <- liftIO newEmptyMVar
    ops  <- liftIO newEmptyMVar
    host <- askHost

    let err = throwM . OperationError
    let waitForDone = do op' <- liftIO $ takeMVar ops
                         case operationStatusCode op' of
                             SSuccess   -> return ()
                             SStopped   -> err "Operation unexpectedly stopped"
                             SCancelled -> err "Opeartion unexpectedly cancelled"
                             SFailure   -> err $ "Operation failed: " ++ operationErr op'
                             SRunning   -> printProgress op' >> waitForDone
                             _           -> waitForDone

    bracket
        (liftIO . async . runWebSockets host operationsPath $ listenForOperation oid ops)
        (liftIO . uninterruptibleCancel) $ \_ -> do
            oid' <- runClient op
            liftIO $ putMVar oid oid'
            waitForDone
  where
    printProgress op' = case Aeson.fromJSON (operationMetadata op') of
        Aeson.Error _ -> return ()
        Aeson.Success (OperationProgress p) -> liftIO $ do
            IO.hPutStr IO.stderr $ "Progres: " ++ p ++ "\r"
            IO.hFlush IO.stderr

-- | Exception raised when the operation was unsuccessful.
newtype OperationError = OperationError String

instance Show OperationError where
    show (OperationError err) = "Operation unsuccessful: " ++ err

instance Exception OperationError where
