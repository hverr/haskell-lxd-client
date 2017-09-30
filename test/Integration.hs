module Main where

import Network.LXD.Client.Internal.Prelude hiding (log)

import Control.Monad.Catch (bracket, bracket_)
import Control.Concurrent (threadDelay)

import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import System.Environment (getExecutablePath, lookupEnv)
import System.Random (randomIO)

import Test.Hspec
import Turtle (sh, procs, empty)

import Network.LXD.Client.Commands hiding (containerName)

main :: IO ()
main = do
    local <- getLocal
    if local
        then runTestSuite
        else bracket createLxd destroyLxd $ \containerName -> do
            initializeLxd containerName
            runTestSuiteLxd containerName
  where
    getLocal = do
        env <- lookupEnv "LOCAL"
        return $ env `elem` map Just ["yes", "y", "1", "true"]

--------------------------------------------------------------------------------

runTestSuite :: IO ()
runTestSuite = hspec testSuite

testSuite :: Spec
testSuite = describe "containers" $ do
    it "should create, start, stop and delete a container" $ runWithLocalHost def $ do
        name <- liftIO randomContainerName
        logOK $ "Creating " ++ show name
        lxcCreate . containerCreateRequest (coerce name)
                  . ContainerSourceRemote
                  $ remoteImage imagesRemote "alpine/3.4/amd64"

        logOK "Starting" >> lxcStart name
        logOK "Stopping" >> lxcStop name False
        logOK "Deleting" >> lxcDelete name

    it "should execute commands" $ do
        out <- withContainer $ \name -> lxcExec name "/bin/cat" [] "Hello World!"
        out `shouldBe` "Hello World!"

    it "should pull files" $ do
        out <- withContainer $ \name -> lxcFilePullRaw name "/etc/hostname"
        logOK $ "Contents of /etc/hostname: " ++ show out
        out `shouldNotBe` ""

    it "should delete files and directories" $ withExts [ExtFileDelete] $ do
        out <- withContainer $ \name -> do
            _ <- lxcExec name "/bin/mkdir" ["/hello_world_dir"] mempty
            _ <- lxcExec name "/bin/touch" ["/hello_world_file"] mempty
            lxcFileDelete name "/hello_world_dir"
            lxcFileDelete name "/hello_world_file"
            lxcFileListDir name "/"
        out `shouldNotContain` ["hello_world_dir"]
        out `shouldNotContain` ["hello_world_file"]


randomContainerName :: IO ContainerName
randomContainerName = ContainerName . T.unpack . ("lxd-test-suite-" <>) . UUID.toText <$> randomIO

withContainer :: (ContainerName -> WithLocalHost a) -> IO a
withContainer action = do
    name <- randomContainerName
    let create = lxcCreate . containerCreateRequest (coerce name)
                           . ContainerSourceRemote
                           $ remoteImage imagesRemote "alpine/3.4/amd64"
    runWithLocalHost def $ bracket_
        (logOK ("Creating " ++ show name) >> create >> lxcStart name)
        (logOK "Stopping" >> lxcStop name True >> lxcDelete name)
        (action name)

withExts :: [ApiExtension] -> IO () -> IO ()
withExts exts action = do
    avail <- apiExtensions <$> runWithLocalHost def lxcApi
    if all (`elem` avail) exts
    then action
    else logErr $ "Could not test: not all extensions available: " ++ show exts

--------------------------------------------------------------------------------

-- | Create a new LXD container.
createLxd :: IO Text
createLxd = do
    containerName <- ("lxd-client-test-" <>) . UUID.toText <$> randomIO
    logOK $ "Creating LXD container " ++ show containerName
    sh $ procs "lxc" ["launch", "images:ubuntu/xenial", containerName,
                      "-c", "security.privileged=true",
                      "-c", "security.nesting=true"] empty
    threadDelay 5000000
    return containerName

-- | Destory the LXD container.
destroyLxd :: Text -> IO ()
destroyLxd containerName = sh $ do
    logOK $ "destroying LXD container " ++ show containerName
    procs "lxc" ["delete", "-f", containerName] empty

-- | Initialize a container to run tests in.
initializeLxd :: Text -> IO ()
initializeLxd containerName = sh $ do
    logOK $ "Installing LXD into " ++ show containerName
    let exec cmd = procs "lxc" (["exec", containerName, "--"] ++ cmd) empty
    exec ["apt-get", "update"]
    exec ["apt-get", "install", "-y", "software-properties-common"]
    exec ["add-apt-repository", "-y", "ppa:ubuntu-lxc/lxd-stable"]
    exec ["apt-get", "update"]
    exec ["apt-get", "-y", "install", "lxd"]
    exec ["lxd", "init", "--auto"]
    exec ["lxc", "network", "create", "lxdbr0"]
    exec ["lxc", "network", "attach-profile", "lxdbr0", "default", "eth0"]

-- | Run the test suite in the specified container.
runTestSuiteLxd :: Text -> IO ()
runTestSuiteLxd containerName = sh $ do
    executable <- liftIO getExecutablePath
    logOK $ "Pushing " ++ executable ++ " into " ++ show containerName
    procs "lxc" ["file", "push", T.pack executable,
                 containerName <> "/usr/local/bin/lxd-client-integration"]
                empty

    logOK $ "Executing the integration tests inside " ++ show containerName
    procs "lxc" ["exec", containerName,
                 "--env", "LOCAL=yes", "--",
                 "/usr/local/bin/lxd-client-integration"] empty

--------------------------------------------------------------------------------

logOK :: MonadIO m => String -> m ()
logOK = log "[+]"

logErr :: MonadIO m => String -> m ()
logErr = log "[-]"

log :: MonadIO m => String -> String -> m ()
log p m | (w, msg) <- splitLogMessage m = liftIO . putStrLn $ w ++ p ++ " " ++ msg

splitLogMessage :: String -> (String, String)
splitLogMessage = span (== ' ')

--------------------------------------------------------------------------------
