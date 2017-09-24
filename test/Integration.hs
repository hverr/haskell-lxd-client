module Main where

import Network.LXD.Prelude hiding (log)

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import System.Environment (getExecutablePath, lookupEnv)
import System.Random (randomIO)

import Test.Hspec
import Turtle (sh, procs, empty)

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
testSuite = describe "containers" $
    it "should create a container" $
        True `shouldBe` True

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
