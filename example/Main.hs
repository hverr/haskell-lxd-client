{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Network.LXD.Client.Commands

main :: IO ()
main = runWithLocalHost def $ do
    liftIO $ putStrLn "Creating my-container"
    lxcCreate . containerCreateRequest "my-container"
              . ContainerSourceRemote
              $ remoteImage imagesRemote "ubuntu/xenial/amd64"

    liftIO $ putStrLn "Starting my-container"
    lxcStart "my-container"

    liftIO $ putStrLn "Stopping my-container"
    lxcStop "my-container" False

    liftIO $ putStrLn "Deleting my-container"
    lxcDelete "my-container"
