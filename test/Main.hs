module Main where

import Network.LXD.Client.Internal.Prelude
import Testing

import Integration.API (apiTester)

main :: IO ()
main = runTester' apiTester
