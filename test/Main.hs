module Main where

import Network.LXD.Prelude
import Testing

import Integration.API (apiTester)

main :: IO ()
main = runTester' apiTester
