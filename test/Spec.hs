module Main where

import Control.Monad.Trans.Either (EitherT(..))

import Data.Default (def)
import Data.Either.Combinators (mapBoth)

import Servant.Client (runClientM)

import Network.LXD.Client

main :: IO ()
main =
    test "connectToRemote" connectToRemote

test :: Show a => String -> EitherT String IO a -> IO ()
test name action = do
    putStrLn $ "Testing " ++ name
    res <- runEitherT action
    case res of
        Left err -> putStrLn $ "    error:   " ++ show err
        Right v  -> putStrLn $ "    success: " ++ show v

connectToRemote :: EitherT String IO String
connectToRemote = do
    client <- remoteHostClient host
    EitherT (mapBoth show show <$> runClientM apiConfig client)
  where
    host = def { remoteHostHost = "127.0.0.1"
               , remoteHostClientKey = NoClientAuth }
