{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (error)

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Either (EitherT(..))

import Data.Default (def)

import Servant.Client (runClientM)

import Network.LXD.Client

main :: IO ()
main = do
    test "connectToRemote guest"   $ connectToRemote NoClientAuth
    test "connectToRemote trusted" $ connectToRemote DefaultClientAuth

test :: (MonadIO m, Show a) => Show a => String -> Test m a -> m ()
test name action = do
    liftIO . putStrLn $ "Testing " ++ name
    res <- runEitherT . runTest $ action
    case res of
        Left err -> liftIO . putStrLn $ "    error:   " ++ show err
        Right v  -> liftIO . putStrLn $ "    success: " ++ show v

connectToRemote :: MonadIO m => ClientAuth -> Test m ApiConfig
connectToRemote auth = do
    client <- remoteHostClient host
    resp <- assertEitherShow =<< (lift . liftIO $ runClientM apiConfig client)
    dat <- assertResponseOK resp
    case auth of NoClientAuth      -> assertEq Untrusted (authStatus dat)
                 DefaultClientAuth -> assertEq Trusted   (authStatus dat)
                 ClientAuthKey _   -> assertEq Untrusted (authStatus dat)
    return dat
  where
    host = def { remoteHostHost = "127.0.0.1"
               , remoteHostClientKey = auth }

newtype Test m a = Test { runTest :: EitherT String m a }
               deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

instance MonadTrans Test where
    lift = Test . lift

assertTrue :: Monad m => Bool -> String -> Test m ()
assertTrue True  _ = return ()
assertTrue False m = throwError m

assertEq :: (Monad m, Eq a, Show a) => a -> a -> Test m ()
assertEq a b = assertTrue (a == b) $ "Expected equality but got: " ++ show a ++ " /= " ++ show b

assertEither :: Monad m => Either String a -> Test m a
assertEither (Right x) = return x
assertEither (Left  m) = throwError m

assertEitherShow :: (Monad m, Show err) => Either err a -> Test m a
assertEitherShow (Right x) = return x
assertEitherShow (Left  m) = throwError $ show m

assertResponseOK :: Monad m => Response a -> Test m a
assertResponseOK Response{..}
    | 200 <- statusCode = return metadata
    | otherwise = throwError $ "Expected response with code 200 but got " ++ show statusCode ++ " with error code " ++ show errorCode ++ "(" ++ show error ++ ")"
