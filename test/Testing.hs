{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Testing where

import Network.LXD.Prelude

newtype Tester m a = Tester { runTester :: StateT [(String, Test m String)] m a }
                   deriving (Functor, Applicative, Monad, MonadIO, MonadState [(String, Test m String)])

instance MonadTrans Tester where
    lift = Tester . lift

test :: Monad io => String -> Test io String -> Tester io ()
test name test' = modify (++ [(name, test')])

testShow :: (Monad io, Show a)  => String -> Test io a -> Tester io ()
testShow name test' = test name (show <$> test')

runTester' :: MonadIO m => Tester m () -> m ()
runTester' tester = do
    tests <- snd <$> (flip runStateT [] . runTester $ tester)
    failures <- snd <$> foldlM (runSingleTest (length tests)) (1, 0) tests
    liftIO . putStrLn $ ""
    liftIO . putStrLn $ "Tests finished: " ++ show failures ++ "/" ++ show (length tests) ++ " failed"
  where
    runSingleTest :: MonadIO m => Int -> (Int, Int) -> (String, Test m String) -> m (Int, Int)
    runSingleTest total (i, failures) (name, action) = do
        liftIO . putStrLn $ ""
        liftIO . putStrLn $ "Testing (" ++ show i ++ "/" ++ show total ++ ") " ++ show name
        res <- runExceptT . runTest $ action
        case res of
            Left err -> do
                liftIO . putStrLn $ "    error:   " ++ err
                return (i+1, failures+1)
            Right v  -> do
                liftIO . putStrLn $ "    success: " ++ v
                return (i+1, failures)

newtype Test m a = Test { runTest :: ExceptT String m a }
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
