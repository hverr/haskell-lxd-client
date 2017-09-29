-- | This module implements high-level client for the LXD daemon events
-- end-points.
module Network.LXD.Client.Events where

import Network.LXD.Client.Internal.Prelude

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Exception (Exception, catch, throwIO, finally)

import Data.Aeson (eitherDecode)
import Data.Coerce (coerce)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import qualified Network.WebSockets as WS

import Web.Internal.HttpApiData (ToHttpApiData(..))

import Network.LXD.Client.Types

eventsPath :: [EventType] -> String
eventsPath types = "/1.0/events?" ++ typesQuery
  where
    types' = intercalate "," $ map (T.unpack . toUrlPiece) types
    typesQuery | null types = ""
               | otherwise  = "type=" ++ types'

operationsPath :: String
operationsPath = eventsPath [EventTypeOperation]

readAllEvents :: (Maybe Event -> IO ()) -> WS.ClientApp ()
readAllEvents f con =
    go `finally` WS.sendClose con BL.empty
  where
    go = do
        m <- (Just <$> WS.receiveDataMessage con) `catch` handle'
        case m of Nothing            -> f Nothing
                  Just (WS.Text t)   -> decodeMsg t >>= f . Just >> go
                  Just (WS.Binary b) -> decodeMsg b >>= f . Just >> go

    handle' (WS.CloseRequest _ _) = return Nothing
    handle' e                     = throwIO e

    decodeMsg msg = case eitherDecode msg of
        Left err -> throwIO . LXDMessageError $ "could not decode event: " ++ err
        Right v -> return v

listenForOperation :: MVar OperationId -> MVar Operation -> WS.ClientApp ()
listenForOperation oid' chan con = do
    oid <- liftIO $ takeMVar oid'
    readAllEvents (send oid) con
  where
    send _ Nothing = throwIO $ LXDMessageError "LXD event stream prematurelly stopped"
    send oid (Just event) = case eventMetadata event of
        EventOperationMetadata op -> when (coerce (operationId op) == oid) $ putMVar chan op
        _                         -> return ()

newtype LXDMessageError = LXDMessageError String deriving (Show)

instance Exception LXDMessageError where
