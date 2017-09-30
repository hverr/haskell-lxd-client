{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.LXD.Client.Internal.Compatibility.WebSockets where

import Network.LXD.Client.Internal.Prelude

import Data.ByteString.Lazy (ByteString)

import qualified Network.WebSockets as WS

import Network.LXD.Client.Internal.Compatibility (Compatibility(..))

data DataMessage = Text ByteString
                 | Binary ByteString
                 deriving (Eq, Show)

instance Compatibility WS.DataMessage DataMessage where
#if MIN_VERSION_websockets(0, 11, 0)
    compat (WS.Text v _) = Text v
    compat (WS.Binary v) = Binary v
#else
    compat (WS.Text v)   = Text v
    compat (WS.Binary v) = Binary v
#endif
