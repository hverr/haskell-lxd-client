module Network.LXD.Prelude (
  -- * Re-exports
  module Prelude

  -- * Monads
, module Control.Monad

  -- * Monad Transformers
, MonadError, ExceptT, runExceptT, throwError
, MonadIO, liftIO
, MonadState, StateT(..), modify, get, put
, MonadTrans, lift

  -- * Foldable
, foldlM

  -- * Monoid
, module Data.Monoid

  -- * Strings
, IsString(fromString)
) where

import Prelude hiding (error)

-- Monads
import Control.Monad (join, unless, void)

-- Monad transformers
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT(..), modify, get, put)
import Control.Monad.Trans.Class (MonadTrans, lift)

-- Foldable
import Data.Foldable (foldlM)

-- Monoid
import Data.Monoid (Monoid, mempty, mappend, (<>))

-- Strings
import Data.String (IsString(fromString))
