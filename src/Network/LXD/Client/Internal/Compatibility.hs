{-# LANGUAGE FunctionalDependencies #-}
module Network.LXD.Client.Internal.Compatibility where

class Compatibility a b | a -> b where
    compat :: a -> b
