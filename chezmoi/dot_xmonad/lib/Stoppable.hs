{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
module Stoppable
    ( 
      STOPPABLE(..)
    ) where

import XMonad hiding ((|||))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Stoppable
import XMonad.Layout.MultiToggle

data STOPPABLE = STOPPABLE deriving (Read, Show, Eq, Typeable)
instance Transformer STOPPABLE Window where
  transform _ x k = k (stoppable x) (\(ModifiedLayout _ x') -> x')
