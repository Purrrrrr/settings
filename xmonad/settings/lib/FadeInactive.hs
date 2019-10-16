{-# LANGUAGE DeriveDataTypeable #-}

module FadeInactive(toggleFadeInactive, fadingOn, fadeInactiveLogHook) where

import XMonad
import XMonad.Core
import qualified XMonad.Hooks.FadeInactive as F
import qualified XMonad.Util.ExtensibleState as XS

data FadeInactiveOn = FadeInactiveOn Bool deriving Typeable

instance ExtensionClass FadeInactiveOn where
  initialValue = FadeInactiveOn True

toggleFadeInactive :: X ()
toggleFadeInactive = do
  XS.modify (\x@(FadeInactiveOn b) -> FadeInactiveOn (not b))
  refresh

isUnfocusedAndFadingOn :: Query Bool
isUnfocusedAndFadingOn = F.isUnfocused <&&> fadingOn

fadingOn :: Query Bool
fadingOn= liftX $ do 
  f@(FadeInactiveOn b) <- XS.get :: X FadeInactiveOn
  return b

fadeInactiveLogHook :: Rational -> X ()
fadeInactiveLogHook = F.fadeOutLogHook . F.fadeIf isUnfocusedAndFadingOn
