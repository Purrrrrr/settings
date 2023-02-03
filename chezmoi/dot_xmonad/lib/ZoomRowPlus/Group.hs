{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}

module ZoomRowPlus.Group ( 
       zoomRowG
     , zoomColumnIn
     , zoomColumnOut
     , zoomColumnReset
     , zoomWindowIn
     , zoomWindowOut
     , zoomWindowReset 
     , toggleColumnFull
     , toggleWindowFull

     , growColumnDown
     , growColumnUp
     , shrinkColumnDown
     , shrinkColumnUp
      
     , growWindowDown
     , growWindowUp
     , shrinkWindowDown
     , shrinkWindowUp
) where

import XMonad hiding ((|||))

import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Helpers
import ZoomRowPlus


-- | Compare two 'Group's by comparing the ids of their layouts.
data GroupEQ a = GroupEQ
  deriving (Show, Read)

instance Eq a => EQF GroupEQ (G.Group l a) where
    eq _ (G.G l1 _) (G.G l2 _) = G.sameID l1 l2

zoomRowG :: (Eq a, Show a, Read a, Show (l a), Read (l a))
            => ZoomRow GroupEQ (G.Group l a)
zoomRowG = zoomRowWith GroupEQ

-- | Increase the width of the focused column
zoomColumnIn :: X ()
zoomColumnIn = sendMessage $ G.ToEnclosing $ SomeMessage $ zoomIn

-- | Decrease the width of the focused column
zoomColumnOut :: X ()
zoomColumnOut = sendMessage $ G.ToEnclosing $ SomeMessage $ zoomOut

-- | Reset the width of the focused column
zoomColumnReset :: X ()
zoomColumnReset = sendMessage $ G.ToEnclosing $ SomeMessage $ zoomReset

-- | Increase the heigth of the focused window
zoomWindowIn :: X ()
zoomWindowIn = sendMessage zoomIn

-- | Decrease the height of the focused window
zoomWindowOut :: X ()
zoomWindowOut = sendMessage zoomOut

-- | Reset the height of the focused window
zoomWindowReset :: X ()
zoomWindowReset = sendMessage zoomReset


-- | Toggle whether the currently focused column should
-- take up all available space whenever it has focus
toggleColumnFull :: X ()
toggleColumnFull = sendMessage $ G.ToEnclosing $ SomeMessage $ ZoomFullToggle

-- | Toggle whether the currently focused window should
-- take up the whole column whenever it has focus
toggleWindowFull :: X ()
toggleWindowFull = sendMessage ZoomFullToggle

growColumnDown = sendMessage $ G.ToEnclosing $ SomeMessage $  growDown
growColumnUp = sendMessage $ G.ToEnclosing $ SomeMessage $  growUp
shrinkColumnDown = sendMessage $ G.ToEnclosing $ SomeMessage $  shrinkDown
shrinkColumnUp = sendMessage $ G.ToEnclosing $ SomeMessage $  shrinkUp

growWindowDown = sendMessage growDown
growWindowUp = sendMessage growUp
shrinkWindowDown = sendMessage shrinkDown
shrinkWindowUp = sendMessage shrinkUp
