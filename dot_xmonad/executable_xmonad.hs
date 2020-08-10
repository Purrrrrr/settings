import XMonad hiding ((|||))
import XMonad.Util.Run 
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import XMonad.Hooks.ManageDocks 
import XMonad.Hooks.DynamicLog --Used for dzen
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.LayoutCombinators 
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Helpers
--import XMonad.Layout.ZoomRow
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.MessageControl
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import Data.List (isInfixOf)
import qualified Data.Map (union)

import MyDzen
import MyKeys
import TileConky
import ZoomRowPlus
import ZoomRowPlus.Group
import TabAccordion
import FadeInactive
import Stoppable
import SpawnAndRead

main = do
  dzen_main <- spawnPipe dzenCmd 
  xcompmgr <- spawn "xcompmgr"
  xmonad $ ewmh defaultConfig { 
        terminal = "gnome-terminal"
      , borderWidth = 0 
      , modMask = mod4Mask
      , manageHook = manageDocks <+> composeAll [
          className =? "conky" --> tileConky
        , className =? "Gimp" --> (ask >>= doF . W.sink)
        , className =? "Gimp" --> doShift "Gimp"
        , className =? "Xfce4-notifyd" --> doF W.focusDown
        , title     ~? "Difference between " --> (ask >>= doF . W.sink)
        ]<+> manageHook defaultConfig
      , handleEventHook = tileConkyHook >> docksEventHook
      , logHook = takeTopFocus >> fadeInactiveLogHook 0.9 >> dynamicLogWithPP mydzenPP { ppOutput = hPutStrLn dzen_main }
      , startupHook = do
          setWMName "LG3D"
          spawnOnce "conky"
      , layoutHook = myLayoutHook
      , keys = \c -> myKeys c `Data.Map.union` keys defaultConfig c
      , workspaces = concatMap (\x -> [show x, show x ++ "B"]) [1..9] ++ ["Temp"]
  }

(~?) :: (Eq a, Functor f) => f [a] -> [a] -> f Bool
q ~? x  = fmap (x `isInfixOf`) q

myTheme = theme smallClean

wmii s t = G.group innerLayout zoomRowG
    where column = named "Column" $ Mirror zoomRow
          tabs = named "Tabs" $ tabAccordion s t
          innerLayout = renamed [CutWordsLeft 2] 
                        $ ignore NextLayout 
                        $ ignore (JumpToLayout "") $ unEscape 
                           $ column ||| tabs ||| Full

myLayoutHook = avoidStruts $
  --onWorkspace "Gimp" (Tall 1 (3/100) (4/5)) $
  mkToggle (single STOPPABLE) $
  mkToggle (single FULL) $
  mkToggle (single MIRROR) $
  wmii shrinkText myTheme
 
