module MyXmobar (myxmobarPP, xmobarCmd, xmobarHeight) where

import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.DynamicLog

xmobarHeight :: Int
xmobarHeight = 34

xmobarCmd :: String
xmobarCmd = "xmobar ~/.xmobarrc"

xmobarBtn :: Int -> String -> String -> String
xmobarBtn n cmd = wrap open "</action>"
  where open = "<action=`" ++ cmd ++ "` button=" ++ show n ++ ">"

-- workspaceScrollable :: String -> String
-- workspaceScrollable =
--     xmobarBtn 4 "xdotool key super+Up" .
--     xmobarBtn 5 "xdotool key super+Down"

clickToSwitchWorkspace :: String -> String
clickToSwitchWorkspace n =
    xmobarBtn 1 ("$HOME/.xmonad/bin/switch_workplace " ++ n)
    $ wrap " " " " n

myxmobarPP :: PP
myxmobarPP = def
  { ppCurrent         = xmobarColor "black" "white" . wrap "  " "  "
  , ppVisible         = xmobarColor "gray"  "black" . clickToSwitchWorkspace
  , ppHidden          = xmobarColor "gray"  "black" . clickToSwitchWorkspace
  , ppHiddenNoWindows = const ""
  , ppUrgent          = xmobarColor "red"   "yellow" . pad
  , ppWsSep           = ""
  , ppSep             = ""
  , ppSort            = getSortByTag
  , ppLayout          = xmobarColor "white" "black"
                        . xmobarBtn 1 "xdotool key super+space"
                        . (\x -> pad $ case x of
                            "Column by ZoomRow"              -> "≡│"
                            "Full by ZoomRow"                -> "□│"
                            "Tabs by ZoomRow"                -> "┴│"
                            "Mirror Column by ZoomRow"       -> "◫-"
                            "Mirror Full by ZoomRow"         -> "□-"
                            "Mirror Tabs by ZoomRow"         -> "┴-"
                            "Column by ZoomRow (Max)"        -> "≡"
                            "Full by ZoomRow (Max)"          -> "□"
                            "Tabs by ZoomRow (Max)"          -> "┴"
                            "Mirror Column by ZoomRow (Max)" -> "◫"
                            "Mirror Full by ZoomRow (Max)"   -> "□"
                            "Mirror Tabs by ZoomRow (Max)"   -> "┴"
                            "Full"                           -> "□ "
                            _                                -> x
                          )
  , ppTitle           = xmobarColor "white" "black" . xmobarStrip
  }
