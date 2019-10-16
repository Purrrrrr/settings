module MyDzen (mydzenPP, dzenCmd, dzenWidth, dzenHeight) where

import XMonad.Util.Loggers 
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.DynamicLog 

dzenWidth = 500
dzenHeight = 20
dzenCmd = "dzen2 -bg black -fg white -fn :size=7 -w " ++ (show dzenWidth) 
  ++ " -h " ++ (show dzenHeight) ++ " -ta l"

withMouseAction button action n = wrap ("^ca(" ++ (show button) ++ ", " ++ action ++ ")") "^ca()" n

wheelable up down n = withMouseAction 4 up $ withMouseAction 5 down n
clickable action n = withMouseAction 1 action n
doublepad = wrap "  " "  "

workspaceWheelable = wheelable "xdotool key super+Up" "xdotool key super+Down"
clickToSwitchWorksplace n = clickable ("$HOME/.xmonad/switch_workplace "++n) $ doublepad n

mydzenPP :: PP
mydzenPP = defaultPP { 
    ppCurrent  = dzenColor "black" "white" . workspaceWheelable . doublepad
  , ppVisible  = dzenColor "gray" "black" . workspaceWheelable .
                 clickToSwitchWorksplace
  , ppHidden   = dzenColor "gray" "black" . workspaceWheelable .
                 clickToSwitchWorksplace
  , ppHiddenNoWindows = const "" --dzenColor "white" "#CCCCCC" . pad
  , ppUrgent   = dzenColor "red" "yellow" . pad
  , ppWsSep    = ""
  , ppSep      = ""
  , ppSort     = getSortByTag
  , ppLayout   = dzenColor "white" "black" .
                 workspaceWheelable .
                 --wheelable "xdotool key super+space" "xdotool key super+space" .
                 clickable "xdotool key super+space" .
                 (\ x -> pad $ case x of
                           "Column by ZoomRow" -> "≡│"
                           "Full by ZoomRow" -> "□│"
                           "Tabs by ZoomRow" -> "┴│"
                           "Mirror Column by ZoomRow" -> "◫-"
                           "Mirror Full by ZoomRow" -> "□-"
                           "Mirror Tabs by ZoomRow" -> "┴-"
                           "Column by ZoomRow (Max)" -> "≡"
                           "Full by ZoomRow (Max)" -> "□"
                           "Tabs by ZoomRow (Max)" -> "┴"
                           "Mirror Column by ZoomRow (Max)" -> "◫"
                           "Mirror Full by ZoomRow (Max)" -> "□"
                           "Mirror Tabs by ZoomRow (Max)" -> "┴"
                           "Full"          -> "□ "
                           _                      -> x
                 ) 
  , ppTitle    = workspaceWheelable . 
                 ("^bg(black) ^fg(white)" ++) . dzenEscape
}
