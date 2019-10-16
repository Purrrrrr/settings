module MyKeys(myKeys) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Window
import WorkspacePromptPlus
import XMonad.Util.Run 
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageDocks 
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Commands

import qualified XMonad.StackSet as StackSet
import XMonad.Layout.Groups.Helpers

import XMonad.Layout.MessageControl
import XMonad.Layout.LayoutCombinators 
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import WorkspaceGroups
import ZoomRowPlus
import ZoomRowPlus.Group

import FadeInactive
import Stoppable

import qualified Data.Map as M
import Data.List (findIndex, isInfixOf, isPrefixOf)
import Data.Char (toLower )
altMask = mod1Mask
changeInnerlayout l = sendMessage $ escape $ JumpToLayout l

promptConfig = defaultXPConfig { 
  position = Top,
  searchPredicate = (\s -> \s2 -> isInfixOf (map toLower s) (map toLower s2))
}
autoPromptConfig = promptConfig { 
  autoComplete = Just 1
}

dmenu_colors = "-fn '-*-fixed-medium-r-*-*-18-*-*-*-*-*-utf-8' -nb 'white' -nf 'black' -sf 'white' -sb 'red'"
dmenu_exec  = "dmenu_run "++dmenu_colors
dmenu_files = "exe=`$HOME/.xmonad/dmenu_home | dmenu -i -l 30 "++dmenu_colors++"` && eval \"xdg-open \\\"$exe\\\"\""
--dmenu_directories = "exe=`$HOME/.xmonad/dmenu_home | dmenu -i -l 30 "++dmenu_colors++"` && eval \"xdg-open $( dirname \\\"$exe\\\" ;)\" "

suspend_cmd   = "$HOME/.xmonad/suspend.sh"
hibernate_cmd = "dbus-send --system --print-reply --dest=\"org.freedesktop.UPower\" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate"

currentLayoutName :: X String
currentLayoutName = withWindowSet handle
                    where handle :: WindowSet -> X String
                          handle ws = return $ layoutName ws
                          layoutName :: WindowSet -> String
                          layoutName ws = description . StackSet.layout . StackSet.workspace . StackSet.current $ ws

mirrorAction act mirroredAct = do
  name <- currentLayoutName
  if (head $ words name) == "Mirror"
  then mirroredAct
  else act 

rotView dir = do t <- findWorkspace getSortByTag dir NonEmptyWS 1
                 withDefaultUpdate $ windows . StackSet.view $ t

myKeys conf@(XConfig {XMonad.modMask = modMask}) = 
  let _swapDown = do { swapDown; refresh; } --For some obscure reasons these two need a refresh
      _swapUp = do { swapUp; refresh; }
      _moveToGroupDown = moveToGroupDown False
      _moveToGroupUp = moveToGroupUp False
  in M.fromList $ [ 
      ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_x), spawn "xeyes")
    , ((modMask, xK_p), spawn dmenu_exec)
    , ((modMask, xK_o), spawn dmenu_files)
    --, ((modMask .|. shiftMask, xK_o), spawn dmenu_directories)
    , ((modMask, xK_Escape), spawn hibernate_cmd)
    , ((modMask, xK_s), spawn suspend_cmd)
    --
    , ((modMask, xK_Down),  rotView Next)
    , ((modMask, xK_Up),    rotView Prev)
    --
    , ((modMask .|. altMask, xK_Down),  spawn "pactl set-sink-volume 0 -- -5%")
    , ((modMask .|. altMask, xK_Up),    spawn "pactl set-sink-volume 0 -- +5%")
    --
    , ((modMask .|. controlMask,  xK_j), nextScreen) 
    , ((modMask .|. controlMask,  xK_k), prevScreen) 
    , ((modMask,                xK_d), swapNextScreen)
    , ((modMask .|. shiftMask,  xK_d), shiftNextScreen)

    --, ((modMask, xK_w), goToSelected gridConfigWin) 
    --, ((modMask, xK_t), gridselectWorkspace gridConfig StackSet.view) 
    --, ((modMask .|. shiftMask, xK_t), gridselectWorkspace gridConfig StackSet.shift)
    , ((modMask, xK_w), withDefaultUpdate $ windowPromptGoto autoPromptConfig) 
    , ((modMask, xK_t), withDefaultUpdate $ workspacePrompt autoPromptConfig (windows.StackSet.view)) 
    , ((modMask .|. shiftMask, xK_t), workspacePrompt autoPromptConfig (windows.StackSet.shift))

    , ((modMask .|. controlMask , xK_t), withFocused $ windows . StackSet.sink) 

    , ((modMask, xK_n ), withDefaultUpdate $ addWorkspacePrompt promptConfig)
    , ((modMask, xK_r ), withDefaultUpdate $ workspacePrompt promptConfig $ windows . swapWithCurrent)
    --, ((modMask, xK_r ), renameWorkspace promptConfig)
    --, ((modMask, xK_r ), gridselectWorkspace gridConfig (swapWithCurrent))
    , ((modMask, xK_BackSpace), withDefaultUpdate $ removeEmptyWorkspace)
    , ((modMask, xK_0), do { ws <-nextUnboundWorkspace; withDefaultUpdate $ windows (StackSet.view ws); } )

    , ((modMask, xK_b), sendMessage ToggleStruts)
    --wmiLike bindings
    , ((modMask, xK_j), mirrorAction focusDown focusGroupDown)
    , ((modMask, xK_k), mirrorAction focusUp focusGroupUp)
    , ((modMask .|. shiftMask , xK_j), mirrorAction _swapDown _moveToGroupDown) 
    , ((modMask .|. shiftMask , xK_k), mirrorAction _swapUp _moveToGroupUp) 
    , ((modMask, xK_l), mirrorAction focusGroupDown focusDown)
    , ((modMask, xK_h), mirrorAction focusGroupUp focusUp)
    , ((modMask .|. shiftMask , xK_l), (mirrorAction _moveToGroupDown _swapDown))
    , ((modMask .|. shiftMask , xK_h), (mirrorAction _moveToGroupUp _swapUp))

    --Grow and shrink bindings
    , ((altMask, xK_l), mirrorAction growColumnDown growWindowDown)
    , ((altMask, xK_h), mirrorAction shrinkColumnDown shrinkWindowDown)
    , ((altMask, xK_j), mirrorAction growWindowDown growColumnDown)
    , ((altMask, xK_k), mirrorAction shrinkWindowDown shrinkColumnDown)
    , ((altMask .|. shiftMask, xK_l), mirrorAction shrinkColumnUp shrinkWindowUp)
    , ((altMask .|. shiftMask, xK_h), mirrorAction growColumnUp growWindowUp)
    , ((altMask .|. shiftMask, xK_j), mirrorAction shrinkWindowUp shrinkColumnUp)
    , ((altMask .|. shiftMask, xK_k), mirrorAction growWindowUp growColumnUp)

    , ((modMask, xK_u), changeInnerlayout "Tabs")
    , ((modMask, xK_i), changeInnerlayout "Column")
    , ((modMask, xK_y), changeInnerlayout "Full")
    , ((modMask .|. shiftMask, xK_f), toggleColumnFull)

    , ((modMask .|. altMask, xK_f), toggleFadeInactive) 
    , ((modMask, xK_m), sendMessage $ Toggle MIRROR) 
    , ((modMask, xK_space ), sendMessage $ Toggle FULL ) 
    , ((modMask, xK_f), sendMessage $ Toggle FULL ) 
    , ((modMask .|. shiftMask, xK_p), sendMessage $ Toggle STOPPABLE )
    --, ((modMask, xK_space ), sendMessage NextLayout) 
    --, ((modMask, .|. shiftMask, xK_space ), sendMessage PreviousLayout)  -- Not implemented!

    , ((modMask, xK_a), windows copyToAll)
    , ((modMask .|. shiftMask, xK_a), killAllOtherCopies)
    ] ++ 
    [ ((modifier .|. modMask, xK_Tab), do
      ws <- gets windowset
      let cur = StackSet.tag $ StackSet.workspace $ StackSet.current ws
          h = head cur
          group = if h `elem` ['1'..'9'] then [h] else ""
      nextws <- nextFromWorkspaceGroup group
      withDefaultUpdate $ windows $ action nextws
      ) | (action, modifier) <- [(StackSet.view, 0), (StackSet.shift, shiftMask)] 
    ] ++ 
    [ ((modifier2 .|. modifier .|. modMask, key), do
        nextws <- nextFromWorkspaceGroup $ show wsnum
        defaultws <- getDefaultWorkspace nextws $ show wsnum
        let ws = if useOnlyDefault then defaultws else nextws
            in withDefaultUpdate $ windows $ action ws
       )
        | 
        (wsnum, key) <- zip ([1..]) ( [xK_1 .. xK_9]),
        (action, modifier) <- [(StackSet.view, 0), (StackSet.shift, shiftMask), (copy, controlMask)],

        (modifier2, useOnlyDefault) <- [(altMask, False), (0,True)]
    ]
