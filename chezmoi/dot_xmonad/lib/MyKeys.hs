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

import XMonad.Layout.Gaps
import XMonad.Layout.MessageControl
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import WorkspaceGroups
import ZoomRowPlus
import ZoomRowPlus.Group

import FadeInactive

import qualified Data.Map as M
import Data.List (findIndex, isInfixOf, isPrefixOf)
import Data.Char (toLower )
import System.Exit

altMask = mod1Mask
altGrMask = mod5Mask
changeInnerlayout l = sendMessage $ escape $ JumpToLayout l

promptConfig = defaultXPConfig {
  position = Top,
  searchPredicate = (\s s2 -> isInfixOf (map toLower s) (map toLower s2))
}
autoPromptConfig = promptConfig {
  autoComplete = Just 1
}

dmenu_colors = "-fn '-*-fixed-medium-r-*-*-18-*-*-*-*-*-utf-8' -nb 'white' -nf 'black' -sf 'white' -sb 'red'"
dmenu_exec  = "dmenu_run "++dmenu_colors
dmenu_files = "exe=`$HOME/.xmonad/bin/dmenu_home | dmenu -i -l 30 "++dmenu_colors++"` && eval \"xdg-open \\\"$exe\\\"\""

mirrorAction act mirroredAct = do
  name <- currentLayoutName
  if (head $ words name) == "Mirror"
  then mirroredAct
  else act

currentLayoutName :: X String
currentLayoutName = withWindowSet handle
                    where handle :: WindowSet -> X String
                          handle ws = return $ layoutName ws
                          layoutName :: WindowSet -> String
                          layoutName ws = description . StackSet.layout . StackSet.workspace . StackSet.current $ ws

myKeys conf@(XConfig {XMonad.modMask = modMask}) =
  let _swapDown = do { swapDown; refresh; } --For some obscure reasons these two need a refresh
      _swapUp = do { swapUp; refresh; }
      _moveToGroupDown = moveToGroupDown False
      _moveToGroupUp = moveToGroupUp False
      rotView dir = do t <- findWorkspace getSortByTag dir NonEmptyWS 1
                       updateDefaultInGroup $ windows . StackSet.view $ t
  in M.fromList $ [
    --Misc
      ((modMask .|. altMask, xK_Down),  spawn "pactl set-sink-volume 0 -- -5%")
    , ((modMask .|. altMask, xK_Up),    spawn "pactl set-sink-volume 0 -- +5%")
    , ((modMask, xK_b), do
      sendMessage ToggleGaps
      sendMessage ToggleStruts
      )
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    --Spawn programs
    , ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_x), spawn "xeyes")
    , ((modMask, xK_p), spawn dmenu_exec)
    , ((modMask, xK_o), spawn dmenu_files)

    --wmiLike bindings
    , ((altMask, xK_j), mirrorAction focusDown focusGroupDown)
    , ((altMask, xK_k), mirrorAction focusUp focusGroupUp)
    , ((altMask .|. shiftMask , xK_j), mirrorAction _swapDown _moveToGroupDown)
    , ((altMask .|. shiftMask , xK_k), mirrorAction _swapUp _moveToGroupUp)
    , ((altMask, xK_l), mirrorAction focusGroupDown focusDown)
    , ((altMask, xK_h), mirrorAction focusGroupUp focusUp)
    , ((altMask .|. shiftMask , xK_l), (mirrorAction _moveToGroupDown _swapDown))
    , ((altMask .|. shiftMask , xK_h), (mirrorAction _moveToGroupUp _swapUp))

    --Grow and shrink bindings
    , ((altGrMask, xK_l), mirrorAction growColumnDown growWindowDown)
    , ((altGrMask, xK_h), mirrorAction shrinkColumnDown shrinkWindowDown)
    , ((altGrMask, xK_j), mirrorAction growWindowDown growColumnDown)
    , ((altGrMask, xK_k), mirrorAction shrinkWindowDown shrinkColumnDown)
    , ((altGrMask .|. shiftMask, xK_l), mirrorAction shrinkColumnUp shrinkWindowUp)
    , ((altGrMask .|. shiftMask, xK_h), mirrorAction growColumnUp growWindowUp)
    , ((altGrMask .|. shiftMask, xK_j), mirrorAction shrinkWindowUp shrinkColumnUp)
    , ((altGrMask .|. shiftMask, xK_k), mirrorAction growWindowUp growColumnUp)

    --Change layouts
    , ((modMask, xK_u), changeInnerlayout "Tabs")
    , ((modMask, xK_i), changeInnerlayout "Column")
    , ((modMask, xK_y), changeInnerlayout "Full")
    , ((modMask .|. shiftMask, xK_f), toggleColumnFull)
    , ((modMask .|. altMask, xK_f), toggleFadeInactive)
    , ((modMask, xK_m), sendMessage $ Toggle MIRROR)
    , ((modMask, xK_space ), sendMessage $ Toggle FULL )
    , ((modMask, xK_f), sendMessage $ Toggle FULL )

    --Manage windows and workspaces
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
    , ((modMask .|. controlMask , xK_t), withFocused $ windows . StackSet.sink)

    , ((modMask, xK_a), windows copyToAll)
    , ((modMask .|. shiftMask, xK_a), killAllOtherCopies)

    , ((modMask, xK_0), do { ws <-nextUnboundWorkspace; updateDefaultInGroup $ windows (StackSet.view ws); } )
    , ((modMask, xK_Down),  rotView Next)
    , ((modMask, xK_Up),    rotView Prev)
    , ((modMask, xK_Tab), nextWorkspaceInGroup >>= updateDefaultInGroup.windows.(StackSet.view))
    , ((modMask .|. shiftMask, xK_Tab), nextWorkspaceInGroup >>= updateDefaultInGroup.windows.(StackSet.shift))

    , ((modMask, xK_n ), updateDefaultInGroup $ addWorkspacePrompt promptConfig)
    , ((modMask, xK_r ), updateDefaultInGroup $ workspacePrompt promptConfig $ windows . swapWithCurrent)
    , ((modMask, xK_BackSpace), updateDefaultInGroup $ removeEmptyWorkspace)

    , ((modMask, xK_t), updateDefaultInGroup $ workspacePrompt autoPromptConfig (windows.StackSet.view))
    , ((modMask .|. shiftMask, xK_t), workspacePrompt autoPromptConfig (windows.StackSet.shift))
    --
    --Manage screens
    , ((modMask .|. controlMask,  xK_j), nextScreen)
    , ((modMask .|. controlMask,  xK_k), prevScreen)
    , ((modMask,                xK_d), swapNextScreen)
    , ((modMask .|. shiftMask,  xK_d), shiftNextScreen)
    ] ++
    concat [ 
      ([
        ((modMask, key), withDefaultWorkspaceInGroup group $ updateDefaultInGroup.windows.(StackSet.view))
      , ((modMask .|. shiftMask, key), withDefaultWorkspaceInGroup group $ updateDefaultInGroup.windows.(StackSet.shift))
      , ((modMask .|. controlMask, key), withDefaultWorkspaceInGroup group $ updateDefaultInGroup.windows.copy)
      ])
        |
        (group, key) <- zip (map show [1..]) [xK_1 .. xK_9]
    ]
