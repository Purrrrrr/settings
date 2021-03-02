{-# LANGUAGE DeriveDataTypeable #-}
module WorkspaceGroups(defaultInWorkspaceGroup, nextUnboundWorkspace, nextWorkspaceInGroup, nextFromWorkspaceGroup, updateCurrentDefault, updateDefaultInGroup, withDefaultWorkspaceInGroup) where

import XMonad
import qualified XMonad.StackSet as SSet
import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as XS
import Data.List (findIndex, isPrefixOf)
import Data.Maybe
import Control.Monad
import XMonad.Util.WorkspaceCompare

data DefaultWorkspaces = DefaultWorkspaces (M.Map String WorkspaceId) deriving (Typeable,Read,Show)

instance ExtensionClass DefaultWorkspaces where
  initialValue = DefaultWorkspaces M.empty
  extensionType = PersistentExtension

type Group = String

--TODO: nextScreen?, swapnextScreen?, windowPromptGoto, workspacePrompt, swapWorkspace

updateDefaultInGroup :: X() -> X()
updateDefaultInGroup act = do
  act
  updateCurrentDefault

updateCurrentDefault :: X()
updateCurrentDefault = do
  ws <- gets windowset
  let cur = SSet.tag $ SSet.workspace $ SSet.current ws
      group = workspaceGroup cur
  setDefaultWorkspace group cur

setDefaultWorkspace :: String -> WorkspaceId -> X()
setDefaultWorkspace group tag = do
  XS.modify setTag
  where setTag (DefaultWorkspaces m) =
          let newM = M.insert group tag m
          in DefaultWorkspaces newM

withDefaultWorkspaceInGroup :: Group -> (WorkspaceId -> X ()) -> X ()
withDefaultWorkspaceInGroup grp action = do
  ws <- defaultInWorkspaceGroup grp
  fromMaybe (return ()) (fmap action ws)

defaultInWorkspaceGroup :: Group -> X (Maybe WorkspaceId)
defaultInWorkspaceGroup group = do
  (DefaultWorkspaces m) <- XS.get :: X DefaultWorkspaces
  ws   <- gets windowset
  workspaces <- workspacesByGroup group
  let existing = map SSet.tag $ SSet.workspaces ws
      tagExists x = x `elem` existing
      possibleDefault = mfilter tagExists $ M.lookup group m
  return $ case possibleDefault of
                Nothing -> case workspaces of
                             ws:_ -> Just ws
                             _ -> Nothing
                x -> x

nextWorkspaceInGroup :: X WorkspaceId
nextWorkspaceInGroup = do
  ws   <- gets windowset
  let cur = SSet.tag $ SSet.workspace $ SSet.current ws
  nextFromWorkspaceGroup (workspaceGroup cur)

nextUnboundWorkspace :: X WorkspaceId
nextUnboundWorkspace = nextFromWorkspaceGroup ""

nextFromWorkspaceGroup :: Group -> X WorkspaceId
nextFromWorkspaceGroup group = do
  list <- workspacesByGroup group
  ws   <- gets windowset
  let cur = SSet.tag $ SSet.workspace $ SSet.current ws
  def <- defaultInWorkspaceGroup group
  let next = nextWorkspaceFromList cur list
  return $ if cur `elem` list then next else (fromMaybe cur def)

workspaceGroup :: WorkspaceId -> Group
workspaceGroup ws = let x = head ws
                        isNumbered = '0' < x && x <= '9'
                    in if isNumbered then [x] else ""

isInGroup :: Group -> WorkspaceId -> Bool
isInGroup grp ws = workspaceGroup ws == grp

workspacesByGroup :: Group -> X [WorkspaceId]
workspacesByGroup group = do
  sort <- getSortByTag
  ws   <- gets windowset
  let sorted = map SSet.tag $ sort $ SSet.workspaces ws
  return $ filter (isInGroup group) sorted

nextWorkspaceFromList :: WorkspaceId -> [WorkspaceId] -> WorkspaceId
nextWorkspaceFromList cur [] = cur
nextWorkspaceFromList cur wss = let pivoted = let (a,b) = span (/= cur) wss in b ++ a 
                                    curindex  = findIndex (== cur) pivoted
                                in  case curindex of
                                         Nothing -> pivoted !! 0
                                         Just ix -> pivoted !! ((ix + 1) `mod` length pivoted)
