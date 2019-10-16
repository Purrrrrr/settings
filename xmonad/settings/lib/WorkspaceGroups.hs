{-# LANGUAGE DeriveDataTypeable #-}
module WorkspaceGroups(getDefaultWorkspace, nextUnboundWorkspace, nextFromWorkspaceGroup, updateCurrentDefault, withDefaultUpdate, test) where

import XMonad
import qualified XMonad.StackSet as SSet
import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as XS
import Data.List (findIndex, isPrefixOf)
import XMonad.Util.WorkspaceCompare

data DefaultWorkspaces = DefaultWorkspaces (M.Map String WorkspaceId) deriving (Typeable,Read,Show)

instance ExtensionClass DefaultWorkspaces where
  initialValue = DefaultWorkspaces M.empty
  extensionType = PersistentExtension

--TODO: nextScreen?, swapnextScreen?, windowPromptGoto, workspacePrompt, swapWorkspace

withDefaultUpdate :: X() -> X()
withDefaultUpdate act = do
  act
  updateCurrentDefault

test :: X() 
test = do
  (DefaultWorkspaces m) <- XS.get :: X DefaultWorkspaces
  trace (show m)

updateCurrentDefault :: X()
updateCurrentDefault = do
  ws <- gets windowset
  let cur = SSet.tag $ SSet.workspace $ SSet.current ws
      h = head cur
      group = if h `elem` ['1'..'9'] then [h] else ""
  setDefaultWorkspace group cur

setDefaultWorkspace :: String -> WorkspaceId -> X()
setDefaultWorkspace group tag = do
  XS.modify setTag
  where setTag (DefaultWorkspaces m) =
          let newM = M.insert group tag m
          in DefaultWorkspaces newM

getDefaultWorkspace :: WorkspaceId -> String -> X WorkspaceId
getDefaultWorkspace def group = do
  ws   <- gets windowset
  (DefaultWorkspaces m) <- XS.get :: X DefaultWorkspaces
  let tag = M.findWithDefault def group m
      existing = map SSet.tag $ SSet.workspaces ws
      tagExists = tag `elem` existing
  return $ if tagExists then tag else def

nextFromWorkspaceGroup:: String -> X WorkspaceId
nextFromWorkspaceGroup init = nextByFunction init (isPrefixOf init)

nextUnboundWorkspace :: X WorkspaceId
nextUnboundWorkspace = nextByFunction "" notBound
 where notBound x = not $ elem (head x) ['1'..'9']

nextByFunction :: String -> (WorkspaceId -> Bool) -> X WorkspaceId
nextByFunction group fun = do
  sort <- getSortByTag
  ws   <- gets windowset
  let sorted = map SSet.tag $ sort $ SSet.workspaces ws
      list = filter fun sorted
      cur = SSet.tag $ SSet.workspace $ SSet.current ws
      list_def = if null list then cur else head list
  def <- getDefaultWorkspace list_def group
  next <- nextWorkspaceFromList cur list
  return $ if cur `elem` list then next else def

-- Returns next 
nextWorkspaceFromList :: WorkspaceId -> [WorkspaceId] -> X WorkspaceId
nextWorkspaceFromList cur [] = do return cur
nextWorkspaceFromList cur wss = do
  ws <- gets windowset
  let pivoted = let (a,b) = span (/= cur) wss in b ++ a 
      curindex  = findIndex (== cur) pivoted
      next    = case curindex of
                    Nothing -> pivoted !! 0
                    Just ix -> pivoted !! ((ix + 1) `mod` length pivoted)
  return next 
