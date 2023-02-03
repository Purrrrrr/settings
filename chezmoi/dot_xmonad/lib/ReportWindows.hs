module ReportWindows(
  reportWindows,
  logWindowSet
) where

import XMonad hiding (focus, workspaces)
import XMonad.StackSet
import XMonad.Util.Run 
import System.IO
import Data.Maybe
import Data.List
import Control.Monad
-- import 

reportWindows :: X ()
reportWindows = do
  p <- spawnPipe "xargs -0 xmessage"
  logWindowSet p

logWindowSet :: Handle -> X ()
logWindowSet handle = do
  s <- withWindowSet (\ws -> winTitles ws)
  io $ do 
    hPutStrLn handle s
    hClose handle

implode :: String -> [String] -> String
implode _ []     = ""
implode _ [x]    = x
implode s (x:xs) = x ++ s ++ (implode s xs)

winTitles :: WindowSet -> X String
winTitles wset = wsStrs
  where 
        wsStrs = liftM (implode "\n\n" ) $ liftM sort $sequence stackStrs

        stackStrs = mapMaybe stackStr $ workspaces wset
          where stackStr ws | Just s <- stack ws = Just $ addInfo ws $ titles $ windows [s]
                stackStr _  = Nothing
        
        --addInfo :: Workspace -> X String -> X String
        addInfo ws = liftM (((tag ws) ++ ": " ++ (description $ layout ws) ++ "\n") ++) 

        titles wins = liftM (implode "\n") $ mapM (runQuery title) wins 
        windows ss = (map focus $ ss) ++ (concatMap up ss) ++ (concatMap down ss)
