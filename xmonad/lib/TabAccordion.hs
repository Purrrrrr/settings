{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances, DeriveDataTypeable #-}

module TabAccordion(
        tabAccordion
) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Named
import qualified XMonad.StackSet as Stack

--tabAccordion :: Shrinker s => s -> Theme
--                 -> ModifiedLayout (Decoration AccordionTabs s) FixedAccordion Window
tabAccordion s t = named "TabAccordion" $ ModifiedLayout WindowHider $ decoration s t AccTabs $ FixedAccordion (decoHeight t)

data WindowHider a = WindowHider deriving (Show, Read)

-- Hides everything except tabs and the focused window
instance LayoutModifier WindowHider Window where
    redoLayout m r ms wrs = do 
      hook m
      let nwrs@(rects, l) = pureModifier m r ms wrs
          isDeco Nothing _ = False
          isDeco (Just js) (win, _) = not $ isInStack js win
          decorations = filter (isDeco ms) rects 
          isFocused Nothing _ = False
          isFocused (Just js) (win, _) = win == (Stack.focus js)
          focused = filter (isFocused ms) rects

      --trace $ show $ focused ++ decorations

      return (focused ++ decorations, l)

--Adds tabs to the top or bottom depending on if the window is in the set "down"
data AccordionTabs a = AccTabs deriving (Show, Read)

instance Eq a => DecorationStyle AccordionTabs a where
    --shrink  _ _  r = r
    
    --pureDecoration _ tabW tabH Screen WinStack LayoutWindows CurWin 
    pureDecoration _ tabW tabH _ s@(Stack.Stack fw ups dns) _ (w,Rectangle x y winW winH) =
        if dns == [] && ups == [] 
        then Nothing
        else Just $ if w `elem` dns 
               then Rectangle x ny winW tabH  --bottom tab
               else Rectangle x y winW tabH --top tab
                    where ny :: Position
                          ny = fromIntegral $ y + fromIntegral (winH - tabH)

--Organizes the windows to the positions where they would lie if
--they were now in focus
data FixedAccordion a = FixedAccordion { d :: !Dimension}
        deriving ( Read, Show )

instance LayoutClass FixedAccordion Window where
    pureLayout (FixedAccordion d) sc ws = focusRec : recsDown ++ recsUp
     where
       fcsd   = Stack.focus ws
       ups    = Stack.up ws
       dns    = Stack.down ws
       wnds   = ups ++ (fcsd : dns)
       wndCount = length wnds
       upsCount = length ups
       baseRec :: Rectangle -> Int -> Rectangle
       baseRec (Rectangle x y w h) n = Rectangle x (y + dy) w nh
               where
                 accH :: Dimension
                 accH = fromIntegral $ wndCount - 1
                 dy = fromIntegral $ n * (fromIntegral d)
                 nh = h - accH * d
       focusRec = (fcsd, baseRec sc upsCount)
       recsUp = map (\(w,n) -> (w, baseRec sc n)) $ zip (reverse ups) [0..]
       recsDown = map (\(w,n) -> (w, baseRec sc n)) $ zip dns [(upsCount+1)..]
