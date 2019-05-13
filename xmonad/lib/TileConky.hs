module TileConky (isConky, tileConky, tileConkyHook) where

import XMonad hiding ((|||))
import XMonad.Util.XUtils (fi)
import MyDzen
import Graphics.X11.Xlib.Types
import qualified XMonad.StackSet as W
import Data.Monoid (All(..))

isConky :: Query Bool
isConky = className =? "Conky"

tileConky :: ManageHook
tileConky = (ask >>= \w -> liftX (reveal w) >> liftX (resizeConky w) >> doF (W.delete w))

tileConkyHook :: Event -> X All
tileConkyHook (MapNotifyEvent {ev_window = w}) = do
  whenX ((not `fmap` (isClient w)) <&&> runQuery isConky w) (resizeConky w)
  return (All True)
tileConkyHook _ = return (All True)


resizeConky w = withDisplay $ \dpy -> do
  rootw <- asks theRoot
  wa <- io $ getWindowAttributes dpy rootw
  let screenW = (fi $ wa_width wa)
  let dzW = fi dzenWidth
  tileWindow w $ Rectangle dzW 0 (screenW - (fi dzW) - (floor $ fi screenW/10*2)) (fi dzenHeight)
