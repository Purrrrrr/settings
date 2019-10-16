module SpawnAndRead(spawnReadPipe) where

import Codec.Binary.UTF8.String
import Control.Monad (unless)
import System.Posix.IO
import System.Posix.Process (executeFile, forkProcess)
import System.IO
import System.Process (runInteractiveProcess)
import XMonad
import Control.Monad


-- | Launch an external application through the system shell and return a @Handle@ to its standard input.
spawnReadPipe :: MonadIO m => String -> m Handle
spawnReadPipe x = io $ do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    r <- fdToHandle wr
    hSetBuffering r LineBuffering
    _ <- xfork $ do
          _ <- dupTo wr stdOutput
          executeFile "/bin/sh" False ["-c", encodeString x] Nothing
    closeFd wr
    return r

processEachLine :: (String -> IO a) -> IO ()
processEachLine f = do
  finished <- isEOF
  unless finished $ do
    f =<< getLine
    processEachLine f

