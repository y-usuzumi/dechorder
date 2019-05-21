module Audio.Chan where

import Control.Monad
import qualified Control.Concurrent.BoundedChan as C
import           Control.Concurrent.MVar

data HBoundedChan a = HBoundedChan { chan   :: C.BoundedChan a
                                   , closed :: MVar Bool
                                   }

ensureOpen :: HBoundedChan a -> String -> IO b -> IO b
ensureOpen ch errMsg action = do
  closed <- readMVar $ closed ch
  -- FIXME: Incomplete implementation
  when closed $ error errMsg
  action

newHBoundedChan :: Int -> IO (HBoundedChan a)
newHBoundedChan x = do
  closed <- newMVar False
  chan <- C.newBoundedChan x
  return HBoundedChan{chan, closed}

writeChan :: HBoundedChan a -> a -> IO ()
writeChan ch = ensureOpen ch "Cannot write to a closed chan" . C.writeChan (chan ch)

tryWriteChan :: HBoundedChan a -> a -> IO Bool
tryWriteChan ch = ensureOpen ch "Cannot write to a closed chan" . C.tryWriteChan (chan ch)

-- TODO:
readChan :: HBoundedChan a -> IO a
readChan ch = C.readChan (chan ch)
