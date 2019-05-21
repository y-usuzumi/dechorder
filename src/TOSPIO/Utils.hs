module TOSPIO.Utils where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString
import           GHC.IO.Handle
import           System.IO.Temp

withFileContentAsTempFile :: (MonadIO m, MonadMask m) => ByteString -> (FilePath -> m a) -> m a
withFileContentAsTempFile content action = withSystemTempFile "dechorderTmp.wav" $ \fp h -> do
  liftIO $ do hPut h content
              hClose h
  action fp
