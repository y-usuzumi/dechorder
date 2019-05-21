module Tests.Integration.TestHibiki where

import           Audio.Hibiki     (playFile)
import           Data.FileEmbed
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.Hspec
import           TOSPIO.Utils     (withFileContentAsTempFile)

test1 :: Spec
test1 =
  it "should play" $ withFileContentAsTempFile $(embedFile "samples/Yamaha-V50-Rock-Beat-120bpm.wav") $ \fp -> playFile fp

{-# NOINLINE tests #-}
tests :: TestTree
tests = unsafePerformIO $ testSpec "test1" test1
