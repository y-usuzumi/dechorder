module Tests.BasicNotation.TestPitchIntervalCalculation where

import           Test.Tasty
import           Test.Tasty.HUnit
import           TOSPIO.Musician.Notation.IPN

tests :: TestTree
tests = testGroup "Pitch interval calculation"
  [ testCase "Test 1" $ do
      let pitch = Pitch (3, PitchClass (C, []))
          interval = Interval { number = 2, quality = 0 }
      pitch <:+ interval @?= Pitch (3, PitchClass (E, []))
  , testCase "Test 1.1" $ do
      let pitch = Pitch (3, PitchClass (C, []))
          interval = Interval { number = 2, quality = -1 }
      pitch <:+ interval @?= Pitch (3, PitchClass (E, [Flat]))
  , testCase "Test 1.2" $ do
      let pitch = Pitch (3, PitchClass (E, []))
          interval = Interval { number = 2, quality = 0 }
      pitch <:- interval @?= Pitch (3, PitchClass (C, []))
  , testCase "Test 2" $ do
      let pitch = Pitch (3, PitchClass (E, []))
          interval = Interval { number = 2, quality = 0 }
      pitch <:+ interval @?= Pitch (3, PitchClass (G, [Sharp]))
  , testCase "Test 3" $ do
      let pitch = Pitch (3, PitchClass (B, []))
          interval = Interval { number = 2, quality = 0 }
      pitch <:+ interval @?= Pitch (4, PitchClass (D, [Sharp]))
  , testCase "Test 4" $ do
      let pitch = Pitch (3, PitchClass (C, []))
          interval = Interval { number = 13, quality = 0 }
      pitch <:+ interval @?= Pitch (4, PitchClass (B, []))
  ]
