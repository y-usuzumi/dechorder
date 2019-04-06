module Tests.BasicNotation.TestParser where

import           Data.Void
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           TOSPIO.Musician.Notation.Basic
import           TOSPIO.Musician.Notation.Basic.Parser

pitchParserTests :: TestTree
pitchParserTests = testGroup "Pitch parser"
  [ testCase "Test 1" $ do
      let result = parseMaybe pitchParser "C#4"
      result @?= Just (Pitch (4, PitchClass (C, [Sharp])))
  , testCase "Test 2" $ do
      let result = parseMaybe pitchParser "C4"
      result @?= Just (Pitch (4, PitchClass (C, [])))
  ]

intervalParserTests :: TestTree
intervalParserTests = testGroup "Interval parser"
  [ testCase "Test 1" $ do
      let result = parseMaybe intervalParser "3+"
      result @?= Just (Interval { number = 2, quality = 1 })
  ]

tests :: TestTree
tests = testGroup "Parser"
  [ pitchParserTests
  , intervalParserTests
  ]
