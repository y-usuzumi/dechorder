module Tests.TestEnv where

import           Options.Applicative
import           System.IO.Unsafe
import           Test.Tasty.Options

newtype TestEnv = TestEnv { runIntegrationTests :: Bool
                          }

instance IsOption TestEnv where
  defaultValue = TestEnv{ runIntegrationTests = False }
  parseValue fp = Just TestEnv{ runIntegrationTests = False }
  optionName = return "run-integration-tests"
  optionHelp = return "Controls whether integration tests are included"
  optionCLParser = mkFlagCLParser (short 'I') $ TestEnv True
