import           Test.Tasty
import qualified Tests.BasicNotation.TestPitchIntervalCalculation as PitchIntervalCalculation
                                                                                                   (tests)
import qualified Tests.BasicNotation.TestParser as Parser
                                                                                                   (tests)

tests :: TestTree
tests = testGroup "All tests" [ PitchIntervalCalculation.tests
                              , Parser.tests
                              ]

main :: IO ()
main = defaultMain tests
