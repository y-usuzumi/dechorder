import           Data.Proxy
import           Test.Tasty
import           Test.Tasty.Options
import qualified Tests.BasicNotation.TestParser                   as Parser (tests)
import qualified Tests.BasicNotation.TestPitchIntervalCalculation as PitchIntervalCalculation
                                                                                               (tests)
import qualified Tests.Integration.TestHibiki as Hibiki (tests)
import           Tests.TestEnv

unitTests :: TestTree
unitTests = testGroup "Unit tests" [ PitchIntervalCalculation.tests
                                   , Parser.tests
                                   ]

integrationTests :: TestTree
integrationTests = testGroup "Integration tests" [ Hibiki.tests
                                                 ]
allTests :: TestTree
allTests = askOption $ \TestEnv{..} ->
  testGroup "All tests" $ if runIntegrationTests
                          then [integrationTests]
                          else [unitTests]

main :: IO ()
main = defaultMainWithIngredients ingredients allTests
  where
    ingredients = configFileIngredient:defaultIngredients
    configFileIngredient = includingOptions [Option (Proxy :: Proxy TestEnv)]
