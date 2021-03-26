import Cardano.Prelude
import qualified Hydra.ContractSpec as ContractSpec
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Contracts tests"
    [ ContractSpec.tests
    ]
