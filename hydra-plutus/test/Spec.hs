import Cardano.Prelude
import qualified Hydra.ContractSpec as Contract
import qualified Hydra.ContractStateMachineSpec as ContractStateMachine
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Contracts tests"
    [ Contract.tests
    , ContractStateMachine.tests
    ]
