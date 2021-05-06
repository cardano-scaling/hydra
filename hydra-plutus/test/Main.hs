module Main where

import Cardano.Prelude
import qualified Hydra.ContractStateMachineTest as ContractStateMachine
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Contracts tests"
    [ ContractStateMachine.tests
    ]
