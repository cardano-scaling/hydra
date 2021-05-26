module Main where

import Cardano.Prelude
import qualified Hydra.ContractModelTest as Model
import qualified Hydra.ContractTest as Contract
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Contracts tests"
    [ Contract.tests
    , Model.tests
    ]
