module Main where

import Cardano.Prelude
import qualified Hydra.ContractModelSpec as Model
import qualified Hydra.ContractSpec as Contract
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
