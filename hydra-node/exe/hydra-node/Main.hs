module Main where

import Cardano.Prelude

import Hydra.Ledger (cardanoLedger)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Node (createHydraNode, runHydraNode)
import Hydra.Repl (startHydraRepl)

main :: IO ()
main = do
  node <- createHydraNode ledger
  startHydraRepl node
  runHydraNode node
 where
  ledger = cardanoLedger defaultEnv

  defaultEnv = MaryTest.mkLedgerEnv
