module Main where

import Cardano.Prelude

import Hydra.Ledger (cardanoLedger)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Node (createHydraNode, runHydraNode)

main :: IO ()
main = do
  createHydraNode ledger >>= runHydraNode
 where
  ledger = cardanoLedger defaultEnv

  defaultEnv = MaryTest.mkLedgerEnv
