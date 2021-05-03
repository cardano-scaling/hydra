module Main where

import Cardano.Prelude
import Hydra.Ledger (cardanoLedger)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Logic (HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node

main :: IO ()
main = do
  createHydraNode headState ledger >>= runHydraNode
 where
  ledger = cardanoLedger defaultEnv

  defaultEnv = MaryTest.mkLedgerEnv
