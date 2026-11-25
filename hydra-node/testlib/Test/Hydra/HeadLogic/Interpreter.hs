module Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig) where

import Hydra.Prelude

import Hydra.HeadLogic.Interpreter (InterpreterConfig, SnapshotSigner (..), mkInterpreterConfig)
import Hydra.Ledger (Ledger)
import Hydra.Tx (IsTx)
import Hydra.Tx.Crypto (getVerificationKey, sign)
import Test.Hydra.Tx.Fixture (aliceSk)

testInterpreterConfig :: IsTx tx => Ledger tx -> InterpreterConfig tx
testInterpreterConfig ledger =
  mkInterpreterConfig ledger $ SnapshotSigner (sign aliceSk) (getVerificationKey aliceSk)
