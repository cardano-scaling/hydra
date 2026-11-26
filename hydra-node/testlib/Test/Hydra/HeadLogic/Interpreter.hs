module Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig, evaluateResult) where

import Hydra.Prelude

import Hydra.HeadLogic.Interpreter (InterpreterConfig (..), SnapshotSigner (..), mkInterpreterConfig)
import Hydra.HeadLogic.Outcome (Outcome (..))
import Hydra.HeadLogic.Program (Instruction (..), Program, foldProgram)
import Hydra.Ledger (Ledger)
import Hydra.Tx (IsTx)
import Hydra.Tx.Crypto (getVerificationKey, sign)
import Test.Hydra.Tx.Fixture (aliceSk)

testInterpreterConfig :: IsTx tx => Ledger tx -> InterpreterConfig tx
testInterpreterConfig ledger =
  mkInterpreterConfig ledger $ SnapshotSigner (sign aliceSk) (getVerificationKey aliceSk)

-- | Evaluate validation programs using the production primitive implementations.
-- Returning a value here must not emit effects or retain state changes.
evaluateResult :: InterpreterConfig tx -> Program tx a -> Either (Outcome tx) a
evaluateResult config = foldProgram $ \case
  ApplyTransaction slot utxo tx -> pure $ transactionApplication config slot utxo tx
  SignSnapshot snapshot -> pure $ signOne (snapshotSigner config) snapshot
  VerifySnapshot parties signatures bytes -> pure $ snapshotVerifier config parties signatures bytes
  TraceBoolean _ answer -> pure answer
  Suspend reason -> Left $ Wait reason []
  Reject err -> Left $ Error err
  Change event -> Left $ Continue [event] []
  Emit effect -> Left $ Continue [] [effect]
  GetUserValidation -> pure $ userValidation config
