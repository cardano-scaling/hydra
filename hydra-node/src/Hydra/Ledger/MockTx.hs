-- | A mock implementation of a ledger
module Hydra.Ledger.MockTx where

import Cardano.Prelude
import Hydra.Ledger

data MockTx = ValidTx Integer | InvalidTx
  deriving (Eq, Show)

type instance LedgerState MockTx = [MockTx]

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx _ -> Valid
        InvalidTx -> Invalid ValidationError
    , applyTransaction = \st tx -> pure (tx : st)
    , initLedgerState = []
    }
