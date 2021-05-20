-- | A mock implementation of a ledger
module Hydra.Ledger.Mock where

import Cardano.Prelude
import Hydra.Ledger

data MockTx = ValidTx Integer | InvalidTx
  deriving (Eq, Read, Show)

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
