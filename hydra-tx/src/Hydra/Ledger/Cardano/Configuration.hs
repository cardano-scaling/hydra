module Hydra.Ledger.Cardano.Configuration (
  module Hydra.Ledger.Cardano.Configuration,
  Ledger.Globals,
  Ledger.LedgerEnv,
) where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Shelley.API.Types qualified as Ledger

-- * LedgerEnv

-- | Create a new ledger env from given protocol parameters.
newLedgerEnv :: PParams LedgerEra -> Ledger.LedgerEnv LedgerEra
newLedgerEnv protocolParams =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 0
    , -- NOTE: This can probably stay at 0 forever. This is used internally by the
      -- node's mempool to keep track of transaction seen from peers. Transactions
      -- in Hydra do not go through the node's mempool and follow a different
      -- consensus path so this will remain unused.
      Ledger.ledgerIx = minBound
    , -- NOTE: This keeps track of the ledger's treasury and reserve which are
      -- both unused in Hydra. There might be room for interesting features in the
      -- future with these two but for now, we'll consider them empty.
      Ledger.ledgerAccount = Ledger.AccountState mempty mempty
    , Ledger.ledgerPp = protocolParams
    }
