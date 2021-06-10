module Hydra.Ledger where

import Cardano.Prelude hiding (undefined)

-- NOTE(MB): We probably want to move these common types somewhere else. Putting
-- here to avoid circular dependencies with Hydra.Logic

type Committed = Map ParticipationToken Amount

-- | Naiive representation of value, which is likely to change.
type Amount = Natural

-- | Identifies the commit of a single party member
data ParticipationToken = ParticipationToken
  { totalTokens :: Natural
  , thisToken :: Party
  }
  deriving (Eq, Ord, Show, Read)

-- | Identifies a party in a Hydra head.
type Party = Natural

-- * Ledger interface

class
  ( Eq tx
  , Eq (UTxO tx)
  , Eq (LedgerState tx)
  , Show tx
  , Show (UTxO tx)
  , Show (LedgerState tx)
  ) =>
  Tx tx
  where
  type UTxO tx
  type LedgerState tx

data Ledger tx = Ledger
  { canApply :: LedgerState tx -> tx -> ValidationResult
  , applyTransaction :: LedgerState tx -> tx -> Either ValidationError (LedgerState tx)
  , initLedgerState :: LedgerState tx
  , fromUTxO :: UTxO tx -> LedgerState tx
  , getUTxO :: LedgerState tx -> UTxO tx
  }

makeUTxO :: forall tx. Ledger tx -> UTxO tx -> [tx] -> Either ValidationError (UTxO tx)
makeUTxO Ledger{getUTxO, fromUTxO, applyTransaction} utxo txs =
  getUTxO <$> foldM applyTransaction (fromUTxO utxo) txs

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving (Eq, Show)

data ValidationError = ValidationError deriving (Eq, Show)

emptyUTxO :: Ledger tx -> UTxO tx
emptyUTxO Ledger{initLedgerState, getUTxO} =
  getUTxO initLedgerState
