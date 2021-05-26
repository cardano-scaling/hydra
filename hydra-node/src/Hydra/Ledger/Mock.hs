{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

-- | A mock implementation of a ledger
module Hydra.Ledger.Mock where

import Cardano.Prelude hiding (show)
import Codec.Serialise
import Hydra.Ledger
import Text.Read (Read (..))
import Text.Show (Show (..))

-- | Simple mock transaction, which conflates value and identity
data MockTx = ValidTx Amount | InvalidTx
  deriving stock (Eq, Generic)
  deriving anyclass (Serialise)

instance Read MockTx where
  readPrec =
    ValidTx <$> readPrec @Amount
      <|> pure InvalidTx

instance Show MockTx where
  show = \case
    ValidTx i -> show i
    InvalidTx -> "_|_"

type instance LedgerState MockTx = MockLedgerState

data MockLedgerState = MockLedgerState
  { utxo :: Map ParticipationToken Amount
  , transactions :: [MockTx]
  }
  deriving (Show)

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx _ -> Valid
        InvalidTx -> Invalid ValidationError
    , applyTransaction = \MockLedgerState{transactions, utxo} tx ->
        -- TODO(MB): Here we need to check whether that particular tx is
        -- "valid", that is, if whoever is sending it has the funds.
        let transactions' = tx : transactions
         in Right $ MockLedgerState{utxo, transactions = transactions'}
    , initLedgerState = \utxo ->
        let transactions = mempty
         in MockLedgerState{utxo, transactions}
    }
