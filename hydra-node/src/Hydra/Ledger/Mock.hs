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
data MockTx = ValidTx TxId | InvalidTx
  deriving stock (Eq, Generic)
  deriving anyclass (Serialise)

type TxId = Integer

instance Read MockTx where
  readsPrec prec =
    \case
      "_|_" -> [(InvalidTx, "")]
      s -> first ValidTx <$> readsPrec prec s

instance Show MockTx where
  show = \case
    ValidTx i -> show i
    InvalidTx -> "_|_"

type instance LedgerState MockTx = MockLedgerState

newtype MockLedgerState = MockLedgerState
  { transactions :: [MockTx]
  }
  deriving (Show)

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx _ -> Valid
        InvalidTx -> Invalid ValidationError
    , applyTransaction = \MockLedgerState{transactions} tx ->
        -- NOTE:
        -- There's no need to represent a real `tx` and do any fake ledger
        -- validation because we can already represent that via `InvalidTx`.
        --
        -- In the end, we are really interested in the resulting UTxO which
        -- _could_ be constructed from all the valid transactions that have
        -- passed through the head. So it suffices to keep a list of all valid
        -- transactions in the mock.
        case tx of
          InvalidTx ->
            Left ValidationError
          ValidTx{} ->
            let transactions' = tx : transactions
             in Right $ MockLedgerState{transactions = transactions'}
    , initLedgerState = MockLedgerState mempty
    }
