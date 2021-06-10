{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

-- | A mock implementation of a ledger
module Hydra.Ledger.Mock where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeTag, encodeTag)
import Cardano.Prelude
import Control.Monad (fail)
import Data.List (nub)
import Hydra.Ledger

-- | Simple mock transaction.
--
-- NOTE: There's no need to represent a real `tx` and do any fake ledger
-- validation because we can already represent that via `InvalidTx`.
--
-- In the end, we are really interested in the resulting UTxO which
-- _could_ be constructed from all the valid transactions that have
-- passed through the head. So it suffices to keep a list of all valid
-- transactions in the mock.
data MockTx = ValidTx TxId | InvalidTx
  deriving stock (Eq, Ord, Generic, Read, Show)

instance ToCBOR MockTx where
  toCBOR (ValidTx txid) = encodeTag 1 <> toCBOR txid
  toCBOR InvalidTx = encodeTag 2

instance FromCBOR MockTx where
  fromCBOR = do
    tag <- decodeTag
    case tag of
      1 -> ValidTx <$> fromCBOR
      2 -> pure InvalidTx
      _ -> fail $ "Unknown tag " <> show tag

type TxId = Integer

instance Tx MockTx where
  type UTxO MockTx = [MockTx]

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { applyTransactions = foldM $ \utxo tx ->
        case tx of
          InvalidTx ->
            Left ValidationError
          ValidTx{} -> Right $ nub (tx : utxo)
    , initUTxO = mempty
    }
