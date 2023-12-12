{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Ledger where

import Hydra.Prelude

import Data.Aeson (FromJSONKey, ToJSONKey)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()

class
  ( Eq tx
  , Show tx
  , Typeable tx
  , Arbitrary tx
  , FromCBOR tx
  , ToCBOR tx
  , FromJSON tx
  , ToJSON tx
  , --
    Eq (TxIdType tx)
  , Ord (TxIdType tx)
  , Show (TxIdType tx)
  , Typeable (TxIdType tx)
  , Arbitrary (TxIdType tx)
  , FromJSON (TxIdType tx)
  , ToJSON (TxIdType tx)
  , FromCBOR (TxIdType tx)
  , ToCBOR (TxIdType tx)
  , FromJSONKey (TxIdType tx)
  , ToJSONKey (TxIdType tx)
  , --
    Eq (UTxOType tx)
  , Show (UTxOType tx)
  , Arbitrary (UTxOType tx)
  , FromJSON (UTxOType tx)
  , Monoid (UTxOType tx)
  , ToJSON (UTxOType tx)
  , FromCBOR (UTxOType tx)
  , ToCBOR (UTxOType tx)
  ) =>
  IsTx tx
  where
  type UTxOType tx = utxo | utxo -> tx
  type TxIdType tx
  type ValueType tx

  -- XXX(SN): this name easily conflicts
  txId :: tx -> TxIdType tx
  balance :: UTxOType tx -> ValueType tx

  -- | Hash a utxo set to be able to sign (off-chain) and verify it (on-chain).
  hashUTxO :: UTxOType tx -> ByteString

  txSpendingUTxO :: UTxOType tx -> tx

  -- | Get the UTxO produced by given transaction.
  utxoFromTx :: tx -> UTxOType tx

  -- | Return the left-hand side without the right-hand side.
  withoutUTxO :: UTxOType tx -> UTxOType tx -> UTxOType tx

-- | A generic description for a chain slot all implementions need to use.
newtype ChainSlot = ChainSlot Natural
  deriving stock (Ord, Eq, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON)

instance Arbitrary ChainSlot where
  arbitrary = genericArbitrary

-- | Get the next chain slot. Use this instead of giving 'Enum' or 'Num'
-- instances to 'ChainSlot'.
nextChainSlot :: ChainSlot -> ChainSlot
nextChainSlot (ChainSlot n) = ChainSlot (n + 1)

-- | An abstract interface for a 'Ledger'. Allows to define mock / simpler
-- implementation for testing as well as limiting feature-envy from the business
-- logic by forcing a closed interface.
newtype Ledger tx = Ledger
  { applyTransactions ::
      ChainSlot ->
      UTxOType tx ->
      [tx] ->
      Either (tx, ValidationError) (UTxOType tx)
  -- ^ Apply a set of transaction to a given UTxO set. Returns the new UTxO or
  -- validation failures returned from the ledger.
  -- TODO: 'ValidationError' should also include the UTxO, which is not
  -- necessarily the same as the given UTxO after some transactions
  }

canApply :: Ledger tx -> ChainSlot -> UTxOType tx -> tx -> ValidationResult
canApply ledger slot utxo tx =
  either (Invalid . snd) (const Valid) $ applyTransactions ledger slot utxo (pure tx)

-- | Collect applicable transactions and resulting UTxO. In contrast to
-- 'applyTransactions', this functions continues on validation errors.
collectTransactions :: Ledger tx -> ChainSlot -> UTxOType tx -> [tx] -> ([tx], UTxOType tx)
collectTransactions Ledger{applyTransactions} slot utxo =
  foldr go ([], utxo)
 where
  go tx (applicableTxs, u) =
    case applyTransactions slot u [tx] of
      Left _ -> (applicableTxs, u)
      Right u' -> (applicableTxs <> [tx], u')

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ValidationError = ValidationError {reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary
