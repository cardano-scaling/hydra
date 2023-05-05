{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Ledger where

import Hydra.Prelude

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
  , --
    Eq (UTxOType tx)
  , Show (UTxOType tx)
  , Arbitrary (UTxOType tx)
  , FromJSON (UTxOType tx)
  , Monoid (UTxOType tx)
  , ToJSON (UTxOType tx)
  ) =>
  IsTx tx
  where
  type UTxOType tx
  type TxIdType tx
  type ValueType tx

  -- XXX(SN): this name easily conflicts
  txId :: tx -> TxIdType tx
  balance :: UTxOType tx -> ValueType tx

  -- | Hash a utxo set to be able to sign (off-chain) and verify it (on-chain).
  hashUTxO :: UTxOType tx -> ByteString

-- | An abstract interface for a 'Ledger'. Allows to define mock / simpler
-- implementation for testing as well as limiting feature-envy from the business
-- logic by forcing a closed interface.
data Ledger tx = Ledger
  { applyTransactions ::
      ChainSlot ->
      UTxOType tx ->
      [tx] ->
      Either (tx, ValidationError) (UTxOType tx)
  -- ^ Apply a set of transaction to a given UTXO set. Returns the new UTXO or
  -- validation failures returned from the ledger.
  -- TODO: 'ValidationError' should also include the UTxO, which is not
  -- necessarily the same as the given UTxO after some transactions
  , initUTxO :: UTxOType tx
  -- ^ Generates an initial UTXO set. This is only temporary as it does not
  -- allow to initialize the UTXO.
  --
  -- TODO: This seems redundant with the `Monoid (UTxOType tx)` constraints
  -- coming with `IsTx`. We probably want to dry this out.
  }

-- | A generic description for a chain slot all implementions need to use.
newtype ChainSlot = ChainSlot Natural
  deriving (Ord, Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Get the next chain slot. Use this instead of giving 'Enum' or 'Num'
-- instances to 'ChainSlot'.
nextChainSlot :: ChainSlot -> ChainSlot
nextChainSlot (ChainSlot n) = ChainSlot (n + 1)

instance Arbitrary ChainSlot where
  arbitrary = genericArbitrary

canApply :: Ledger tx -> ChainSlot -> UTxOType tx -> tx -> ValidationResult
canApply ledger slot utxo tx =
  either (Invalid . snd) (const Valid) $ applyTransactions ledger slot utxo (pure tx)

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
