{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Ledger where

import Hydra.Prelude

class
  ( Eq tx
  , Show tx
  , Typeable tx
  , FromCBOR tx
  , ToCBOR tx
  , FromJSON tx
  , ToJSON tx
  , --
    Eq (TxIdType tx)
  , Ord (TxIdType tx)
  , Show (TxIdType tx)
  , Typeable (TxIdType tx)
  , FromJSON (TxIdType tx)
  , ToJSON (TxIdType tx)
  , --
    Eq (UtxoType tx)
  , Show (UtxoType tx)
  , FromJSON (UtxoType tx)
  , Monoid (UtxoType tx)
  , ToJSON (UtxoType tx)
  ) =>
  IsTx tx
  where
  type UtxoType tx
  type TxIdType tx
  type ValueType tx

  txId :: tx -> TxIdType tx
  balance :: UtxoType tx -> ValueType tx

-- | An abstract interface for a 'Ledger'. Allows to define mock / simpler
-- implementation for testing as well as limiting feature-envy from the business
-- logic by forcing a closed interface.
data Ledger tx = Ledger
  { -- | Apply a set of transaction to a given UTXO set. Returns the new UTXO or
    -- validation failures returned from the ledger.
    applyTransactions ::
      UtxoType tx ->
      [tx] ->
      Either (tx, ValidationError) (UtxoType tx)
  , -- | Generates an initial UTXO set. This is only temporary as it does not
    -- allow to initialize the UTXO.
    --
    -- TODO: This seems redundant with the `Monoid (UtxoType tx)` constraints
    -- coming with `IsTx`. We probably want to dry this out.
    initUtxo :: UtxoType tx
  }

canApply :: Ledger tx -> UtxoType tx -> tx -> ValidationResult
canApply ledger utxo tx =
  either (Invalid . snd) (const Valid) $ applyTransactions ledger utxo (pure tx)

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
