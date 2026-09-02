{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Ledger where

import Hydra.Prelude

import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Tx.IsTx (IsTx (..))

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

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving stock (Eq, Show, Generic)

newtype ValidationError = ValidationError {reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR ValidationError where
  toCBOR = genericToCBOR

instance FromCBOR ValidationError where
  fromCBOR = genericFromCBOR
