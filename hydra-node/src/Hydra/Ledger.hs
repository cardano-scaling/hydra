{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Ledger where

import "hydra-prelude" Hydra.Prelude

import "hydra-tx" Hydra.Chain.ChainState (ChainSlot (..))
import "hydra-tx" Hydra.Tx.IsTx (IsTx (..))

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

newtype ValidationError = ValidationError {reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
