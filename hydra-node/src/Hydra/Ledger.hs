{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Ledger where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..))
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Tx.IsTx (IsTx (..))
import Test.Gen.Cardano.Api.Typed (genBlockHeaderHash)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()

-- | Get the next chain point.
nextChainPoint :: ChainPoint -> Gen ChainPoint
nextChainPoint point = do
  blockHash <- hedgehog genBlockHeaderHash
  pure $ case point of
    ChainPointAtGenesis -> ChainPoint 0 blockHash
    ChainPoint (SlotNo n) _ -> ChainPoint (fromIntegral n + 1) blockHash

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

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary
