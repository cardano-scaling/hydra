{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Tx.IsTx (IsTx (applyTxTo), UTxOType)
import Hydra.UserValidationRules (PolicyContext (..), PolicyViolation, UTxOValidation (..))
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary

instance Arbitrary PolicyViolation where
  arbitrary = genericArbitrary

-- | Build test contexts by checking the complete UTxO after each update.
-- Fixtures supply only their rule; no optimized summary is needed.
utxoValidationWith :: IsTx tx => (UTxOType tx -> UTxOType tx -> Either PolicyViolation ()) -> UTxOValidation tx
utxoValidationWith check =
  UTxOValidation
    { preparePolicyUTxO = contextFor
    , localPolicyContext = Nothing
    }
 where
  contextFor active outgoing =
    PolicyContext
      { policyResult = check active outgoing
      , applyPolicyTx = \tx -> contextFor (applyTxTo tx active) outgoing
      , addPolicyUTxO = \incoming -> contextFor (active <> incoming) outgoing
      }

-- | Get the next chain slot. Use this instead of giving 'Enum' or 'Num'
-- instances to 'ChainSlot'.
nextChainSlot :: ChainSlot -> ChainSlot
nextChainSlot (ChainSlot n) = ChainSlot (n + 1)

-- | Collect applicable transactions and resulting UTxO. In contrast to
-- 'applyTransactions', this functions continues on validation errors.
collectTransactions :: Ledger tx -> ChainSlot -> UTxOType tx -> [tx] -> ([tx], UTxOType tx)
collectTransactions Ledger{applyTransactions} slot utxo =
  foldl' go ([], utxo)
 where
  go (applicableTxs, u) tx =
    case applyTransactions slot u [tx] of
      Left _ -> (applicableTxs, u)
      Right u' -> (applicableTxs <> [tx], u')
