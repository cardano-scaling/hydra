{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Tx.IsTx (UTxOType)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary

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
