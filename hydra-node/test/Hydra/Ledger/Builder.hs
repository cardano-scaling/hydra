-- | Provides utility functions to build transaction and other data types
-- useful for testing purpose
module Hydra.Ledger.Builder where

import Hydra.Prelude

import qualified Data.Set as Set
import Hydra.Ledger (UTxO)
import Hydra.Ledger.Simple (SimpleTx (..), TxIn (..))

utxoRef :: Integer -> UTxO SimpleTx
utxoRef = Set.singleton . TxIn

utxoRefs :: [Integer] -> UTxO SimpleTx
utxoRefs = Set.fromList . fmap TxIn

aValidTx :: Integer -> SimpleTx
aValidTx n = SimpleTx n mempty (utxoRef n)
