-- | Module for the on-chain representation of Utxo.
module Hydra.Data.Utxo where

import Hydra.Prelude

import qualified PlutusTx

data Utxo = Utxo

instance PlutusTx.FromData Utxo where
  fromBuiltinData _ = pure Utxo
