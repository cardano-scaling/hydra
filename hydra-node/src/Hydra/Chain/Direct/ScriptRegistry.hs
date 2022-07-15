-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.Direct.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Hydra.Cardano.Api (
  CtxUTxO,
  TxIn,
  TxOut,
 )

-- | Hydra scripts published as reference scripts at these UTxO.
newtype ScriptRegistry = ScriptRegistry
  { initialReference :: (TxIn, TxOut CtxUTxO)
  }
  deriving (Eq, Show)

registryUtxo :: ScriptRegistry -> UTxO
registryUtxo = undefined
