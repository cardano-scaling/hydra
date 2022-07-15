-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.Direct.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Hydra.Cardano.Api (
  CtxUTxO,
  NetworkId,
  TxId,
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

-- | Query for 'TxIn's in the search for outputs containing all the reference
-- scripts of the 'ScriptRegistry'.
--
-- This is implemented by repeated querying until we have all necessary
-- reference scripts as we do only know the transaction id, not the indices.
--
-- NOTE: This is limited to an upper bound of 10 to not query too much before
-- providing an error.
queryScriptRegistry :: NetworkId -> FilePath -> TxId -> IO ScriptRegistry
queryScriptRegistry = undefined
