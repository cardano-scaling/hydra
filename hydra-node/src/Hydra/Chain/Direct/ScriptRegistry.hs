-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.Direct.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Hydra.Cardano.Api (
  CtxUTxO,
  NetworkId,
  TxId,
  TxIn (..),
  TxIx (..),
  TxOut,
 )
import Hydra.Chain.CardanoClient (QueryPoint (..), queryUTxOByTxIn)

-- | Hydra scripts published as reference scripts at these UTxO.
newtype ScriptRegistry = ScriptRegistry
  { initialReference :: (TxIn, TxOut CtxUTxO)
  }
  deriving (Eq, Show)

registryUtxo :: ScriptRegistry -> UTxO
registryUtxo = undefined

-- TODO: Give more context to this exception.
data UnableToConstructRegistry = UnableToConstructRegistry
  deriving (Show)

instance Exception UnableToConstructRegistry

-- | Query for 'TxIn's in the search for outputs containing all the reference
-- scripts of the 'ScriptRegistry'.
--
-- This is implemented by repeated querying until we have all necessary
-- reference scripts as we do only know the transaction id, not the indices.
--
-- NOTE: This is limited to an upper bound of 10 to not query too much before
-- providing an error.
queryScriptRegistry :: NetworkId -> FilePath -> TxId -> IO ScriptRegistry
queryScriptRegistry networkId nodeSocket txId =
  loop [TxIx 0 .. maxIteration]
 where
  maxIteration = TxIx 10 -- Arbitrary but, high-enough.
  loop = \case
    [] ->
      throwIO UnableToConstructRegistry
    ixs -> do
      _utxo <- queryUTxOByTxIn networkId nodeSocket QueryTip (TxIn txId <$> ixs)
      print _utxo
      loop []
