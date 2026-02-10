-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Tx.ScriptRegistry where

import "hydra-prelude" Hydra.Prelude
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "containers" Data.Map qualified as Map
import "hydra-cardano-api" Hydra.Cardano.Api (
  CtxUTxO,
  ScriptHash,
  TxIn (..),
  TxOut,
  UTxO,
  hashScriptInAnyLang,
  txOutReferenceScript,
  pattern ReferenceScript,
  pattern ReferenceScriptNone,
 )
import "hydra-plutus" Hydra.Contract (HydraScriptCatalogue (..), hydraScriptCatalogue)

-- | Hydra scripts published as reference scripts at these UTxO.
data ScriptRegistry = ScriptRegistry
  { initialReference :: (TxIn, TxOut CtxUTxO)
  , commitReference :: (TxIn, TxOut CtxUTxO)
  , headReference :: (TxIn, TxOut CtxUTxO)
  }
  deriving stock (Eq, Show, Generic)

data NewScriptRegistryException = MissingScript
  { scriptName :: Text
  , scriptHash :: ScriptHash
  , discoveredScripts :: Set ScriptHash
  }
  deriving stock (Eq, Show)

instance Exception NewScriptRegistryException

-- | Create a script registry from a UTxO containing outputs with reference
-- scripts. This will return 'Nothing' if one or all of the references could not
-- be found.
newScriptRegistry :: UTxO -> Either NewScriptRegistryException ScriptRegistry
newScriptRegistry =
  resolve . Map.foldMapWithKey collect . UTxO.toMap
 where
  collect ::
    TxIn ->
    TxOut CtxUTxO ->
    Map ScriptHash (TxIn, TxOut CtxUTxO)
  collect i o =
    case txOutReferenceScript o of
      ReferenceScriptNone -> mempty
      ReferenceScript script -> Map.singleton (hashScriptInAnyLang script) (i, o)

  resolve ::
    Map ScriptHash (TxIn, TxOut CtxUTxO) ->
    Either NewScriptRegistryException ScriptRegistry
  resolve m = do
    initialReference <- lookupScriptHash "νInitial" initialScriptHash m
    commitReference <- lookupScriptHash "νCommit" commitScriptHash m
    headReference <- lookupScriptHash "νHead" headScriptHash m
    pure $ ScriptRegistry{initialReference, commitReference, headReference}

  lookupScriptHash :: Text -> ScriptHash -> Map ScriptHash (TxIn, TxOut CtxUTxO) -> Either NewScriptRegistryException (TxIn, TxOut CtxUTxO)
  lookupScriptHash name sh m =
    case lookup sh m of
      Nothing -> Left $ MissingScript name sh (Map.keysSet m)
      Just s -> Right s

  HydraScriptCatalogue
    { initialScriptHash
    , commitScriptHash
    , headScriptHash
    } = hydraScriptCatalogue

-- | Get the UTxO that corresponds to a script registry.
--
-- **Property**:
--
--     newScriptRegistry (registryUTxO r) === Just r
registryUTxO :: ScriptRegistry -> UTxO
registryUTxO scriptRegistry =
  UTxO.fromList [initialReference, commitReference, headReference]
 where
  ScriptRegistry
    { initialReference
    , commitReference
    , headReference
    } = scriptRegistry
