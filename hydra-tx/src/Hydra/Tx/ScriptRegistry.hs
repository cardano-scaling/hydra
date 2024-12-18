-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Tx.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Hydra.Cardano.Api
import Hydra.Contract (ScriptInfo (..), scriptInfo)
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.Head qualified as Head
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import PlutusLedgerApi.Common (SerialisedScript)

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

  lookupScriptHash name sh m =
    case lookup sh m of
      Nothing -> Left $ MissingScript name sh (Map.keysSet m)
      Just s -> Right s

  ScriptInfo
    { initialScriptHash
    , commitScriptHash
    , headScriptHash
    } = scriptInfo

data SerialisedScriptRegistry = SerialisedScriptRegistry
  { initialScriptValidator :: SerialisedScript
  , commitScriptValidator :: SerialisedScript
  , headScriptValidator :: SerialisedScript
  , depositScriptValidator :: SerialisedScript
  }

serialisedScriptRegistry :: SerialisedScriptRegistry
serialisedScriptRegistry =
  SerialisedScriptRegistry
    { initialScriptValidator = initialValidatorScript
    , commitScriptValidator = commitValidatorScript
    , headScriptValidator = Head.validatorScript
    , depositScriptValidator = Deposit.validatorScript
    }

-- XXX: used to parse Aiken `compiledCode`.
serialisedScriptFromText :: Text -> SerialisedScript
serialisedScriptFromText base16Text =
  case Base16.decode base16Bytes of
    Left e -> error $ "Failed to decode initial validator: " <> show e
    Right bytes -> toShort bytes
 where
  base16Bytes = encodeUtf8 base16Text

-- XXX: used to parse Plutus `cborHex`.
cborHexToSerialisedScript :: ByteString -> SerialisedScript
cborHexToSerialisedScript cborHex = either (error . show) SBS.toShort $ do
  bytes <- Base16.decode cborHex
  (_, a) <- first show $ CBOR.deserialiseFromBytes @ByteString fromCBOR (LBS.fromStrict bytes)
  pure a

-- | Get the UTxO that corresponds to a script registry.
--
-- **Property**:
--
--     newScriptRegistry (registryUTxO r) === Just r
registryUTxO :: ScriptRegistry -> UTxO
registryUTxO scriptRegistry =
  UTxO.fromPairs [initialReference, commitReference, headReference]
 where
  ScriptRegistry
    { initialReference
    , commitReference
    , headReference
    } = scriptRegistry
