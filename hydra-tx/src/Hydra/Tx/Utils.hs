module Hydra.Tx.Utils where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import GHC.IsList (IsList (..))
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Tx.OnChainId (OnChainId (..))
import PlutusLedgerApi.V2 (FromData, fromBuiltin, getPubKeyHash)
import Test.Cardano.Ledger.Babbage.Arbitrary ()

hydraHeadV1AssetName :: AssetName
hydraHeadV1AssetName = AssetName (fromBuiltin hydraHeadV1)

-- | The metadata label used for identifying Hydra protocol transactions. As
-- suggested by a friendly large language model: The number most commonly
-- associated with "Hydra" is 5, as in the mythological creature Hydra, which
-- had multiple heads, and the number 5 often symbolizes multiplicity or
-- diversity. However, there is no specific numerical association for Hydra
-- smaller than 10000 beyond this mythological reference.
hydraMetadataLabel :: Word64
hydraMetadataLabel = 55555

-- | Create a transaction metadata entry to identify Hydra transactions (for
-- informational purposes).
mkHydraHeadV1TxName :: Text -> TxMetadata
mkHydraHeadV1TxName name =
  TxMetadata $ Map.fromList [(hydraMetadataLabel, TxMetaText $ "HydraV1/" <> name)]

assetNameToOnChainId :: AssetName -> OnChainId
assetNameToOnChainId (AssetName bs) = UnsafeOnChainId bs

onChainIdToAssetName :: OnChainId -> AssetName
onChainIdToAssetName = AssetName . serialiseToRawBytes

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)

-- | Derive the 'OnChainId' from a Cardano 'PaymentKey'. The on-chain identifier
-- is the public key hash as it is also availble to plutus validators.
verificationKeyToOnChainId :: VerificationKey PaymentKey -> OnChainId
verificationKeyToOnChainId =
  UnsafeOnChainId . fromBuiltin . getPubKeyHash . toPlutusKeyHash . verificationKeyHash

headTokensFromValue :: PlutusScript -> Value -> [(AssetName, Quantity)]
headTokensFromValue headTokenScript v =
  [ (assetName, q)
  | (AssetId pid assetName, q) <- toList v
  , pid == scriptPolicyId (PlutusScript headTokenScript)
  ]

-- | Split a given UTxO into two, such that the second UTxO is non-empty. This
-- is useful to pick a UTxO to decommit.
splitUTxO :: UTxO -> (UTxO, UTxO)
splitUTxO utxo =
  case UTxO.pairs utxo of
    [] -> (mempty, mempty)
    (u : us) -> (UTxO.fromPairs us, UTxO.singleton u)

adaOnly :: TxOut CtxUTxO -> TxOut CtxUTxO
adaOnly = \case
  TxOut addr value datum refScript ->
    TxOut addr (lovelaceToValue $ selectLovelace value) datum refScript

-- | Extract the inline datum from a given 'TxOut'.
extractInlineDatumFromTxOut :: FromData a => TxOut CtxUTxO -> Maybe a
extractInlineDatumFromTxOut txout =
  let TxOut _ _ dat _ = txout
   in case dat of
        TxOutDatumInline d ->
          fromScriptData d
        _ -> Nothing
