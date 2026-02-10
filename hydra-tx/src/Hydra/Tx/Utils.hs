module Hydra.Tx.Utils (
  module Hydra.Tx.Utils,
  dummyValidatorScript,
) where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude hiding (toList)

import "base" GHC.IsList (IsList (..))
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Tx qualified as Ledger
import "cardano-ledger-api" Cardano.Ledger.Api (AlonzoTxAuxData (..), auxDataHashTxBodyL, auxDataTxL, bodyTxL, hashTxAuxData)
import "cardano-strict-containers" Data.Maybe.Strict (StrictMaybe (..))
import "containers" Data.Map.Strict qualified as Map
import "hydra-plutus" Hydra.Contract.Dummy (dummyValidatorScript)
import "hydra-plutus" Hydra.Contract.Util (hydraHeadV1)
import "hydra-tx" Hydra.Tx.HeadId (HeadId, mkHeadId)
import "hydra-tx" Hydra.Tx.OnChainId (OnChainId (..))
import "lens" Control.Lens ((.~), (^.))
import "ouroboros-consensus-cardano" Ouroboros.Consensus.Shelley.Eras qualified as Ledger
import "plutus-ledger-api" PlutusLedgerApi.V3 (fromBuiltin, getPubKeyHash)

hydraHeadV1AssetName :: AssetName
hydraHeadV1AssetName = UnsafeAssetName (fromBuiltin hydraHeadV1)

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
assetNameToOnChainId (UnsafeAssetName bs) = UnsafeOnChainId bs

onChainIdToAssetName :: OnChainId -> AssetName
onChainIdToAssetName = UnsafeAssetName . serialiseToRawBytes

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)

-- | Derive the 'OnChainId' from a Cardano 'PaymentKey'. The on-chain identifier
-- is the public key hash as it is also available to plutus validators.
verificationKeyToOnChainId :: VerificationKey PaymentKey -> OnChainId
verificationKeyToOnChainId =
  UnsafeOnChainId . fromBuiltin . getPubKeyHash . toPlutusKeyHash . verificationKeyHash

headTokensFromValue :: PlutusScript -> Value -> PolicyAssets
headTokensFromValue headTokenScript v =
  fromList $
    [ (assetName, q)
    | (AssetId pid assetName, q) <- toList v
    , pid == scriptPolicyId (PlutusScript headTokenScript)
    ]

-- | Split a given UTxO into two, such that the second UTxO is non-empty. This
-- is useful to pick a UTxO to decommit.
splitUTxO :: UTxO -> (UTxO, UTxO)
splitUTxO utxo =
  case UTxO.toList utxo of
    [] -> (mempty, mempty)
    ((u, o) : us) -> (UTxO.fromList us, UTxO.singleton u o)

adaOnly :: TxOut CtxUTxO -> TxOut CtxUTxO
adaOnly = \case
  TxOut addr value datum refScript ->
    TxOut addr (lovelaceToValue $ selectLovelace value) datum refScript

addMetadata :: TxMetadata -> Tx -> Ledger.AlonzoTx Ledger.ConwayEra -> Ledger.AlonzoTx Ledger.ConwayEra
addMetadata (TxMetadata newMetadata) blueprintTx tx =
  let
    newMetadataMap = toShelleyMetadata newMetadata
    newAuxData =
      case toLedgerTx blueprintTx ^. auxDataTxL of
        SNothing -> AlonzoTxAuxData newMetadataMap mempty mempty
        SJust (AlonzoTxAuxData metadata timeLocks languageMap) ->
          AlonzoTxAuxData (Map.union metadata newMetadataMap) timeLocks languageMap
   in
    tx
      & auxDataTxL .~ SJust newAuxData
      & bodyTxL . auxDataHashTxBodyL .~ SJust (hashTxAuxData newAuxData)

-- | Type to encapsulate one of the two possible incremental actions or a
-- regular snapshot. This actually signals that our snapshot modeling is likely
-- not ideal but for now we want to keep track of both fields (de/commit) since
-- we might want to support batch de/commits too in the future, but having both fields
-- be Maybe UTxO introduces a lot of checks if the value is Nothing or mempty.
data IncrementalAction = ToCommit UTxO | ToDecommit UTxO | NoThing deriving (Eq, Show)

setIncrementalActionMaybe :: Maybe UTxO -> Maybe UTxO -> Maybe IncrementalAction
setIncrementalActionMaybe utxoToCommit utxoToDecommit =
  case (utxoToCommit, utxoToDecommit) of
    (Just _, Just _) -> Nothing
    (Just _, Nothing) ->
      ToCommit <$> utxoToCommit
    (Nothing, Just _) -> ToDecommit <$> utxoToDecommit
    (Nothing, Nothing) -> Just NoThing

-- | Find (if it exists) the head identifier contained in given `TxOut`.
findStateToken :: TxOut ctx -> Maybe HeadId
findStateToken =
  fmap (mkHeadId . fst) . findHeadAssetId

findHeadAssetId :: TxOut ctx -> Maybe (PolicyId, AssetName)
findHeadAssetId txOut =
  flip findFirst (toList $ txOutValue txOut) $ \case
    (AssetId pid aname, q)
      | aname == hydraHeadV1AssetName && q == 1 ->
          Just (pid, aname)
    _ ->
      Nothing
