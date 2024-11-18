module Hydra.Tx.Utils (
  module Hydra.Tx.Utils,
  dummyValidatorScript,
  schnorrkelValidatorScript,
) where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.Api (AlonzoTxAuxData (..), auxDataHashTxBodyL, auxDataTxL, bodyTxL, hashTxAuxData)
import Control.Lens ((.~), (^.))
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.IsList (IsList (..))
import Hydra.Contract.Dummy (dummyValidatorScript, schnorrkelValidatorScript)
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Tx.OnChainId (OnChainId (..))
import Ouroboros.Consensus.Shelley.Eras qualified as Ledger
import PlutusLedgerApi.V3 (fromBuiltin, getPubKeyHash)

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

addMetadata :: TxMetadata -> Tx -> Ledger.AlonzoTx (Ledger.ConwayEra StandardCrypto) -> Ledger.AlonzoTx (Ledger.ConwayEra StandardCrypto)
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
-- be Maybe UTxO intruduces a lot of checks if the value is Nothing or mempty.
data IncrementalAction = ToCommit UTxO | ToDecommit UTxO | NoThing deriving (Eq, Show)

setIncrementalActionMaybe :: Maybe UTxO -> Maybe UTxO -> Maybe IncrementalAction
setIncrementalActionMaybe utxoToCommit utxoToDecommit =
  case (utxoToCommit, utxoToDecommit) of
    (Just _, Just _) -> Nothing
    (Just _, Nothing) ->
      ToCommit <$> utxoToCommit
    (Nothing, Just _) -> ToDecommit <$> utxoToDecommit
    (Nothing, Nothing) -> Just NoThing
