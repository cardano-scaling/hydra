module Hydra.Tx.Utils where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Core (auxDataHashTxBodyL, auxDataTxL, bodyTxL, inputsTxBodyL, mkBasicTx)
import Cardano.Ledger.Alonzo.Tx qualified as Ledger
import Cardano.Ledger.Api (AlonzoTxAuxData (..), hashTxAuxData, mkBasicTxBody)
import Control.Lens ((.~), (^.))
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set qualified as Set
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Tx.OnChainId (OnChainId (..))
import Ouroboros.Consensus.Shelley.Eras qualified as Ledger
import PlutusLedgerApi.V2 (fromBuiltin, getPubKeyHash)
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
  | (AssetId pid assetName, q) <- valueToList v
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

-- | Create a transaction spending all given `UTxO`.
txSpendingUTxO :: UTxO -> Tx
txSpendingUTxO utxo =
  fromLedgerTx $
    mkBasicTx
      ( mkBasicTxBody
          & inputsTxBodyL .~ (toLedgerTxIn `Set.map` inputs)
      )
 where
  inputs = UTxO.inputSet utxo
