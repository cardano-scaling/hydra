module Hydra.Tx.Commit where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Core (AsIxItem (..))
import Cardano.Ledger.Api (
  AlonzoTxAuxData (..),
  AsIx (..),
  ConwayPlutusPurpose (..),
  Redeemers (..),
  auxDataHashTxBodyL,
  auxDataTxL,
  bodyTxL,
  hashTxAuxData,
  inputsTxBodyL,
  mintTxBodyL,
  outputsTxBodyL,
  rdmrsTxWitsL,
  redeemerPointerInverse,
  referenceInputsTxBodyL,
  reqSignerHashesTxBodyL,
  unRedeemers,
  witsTxL,
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Control.Lens ((.~), (<>~), (^.))
import Data.Map qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Initial qualified as Initial
import Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.Party (Party, partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, initialReference)
import Hydra.Tx.Utils (mkHydraHeadV1TxName)
import PlutusLedgerApi.V2 (CurrencySymbol)
import PlutusLedgerApi.V2 qualified as Plutus

mkCommitDatum :: Party -> UTxO -> CurrencySymbol -> Plutus.Datum
mkCommitDatum party utxo headId =
  Commit.datum (partyToChain party, commits, headId)
 where
  commits =
    mapMaybe Commit.serializeCommit $ UTxO.pairs utxo

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
commitTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  HeadId ->
  Party ->
  CommitBlueprintTx Tx ->
  -- | The initial output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn, TxOut CtxUTxO, Hash PaymentKey) ->
  Tx
commitTx networkId scriptRegistry headId party commitBlueprintTx (initialInput, out, vkh) =
  -- NOTE: We use the cardano-ledger-api functions here such that we can use the
  -- blueprint transaction as a starting point (cardano-api does not allow
  -- convenient transaction modifications).
  fromLedgerTx $
    toLedgerTx blueprintTx
      & spendFromInitial
      & bodyTxL . outputsTxBodyL .~ StrictSeq.singleton (toLedgerTxOut commitOutput)
      & bodyTxL . reqSignerHashesTxBodyL <>~ Set.singleton (toLedgerKeyHash vkh)
      & bodyTxL . mintTxBodyL .~ mempty
      & addMetadata (mkHydraHeadV1TxName "CommitTx")
 where
  addMetadata (TxMetadata newMetadata) tx =
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

  spendFromInitial tx =
    let newRedeemers =
          resolveSpendingRedeemers tx
            & Map.insert (toLedgerTxIn initialInput) (toLedgerData @LedgerEra initialRedeemer)
        newInputs = tx ^. bodyTxL . inputsTxBodyL <> Set.singleton (toLedgerTxIn initialInput)
     in tx
          & bodyTxL . inputsTxBodyL .~ newInputs
          & bodyTxL . referenceInputsTxBodyL <>~ Set.singleton (toLedgerTxIn initialScriptRef)
          & witsTxL . rdmrsTxWitsL .~ mkRedeemers newRedeemers newInputs

  -- Make redeemers (with zeroed units) from a TxIn -> Data map and a set of transaction inputs
  mkRedeemers resolved inputs =
    Redeemers . Map.fromList $
      foldl'
        ( \newRedeemerData txin ->
            let ix = fromIntegral $ Set.findIndex txin inputs
             in case Map.lookup txin resolved of
                  Nothing -> newRedeemerData
                  Just d ->
                    (ConwaySpending (AsIx ix), (d, ExUnits 0 0)) : newRedeemerData
        )
        []
        inputs

  -- Create a TxIn -> Data map of all spending redeemers
  resolveSpendingRedeemers tx =
    Map.foldMapWithKey
      ( \p (d, _ex) ->
          -- XXX: Should soon be available through cardano-ledger-api again
          case redeemerPointerInverse (tx ^. bodyTxL) p of
            SJust (ConwaySpending (AsIxItem _ txIn)) -> Map.singleton txIn d
            _ -> mempty
      )
      (unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL)

  initialScriptRef =
    fst (initialReference scriptRegistry)

  initialRedeemer =
    toScriptData . Initial.redeemer $
      Initial.ViaCommit (toPlutusTxOutRef <$> committedTxIns)

  committedTxIns = txIns' blueprintTx

  commitOutput =
    TxOut commitAddress commitValue commitDatum ReferenceScriptNone

  commitScript =
    fromPlutusScript Commit.validatorScript

  commitAddress =
    mkScriptAddress @PlutusScriptV2 networkId commitScript

  utxoToCommit =
    UTxO.fromPairs $ mapMaybe (\txin -> (txin,) <$> UTxO.resolve txin lookupUTxO) committedTxIns

  commitValue =
    txOutValue out <> foldMap txOutValue utxoToCommit

  commitDatum =
    mkTxOutDatumInline $ mkCommitDatum party utxoToCommit (headIdToCurrencySymbol headId)

  CommitBlueprintTx{lookupUTxO, blueprintTx} = commitBlueprintTx
