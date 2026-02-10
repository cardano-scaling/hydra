module Hydra.Tx.Commit where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude

import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Core (AlonzoEraTxBody, AlonzoEraTxWits, AsIxItem (..))
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Scripts (PlutusPurpose)
import "cardano-ledger-api" Cardano.Ledger.Api (
  AsIx (..),
  ConwayPlutusPurpose (..),
  Redeemers (..),
  bodyTxL,
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
import "cardano-ledger-core" Cardano.Ledger.BaseTypes (StrictMaybe (..))
import "cardano-ledger-core" Cardano.Ledger.Core qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Plutus.Data (Data)
import "cardano-ledger-core" Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import "cardano-ledger-core" Cardano.Ledger.TxIn qualified as Ledger
import "cardano-strict-containers" Data.Sequence.Strict qualified as StrictSeq
import "containers" Data.Map qualified as Map
import "containers" Data.Set qualified as Set
import "hydra-plutus" Hydra.Contract.Commit qualified as Commit
import "hydra-plutus" Hydra.Contract.Initial qualified as Initial
import "hydra-plutus" Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import "hydra-tx" Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import "hydra-tx" Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol, mkHeadId)
import "hydra-tx" Hydra.Tx.Party (Party, partyFromChain, partyToChain)
import "hydra-tx" Hydra.Tx.ScriptRegistry (ScriptRegistry, initialReference)
import "hydra-tx" Hydra.Tx.Utils (addMetadata, mkHydraHeadV1TxName)
import "lens" Control.Lens ((.~), (<>~), (^.))
import "plutus-ledger-api" PlutusLedgerApi.V3 (CurrencySymbol)
import "plutus-ledger-api" PlutusLedgerApi.V3 qualified as Plutus

-- * Construction

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
      & bodyTxL . Cardano.Ledger.Api.mintTxBodyL .~ mempty
      & addMetadata (mkHydraHeadV1TxName "CommitTx") blueprintTx
 where
  spendFromInitial tx =
    let newRedeemers =
          resolveSpendingRedeemers tx
            & Map.insert (toLedgerTxIn initialInput) (toLedgerData @LedgerEra initialRedeemer)
        newInputs = tx ^. bodyTxL . inputsTxBodyL <> Set.singleton (toLedgerTxIn initialInput)
     in tx
          & bodyTxL . inputsTxBodyL .~ newInputs
          & bodyTxL . Cardano.Ledger.Api.referenceInputsTxBodyL <>~ Set.singleton (toLedgerTxIn initialScriptRef)
          & bodyTxL . Cardano.Ledger.Api.reqSignerHashesTxBodyL <>~ Set.singleton (toLedgerKeyHash vkh)
          & witsTxL . rdmrsTxWitsL
            .~ Redeemers (fromList $ nonSpendingRedeemers tx)
              <> Redeemers (fromList $ mkRedeemers newRedeemers newInputs)

  -- Make redeemers (with zeroed units) from a TxIn -> Data map and a set of transaction inputs
  mkRedeemers :: Ord k => Map k a -> Set k -> [(ConwayPlutusPurpose AsIx era, (a, ExUnits))]
  mkRedeemers resolved inputs =
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
  resolveSpendingRedeemers ::
    (AlonzoEraTxBody era, AlonzoEraTxWits era, Ledger.EraTx era, PlutusPurpose AsIxItem era ~ ConwayPlutusPurpose AsIxItem era) =>
    Ledger.Tx era ->
    Map Ledger.TxIn (Data era)
  resolveSpendingRedeemers tx =
    Map.foldMapWithKey
      ( \p (d, _ex) ->
          -- XXX: Should soon be available through cardano-ledger-api again
          case redeemerPointerInverse (tx ^. bodyTxL) p of
            SJust (ConwaySpending (AsIxItem _ txIn)) -> Map.singleton txIn d
            _ -> mempty
      )
      (unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL)

  nonSpendingRedeemers ::
    (AlonzoEraTxBody era, AlonzoEraTxWits era, Ledger.EraTx era, PlutusPurpose AsIxItem era ~ ConwayPlutusPurpose AsIxItem era) =>
    Ledger.Tx era ->
    [(ConwayPlutusPurpose AsIx era2, (Data era, ExUnits))]
  nonSpendingRedeemers tx =
    Map.foldMapWithKey
      ( \p (d, ex) ->
          case redeemerPointerInverse (tx ^. bodyTxL) p of
            SJust (ConwayMinting (AsIxItem i _)) -> [(ConwayMinting (AsIx i), (d, ex))]
            SJust (ConwayRewarding (AsIxItem i _)) -> [(ConwayRewarding (AsIx i), (d, ex))]
            SJust (ConwayCertifying (AsIxItem i _)) -> [(ConwayCertifying (AsIx i), (d, ex))]
            SJust (ConwayProposing (AsIxItem i _)) -> [(ConwayProposing (AsIx i), (d, ex))]
            SJust (ConwayVoting (AsIxItem i _)) -> [(ConwayVoting (AsIx i), (d, ex))]
            SJust (ConwaySpending (AsIxItem _ _)) -> []
            SNothing -> []
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

  commitAddress =
    mkScriptAddress networkId commitValidatorScript

  utxoToCommit =
    UTxO.fromList $ mapMaybe (\txin -> (txin,) <$> UTxO.resolveTxIn txin lookupUTxO) committedTxIns

  commitValue =
    txOutValue out <> UTxO.totalValue utxoToCommit

  commitDatum =
    mkTxOutDatumInline $ mkCommitDatum party utxoToCommit (headIdToCurrencySymbol headId)

  CommitBlueprintTx{lookupUTxO, blueprintTx} = commitBlueprintTx

mkCommitDatum :: Party -> UTxO -> CurrencySymbol -> Plutus.Datum
mkCommitDatum party utxo headId =
  Commit.datum (partyToChain party, commits, headId)
 where
  commits =
    mapMaybe Commit.serializeCommit $ UTxO.toList utxo

-- * Observation

-- | Full observation of a commit transaction.
data CommitObservation = CommitObservation
  { party :: Party
  -- ^ Hydra participant who committed the UTxO.
  , committed :: UTxO
  , headId :: HeadId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a commit tx by:
--
-- - Check that its spending from the init validator,
-- - Find the outputs which pays to the commit validator,
-- - Using the datum of that output, deserialize the committed output,
-- - Reconstruct the committed UTxO from both values (tx input and output).
observeCommitTx ::
  NetworkId ->
  -- | A UTxO set to lookup tx inputs. Should at least contain the input
  -- spending from νInitial.
  UTxO ->
  Tx ->
  Maybe CommitObservation
observeCommitTx networkId utxo tx = do
  guard isSpendingFromInitial

  (_, commitOut) <- findTxOutByAddress commitAddress tx
  dat <- txOutScriptData commitOut
  (onChainParty, onChainCommits, headId) :: Commit.DatumType <- fromScriptData dat
  party <- partyFromChain onChainParty

  -- NOTE: If we have the resolved inputs (utxo) then we could avoid putting
  -- the commit into the datum (+ changing the hashing strategy of
  -- collect/fanout)
  committed <- do
    committedUTxO <- traverse (Commit.deserializeCommit (toShelleyNetwork networkId)) onChainCommits
    pure . UTxO.fromList $ committedUTxO

  policyId <- fromPlutusCurrencySymbol headId
  pure
    CommitObservation
      { party
      , committed
      , headId = mkHeadId policyId
      }
 where
  isSpendingFromInitial :: Bool
  isSpendingFromInitial =
    any (\o -> txOutAddress o == initialAddress) (UTxO.txOutputs $ resolveInputsUTxO utxo tx)

  initialAddress = mkScriptAddress networkId initialValidatorScript

  commitAddress = mkScriptAddress networkId commitValidatorScript
