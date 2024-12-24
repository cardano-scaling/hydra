module Hydra.Tx.Deposit where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (bodyTxL, inputsTxBodyL, outputsTxBodyL)
import Control.Lens ((.~), (^.))
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Hydra.Cardano.Api
import Hydra.Cardano.Api.Network (Network)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx (CommitBlueprintTx (..), HeadId, fromCurrencySymbol, headIdToCurrencySymbol)
import Hydra.Tx.Utils (addMetadata, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (POSIXTime)

-- * Construction

-- | Builds a deposit transaction to lock funds into the v_deposit script.
depositTx ::
  NetworkId ->
  HeadId ->
  CommitBlueprintTx Tx ->
  -- | Deposit deadline
  UTCTime ->
  Tx
depositTx networkId headId commitBlueprintTx deadline =
  fromLedgerTx $
    toLedgerTx blueprintTx
      & addDepositInputs
      & bodyTxL . outputsTxBodyL .~ StrictSeq.singleton (toLedgerTxOut depositOutput)
      & addMetadata (mkHydraHeadV1TxName "DepositTx") blueprintTx
 where
  addDepositInputs tx =
    let newInputs = tx ^. bodyTxL . inputsTxBodyL <> Set.fromList (toLedgerTxIn . fst <$> depositInputs)
     in tx & bodyTxL . inputsTxBodyL .~ newInputs

  CommitBlueprintTx{lookupUTxO = depositUTxO, blueprintTx} = commitBlueprintTx

  depositInputsList = toList (UTxO.inputSet depositUTxO)

  depositInputs = (,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> depositInputsList

  depositValue = foldMap txOutValue depositUTxO

  depositScript = fromPlutusScript @PlutusScriptV3 depositValidatorScript

  deposits = mapMaybe Commit.serializeCommit $ UTxO.pairs depositUTxO

  depositPlutusDatum = Deposit.datum (headIdToCurrencySymbol headId, posixFromUTCTime deadline, deposits)

  depositDatum = mkTxOutDatumInline depositPlutusDatum

  depositOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV3 networkId depositScript)
      depositValue
      depositDatum
      ReferenceScriptNone

depositAddress :: NetworkId -> AddressInEra
depositAddress networkId = mkScriptAddress @PlutusScriptV3 networkId (fromPlutusScript @PlutusScriptV3 depositValidatorScript)

-- * Observation

data DepositObservation = DepositObservation
  { headId :: HeadId
  , deposited :: UTxO
  , depositTxId :: TxId
  , deadline :: POSIXTime
  }
  deriving stock (Show, Eq, Generic)

observeDepositTx ::
  NetworkId ->
  Tx ->
  Maybe DepositObservation
observeDepositTx networkId tx = do
  -- TODO: could just use the first output and fail otherwise
  (TxIn depositTxId _, depositOut) <- findTxOutByAddress (depositAddress networkId) tx
  (headId, deposited, deadline) <- observeDepositTxOut (networkIdToNetwork networkId) (toUTxOContext depositOut)
  if all (`elem` txIns' tx) (UTxO.inputSet deposited)
    then
      Just
        DepositObservation
          { headId
          , deposited
          , depositTxId
          , deadline
          }
    else Nothing

observeDepositTxOut :: Network -> TxOut CtxUTxO -> Maybe (HeadId, UTxO, POSIXTime)
observeDepositTxOut network depositOut = do
  dat <- case txOutDatum depositOut of
    TxOutDatumInline d -> pure d
    _ -> Nothing
  (headCurrencySymbol, deadline, onChainDeposits) <- fromScriptData dat
  deposit <- do
    depositedUTxO <- traverse (Commit.deserializeCommit network) onChainDeposits
    pure . UTxO.fromPairs $ depositedUTxO
  headId <- fromCurrencySymbol headCurrencySymbol
  pure (headId, deposit, deadline)
