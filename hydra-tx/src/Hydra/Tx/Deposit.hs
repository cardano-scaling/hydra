module Hydra.Tx.Deposit where

import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (bodyTxL, inputsTxBodyL, outputsTxBodyL)
import Control.Lens ((.~), (^.))
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Cardano.Api
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx (CommitBlueprintTx (..), HeadId, currencySymbolToHeadId, headIdToCurrencySymbol)
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
      & bodyTxL . outputsTxBodyL .~ StrictSeq.singleton (toLedgerTxOut $ mkDepositOutput networkId headId depositUTxO deadline)
      & addMetadata (mkHydraHeadV1TxName "DepositTx") blueprintTx
 where
  addDepositInputs tx =
    let newInputs = tx ^. bodyTxL . inputsTxBodyL <> Set.fromList (toLedgerTxIn . fst <$> depositInputs)
     in tx & bodyTxL . inputsTxBodyL .~ newInputs

  CommitBlueprintTx{lookupUTxO = depositUTxO, blueprintTx} = commitBlueprintTx

  depositInputsList = toList (UTxO.inputSet depositUTxO)

  depositInputs = (,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> depositInputsList

mkDepositOutput ::
  NetworkId ->
  HeadId ->
  UTxO ->
  UTCTime ->
  TxOut ctx
mkDepositOutput networkId headId depositUTxO deadline =
  TxOut
    (depositAddress networkId)
    depositValue
    depositDatum
    ReferenceScriptNone
 where
  depositValue = foldMap txOutValue depositUTxO

  deposits = mapMaybe Commit.serializeCommit $ UTxO.toList depositUTxO

  depositPlutusDatum = Deposit.datum (headIdToCurrencySymbol headId, posixFromUTCTime deadline, deposits)

  depositDatum = mkTxOutDatumInline depositPlutusDatum

depositAddress :: NetworkId -> AddressInEra
depositAddress networkId = mkScriptAddress networkId depositValidatorScript

-- * Observation

data DepositObservation = DepositObservation
  { headId :: HeadId
  , deposited :: UTxO
  , depositTxId :: TxId
  , deadline :: POSIXTime
  }
  deriving stock (Show, Eq, Generic)

-- | Observe a deposit transaction by decoding the target head id, deposit
-- deadline and deposited utxo in the datum.
--
-- This includes checking whether
-- - all inputs of deposited utxo are actually spent,
-- - the deposit script output actually contains the deposited value.
observeDepositTx ::
  NetworkId ->
  Tx ->
  Maybe DepositObservation
observeDepositTx networkId tx = do
  -- TODO: could just use the first output and fail otherwise
  (TxIn depositTxId _, depositOut) <- findTxOutByAddress (depositAddress networkId) tx
  (headId, deposited, deadline) <- observeDepositTxOut (toShelleyNetwork networkId) (toCtxUTxOTxOut depositOut)
  guard $ all (`elem` txIns' tx) (UTxO.inputSet deposited)
  pure
    DepositObservation
      { headId
      , deposited
      , depositTxId
      , deadline
      }

observeDepositTxOut :: Network -> TxOut CtxUTxO -> Maybe (HeadId, UTxO, POSIXTime)
observeDepositTxOut network depositOut = do
  dat <- case txOutDatum depositOut of
    TxOutDatumInline d -> pure d
    _ -> Nothing
  (headCurrencySymbol, deadline, onChainDeposits) <- fromScriptData dat
  headId <- currencySymbolToHeadId headCurrencySymbol
  deposit <- do
    depositedUTxO <- UTxO.fromList <$> traverse (Commit.deserializeCommit network) onChainDeposits
    guard $ depositValue `containsValue` foldMap txOutValue depositedUTxO
    pure depositedUTxO
  pure (headId, deposit, deadline)
 where
  depositValue = txOutValue depositOut
