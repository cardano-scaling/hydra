module Hydra.Tx.Deposit where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api
import Hydra.Cardano.Api.Network (Network)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Ledger.Cardano.Builder (
  addInputs,
  addOutputs,
  emptyTxBody,
  unsafeBuildTransaction,
 )
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx (HeadId, fromCurrencySymbol, headIdToCurrencySymbol)
import Hydra.Tx.Utils (mkHydraHeadV1TxName)

-- * Construction

-- | Builds a deposit transaction to lock funds into the v_deposit script.
depositTx ::
  NetworkId ->
  HeadId ->
  -- | UTxO to deposit
  UTxO ->
  -- | Deposit deadline
  UTCTime ->
  Tx
depositTx networkId headId depositUTxO deadline =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs depositInputs
      & addOutputs [depositOutput]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "DepositTx")
 where
  depositInputsList = toList (UTxO.inputSet depositUTxO)

  depositInputs = (,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> depositInputsList

  depositValue = foldMap txOutValue depositUTxO

  depositScript = fromPlutusScript @PlutusScriptV2 Deposit.validatorScript

  deposits = mapMaybe Commit.serializeCommit $ UTxO.pairs depositUTxO

  depositPlutusDatum = Deposit.datum $ Deposit.DepositDatum (headIdToCurrencySymbol headId, posixFromUTCTime deadline, deposits)

  depositDatum = mkTxOutDatumInline depositPlutusDatum

  depositOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV2 networkId depositScript)
      depositValue
      depositDatum
      ReferenceScriptNone

-- * Observation

data DepositObservation = DepositObservation
  { headId :: HeadId
  , deposited :: UTxO
  , -- TODO: really needed?
    utxo :: UTxO
  }
  deriving stock (Show, Eq, Generic)

observeDepositTx ::
  NetworkId ->
  -- TODO: needs to also know the UTxO to resolve tx inputs
  Tx ->
  Maybe DepositObservation
observeDepositTx networkId tx = do
  -- TODO: could just use the first output and fail otherwise
  (depositIn, depositOut) <- findTxOutByAddress depositAddress tx
  -- FIXME: need to verify consistency of datum against tx inputs!
  observeDepositTxOut (networkIdToNetwork networkId) (toUTxOContext depositOut)
 where
  depositScript = fromPlutusScript Deposit.validatorScript

  depositAddress = mkScriptAddress @PlutusScriptV2 networkId depositScript

observeDepositTxOut :: Network -> TxOut CtxUTxO -> Maybe DepositObservation
observeDepositTxOut network depositOut = do
  dat <- case txOutDatum depositOut of
    TxOutDatumInline d -> pure d
    _ -> Nothing
  Deposit.DepositDatum (headCurrencySymbol, _deadline, onChainDeposits) <- fromScriptData dat
  deposit <- do
    depositedUTxO <- traverse (Commit.deserializeCommit network) onChainDeposits
    pure . UTxO.fromPairs $ depositedUTxO
  headId <- fromCurrencySymbol headCurrencySymbol
  pure
    DepositObservation
      { headId
      , deposited = deposit
      , -- FIXME: drop this
        utxo = undefined
      }
