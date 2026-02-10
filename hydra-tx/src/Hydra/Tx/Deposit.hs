module Hydra.Tx.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras.Time (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx (CommitBlueprintTx (..), HeadId, currencySymbolToHeadId, headIdToCurrencySymbol, txId)
import Hydra.Tx.Utils (addMetadata, mkHydraHeadV1TxName)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-api" Cardano.Ledger.Api (AllegraEraTxBody (vldtTxBodyL), ValidityInterval (..), bodyTxL, outputsTxBodyL)
import "cardano-strict-containers" Data.Maybe.Strict (StrictMaybe (..))
import "cardano-strict-containers" Data.Sequence.Strict qualified as StrictSeq
import "lens" Control.Lens ((.~))
import "plutus-ledger-api" PlutusLedgerApi.V3 (POSIXTime)

-- * Construction

-- | Builds a deposit transaction to lock funds into the v_deposit script.
depositTx ::
  NetworkId ->
  PParams LedgerEra ->
  HeadId ->
  CommitBlueprintTx Tx ->
  -- | Slot to use as upper validity. Will mark the time of creation of the deposit.
  SlotNo ->
  -- | Deposit deadline from which onward the deposit can be recovered.
  UTCTime ->
  Maybe AddressInEra ->
  Tx
depositTx networkId pparams headId commitBlueprintTx upperSlot deadline changeAddress =
  let blueprint =
        case txOuts' blueprintTx of
          [] ->
            -- When blueprint tx doesn't contain any outputs we just construct outputs taking the whole of lookupUTxO
            toLedgerTx blueprintTx
              & bodyTxL . outputsTxBodyL
                .~ StrictSeq.singleton (toLedgerTxOut $ mkDepositOutput networkId headId lookupUTxO deadline)
          outs ->
            case changeAddress of
              Nothing ->
                -- In case change address is not specified we expect to see a fully balanced blueprint tx so we
                -- just take all the outputs and replace the `TxIn` to the blueprint one.
                toLedgerTx blueprintTx
                  & bodyTxL . outputsTxBodyL
                    .~ StrictSeq.singleton (toLedgerTxOut $ mkDepositOutput networkId headId (constructDepositUTxO (getTxId $ getTxBody blueprintTx) outs) deadline)
              Just addr ->
                -- When change address is specified we balance the blueprint tx ourselves adding the change output to return to the user.
                let depositOutput =
                      toLedgerTxOut $
                        mkDepositOutput networkId headId (constructDepositUTxO (getTxId $ getTxBody blueprintTx) outs) deadline

                    balance = evaluateTransactionBalance shelleyBasedEra pparams mempty mempty mempty

                    partialTx =
                      fromLedgerTx $
                        toLedgerTx blueprintTx
                          & bodyTxL . outputsTxBodyL .~ StrictSeq.singleton depositOutput

                    completeUTxO = resolveInputsUTxO lookupUTxO blueprintTx

                    leftoverValue = txOutValueToValue $ balance completeUTxO (getTxBody partialTx)

                    changeOutput = toLedgerTxOut $ TxOut addr leftoverValue TxOutDatumNone ReferenceScriptNone
                 in toLedgerTx partialTx
                      & bodyTxL . outputsTxBodyL
                        .~ StrictSeq.fromList [depositOutput, changeOutput]
   in fromLedgerTx $
        blueprint
          & bodyTxL . vldtTxBodyL .~ ValidityInterval{invalidBefore = SNothing, invalidHereafter = SJust upperSlot}
          & addMetadata (mkHydraHeadV1TxName "DepositTx") blueprintTx
 where
  CommitBlueprintTx{lookupUTxO, blueprintTx} = commitBlueprintTx

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
  depositValue = UTxO.totalValue depositUTxO

  deposits = mapMaybe Commit.serializeCommit $ UTxO.toList depositUTxO

  depositPlutusDatum = Deposit.datum (headIdToCurrencySymbol headId, posixFromUTCTime deadline, deposits)

  depositDatum = mkTxOutDatumInline depositPlutusDatum

constructDepositUTxO :: TxId -> [TxOut CtxTx] -> UTxO
constructDepositUTxO txid outputs =
  UTxO.fromList $ (\(txOut, n) -> (TxIn txid (TxIx n), toCtxUTxOTxOut txOut)) <$> zip outputs [0 .. fromIntegral (length outputs)]

depositAddress :: NetworkId -> AddressInEra
depositAddress networkId = mkScriptAddress networkId depositValidatorScript

-- * Observation

data DepositObservation = DepositObservation
  { headId :: HeadId
  , depositTxId :: TxId
  , deposited :: UTxO
  , created :: SlotNo
  , deadline :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Observe a deposit transaction by decoding the target head id, deposit
-- deadline and deposited utxo in the datum.
--
-- This includes checking whether
-- - the first output is a deposit output
-- - all of deposited value is contained in the deposit tx output,
-- - the deposit script output actually contains the deposited value,
-- - an upper validity bound has been set (used as creation slot).
observeDepositTx ::
  NetworkId ->
  Tx ->
  Maybe DepositObservation
observeDepositTx networkId tx = do
  (_, depositOut) <- findTxOutByAddress (depositAddress networkId) tx
  (headId, deposited, deadline) <- observeDepositTxOut network (toCtxUTxOTxOut depositOut)
  created <- getUpperBound
  pure
    DepositObservation
      { headId
      , depositTxId = Hydra.Tx.txId tx
      , deposited
      , created
      , deadline = posixToUTCTime deadline
      }
 where
  getUpperBound =
    case tx & getTxBody & getTxBodyContent & txValidityUpperBound of
      TxValidityUpperBound{upperBound} -> Just upperBound
      TxValidityNoUpperBound -> Nothing

  network = toShelleyNetwork networkId

observeDepositTxOut :: Network -> TxOut CtxUTxO -> Maybe (HeadId, UTxO, POSIXTime)
observeDepositTxOut network depositOut = do
  dat <- case txOutDatum depositOut of
    TxOutDatumInline d -> pure d
    _ -> Nothing
  (headCurrencySymbol, deadline, onChainDeposits) <- fromScriptData dat
  headId <- currencySymbolToHeadId headCurrencySymbol
  deposit <- do
    depositedUTxO <- UTxO.fromList <$> traverse (Commit.deserializeCommit network) onChainDeposits
    guard $ depositValue == UTxO.totalValue depositedUTxO
    pure depositedUTxO
  pure (headId, deposit, deadline)
 where
  depositValue = txOutValue depositOut
