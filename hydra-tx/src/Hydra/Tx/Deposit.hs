module Hydra.Tx.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (AllegraEraTxBody (vldtTxBodyL), ValidityInterval (..), bodyTxL, outputsTxBodyL)
import Control.Lens ((.~))
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict qualified as StrictSeq
import GHC.IsList qualified as IsList
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras.Time (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx (CommitBlueprintTx (..), HeadId, currencySymbolToHeadId, headIdToCurrencySymbol, txId)
import Hydra.Tx.Utils (addMetadata, mkHydraHeadV2TxName)
import PlutusLedgerApi.V3 (POSIXTime)

-- * Construction

-- | Builds a deposit transaction to lock funds into the v_deposit script.
depositTx ::
  HasCallStack =>
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

                    leftoverValue = capNegative . txOutValueToValue $ balance completeUTxO (getTxBody partialTx)

                    capNegative =
                      fromList . map (second (max 0)) . IsList.toList

                    changeOutput = toLedgerTxOut $ TxOut addr leftoverValue TxOutDatumNone ReferenceScriptNone
                 in toLedgerTx partialTx
                      & bodyTxL . outputsTxBodyL
                        .~ StrictSeq.fromList [depositOutput, changeOutput]
   in fromLedgerTx $
        blueprint
          & bodyTxL . vldtTxBodyL .~ ValidityInterval{invalidBefore = SNothing, invalidHereafter = SJust upperSlot}
          & addMetadata (mkHydraHeadV2TxName "DepositTx") blueprintTx
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
-- - the transaction's first output is the deposit output
-- - all of deposited value is contained in the deposit tx output,
-- - the deposit script output actually contains the deposited value,
-- - an upper validity bound has been set (used as creation slot).
observeDepositTx ::
  NetworkId ->
  Tx ->
  Maybe DepositObservation
observeDepositTx networkId tx = do
  (TxIn _ depositIx, depositOut) <- findTxOutByAddress (depositAddress networkId) tx
  -- A deposit is identified by its transaction id alone, which only works
  -- because it is that transaction's first output: 'depositTx' builds it there,
  -- 'recoverTx' spends @TxIn depositTxId (TxIx 0)@, and the increment validator
  -- requires it. Observing one at any other index would let parties sign a
  -- snapshot committing a deposit that can then neither be claimed nor
  -- recovered.
  guard $ depositIx == TxIx 0
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
    depositedUTxO <- UTxO.fromList <$> traverse deserializeRoundTripping onChainDeposits
    -- TODO: This silently ignores deposits that deposit less ADA than what the
    -- min ADA for the deposit output would be. For example: a 1 ADA utxo can be
    -- deposited, but the deposit tx's output will require ~1.5 ADA because of
    -- the inline datum on it. Dropping this or changing to a >= here will not
    -- work because the increment redeemer of the head validator requires an
    -- exact balance (right now).
    guard $ depositValue == UTxO.totalValue depositedUTxO
    pure depositedUTxO
  pure (headId, deposit, deadline)
 where
  depositValue = txOutValue depositOut

  -- The validators hash the datum's 'preSerializedOutput' bytes as they stand,
  -- while the off-chain representation cannot express everything those bytes can:
  -- 'fromPlutusTxOut' drops a reference script, for instance. A deposit whose
  -- commits do not survive the round trip would still be observed and committed by
  -- a snapshot, but every hash recomputed from the off-chain UTxO would differ from
  -- the datum's — leaving it neither claimable by an increment nor recoverable,
  -- since 'recoverTx' rebuilds its outputs through the same lossy path. Refuse to
  -- observe such a deposit; the funds stay recoverable by a transaction that
  -- reproduces the original outputs exactly.
  deserializeRoundTripping commit = do
    (i, o) <- Commit.deserializeCommit network commit
    guard $ Commit.serializeCommit (i, o) == Just commit
    pure (i, o)
