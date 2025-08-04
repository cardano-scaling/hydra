module Hydra.Tx.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (AllegraEraTxBody (vldtTxBodyL), ValidityInterval (..), bodyTxL, inputsTxBodyL, outputsTxBodyL)
import Control.Lens ((.~), (^.))
import Data.List qualified as List
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras.Time (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx (CommitBlueprintTx (..), HeadId, currencySymbolToHeadId, headIdToCurrencySymbol, txId)
import Hydra.Tx.Utils (addMetadata, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (POSIXTime)

-- * Construction

-- | Builds a deposit transaction to lock funds into the v_deposit script.
depositTx ::
  NetworkId ->
  HeadId ->
  CommitBlueprintTx Tx ->
  -- | Slot to use as upper validity. Will mark the time of creation of the deposit.
  SlotNo ->
  -- | Deposit deadline from which onward the deposit can be recovered.
  UTCTime ->
  -- | Optional amount to create partial deposit
  Maybe Coin ->
  Tx
depositTx networkId headId commitBlueprintTx upperSlot deadline amount =
  fromLedgerTx $
    toLedgerTx blueprintTx
      & addDepositInputs
      & bodyTxL . outputsTxBodyL
        .~ ( StrictSeq.singleton (toLedgerTxOut $ mkDepositOutput networkId headId utxoToDeposit deadline)
              <> leftoverOutput
           )
      & bodyTxL . vldtTxBodyL .~ ValidityInterval{invalidBefore = SNothing, invalidHereafter = SJust upperSlot}
      & addMetadata (mkHydraHeadV1TxName "DepositTx") blueprintTx
 where
  addDepositInputs tx =
    let newInputs = tx ^. bodyTxL . inputsTxBodyL <> Set.fromList (toLedgerTxIn . fst <$> depositInputs)
     in tx & bodyTxL . inputsTxBodyL .~ newInputs

  CommitBlueprintTx{lookupUTxO = depositUTxO, blueprintTx} = commitBlueprintTx

  (utxoToDeposit, leftoverUTxO) = maybe (depositUTxO, mempty) (capUTxO depositUTxO) amount

  leftoverOutput =
    if UTxO.null leftoverUTxO
      then StrictSeq.empty
      else
        let leftoverAddress = List.head $ txOutAddress <$> UTxO.txOutputs leftoverUTxO
         in StrictSeq.singleton $
              toLedgerTxOut $
                TxOut leftoverAddress (UTxO.totalValue leftoverUTxO) TxOutDatumNone ReferenceScriptNone

  depositInputsList = toList (UTxO.inputSet utxoToDeposit)

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
  depositValue = UTxO.totalValue depositUTxO

  deposits = mapMaybe Commit.serializeCommit $ UTxO.toList depositUTxO

  depositPlutusDatum = Deposit.datum (headIdToCurrencySymbol headId, posixFromUTCTime deadline, deposits)

  depositDatum = mkTxOutDatumInline depositPlutusDatum

depositAddress :: NetworkId -> AddressInEra
depositAddress networkId = mkScriptAddress networkId depositValidatorScript

-- | Caps a UTxO set to a specified target amount of Lovelace, splitting outputs if necessary.
--
-- Given a 'UTxO' set and a target 'Coin' value (in Lovelace), this function selects unspent transaction outputs
-- (UTxOs) to form a subset that sums as close as possible to the target value without exceeding it. If an output
-- needs to be split to meet the target exactly, it creates two new outputs: one contributing to the target and
-- another for the remaining value. The function ensures that the total Lovelace in the selected outputs does not
-- exceed the target.
--
-- === Algorithm
-- 1. **Base Cases**:
--    - If the target is 0, return an empty 'UTxO' as the selected set and the original 'UTxO' as leftovers.
--    - If the input 'UTxO' is empty, return two empty 'UTxO' sets.
-- 2. **Main Logic**:
--    - Sort the UTxO entries by their Lovelace value (ascending) to prioritize smaller outputs for efficiency.
--    - Use a recursive helper function 'go' to iterate through the sorted UTxO list, accumulating outputs until
--      the target value is reached or no suitable outputs remain.
--    - For each output:
--      - If adding the output's Lovelace value does not exceed the target, include it fully in the selected set
--        and remove it from the leftovers.
--      - If adding the output would exceed the target, split the output into two parts:
--        - One part contributes exactly the remaining amount needed to reach the target.
--        - The other part holds the excess Lovelace.
--        - Reuse 'TxIn' identifiers for the split outputs.
--      - Continue processing until the target is met or no outputs remain.
-- 3. **Termination**:
--    - The function stops when the accumulated Lovelace equals the target or when no more outputs are available.
--    - Returns a pair of 'UTxO' sets: the selected outputs (summing to at most the target) and the remaining outputs.
capUTxO :: UTxO -> Coin -> (UTxO, UTxO)
capUTxO utxo target
  | target == 0 = (mempty, utxo)
  | UTxO.null utxo = (mempty, mempty)
  | otherwise = go mempty utxo 0 (sortBy (comparing (selectLovelace . txOutValue . snd)) (UTxO.toList utxo))
 where
  -- \| Helper function to recursively select and split UTxO outputs to reach the target value.
  go foundSoFar leftovers currentSum sorted
    | currentSum == target = (foundSoFar, leftovers)
    | otherwise = case sorted of
        [] -> (foundSoFar, leftovers)
        (txIn, txOut) : rest ->
          let x = selectLovelace (txOutValue txOut)
           in if currentSum + x <= target
                then
                  -- Include the entire output if it doesn't exceed the target.
                  go
                    (foundSoFar <> UTxO.singleton txIn txOut)
                    (UTxO.difference leftovers $ UTxO.singleton txIn txOut)
                    (currentSum + x)
                    rest
                else
                  -- Split the output to meet the target exactly.
                  let cappedValue = target - currentSum
                      leftoverVal = x - cappedValue
                      cappedTxOut = updateTxOutValue txOut cappedValue
                      leftoverTxOut = updateTxOutValue txOut leftoverVal
                   in go
                        (foundSoFar <> UTxO.singleton txIn cappedTxOut)
                        (UTxO.difference leftovers (UTxO.singleton txIn txOut) <> UTxO.singleton txIn leftoverTxOut)
                        (currentSum + cappedValue)
                        rest

-- | Helper to create a new TxOut with a specified lovelace value
updateTxOutValue :: TxOut ctx -> Coin -> TxOut ctx
updateTxOutValue (TxOut addr _ datum refScript) newValue =
  TxOut addr (fromLedgerValue $ mkAdaValue ShelleyBasedEraConway newValue) datum refScript

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
-- - all inputs of deposited utxo are actually spent,
-- - the deposit script output actually contains the deposited value,
-- - an upper validity bound has been set (used as creation slot).
observeDepositTx ::
  NetworkId ->
  Tx ->
  Maybe DepositObservation
observeDepositTx networkId tx = do
  depositOut <- fmap head . nonEmpty $ txOuts' tx
  (headId, deposited, deadline) <- observeDepositTxOut network (toCtxUTxOTxOut depositOut)
  guard $ spendsAll deposited
  created <- getUpperBound
  pure
    DepositObservation
      { headId
      , depositTxId = txId tx
      , deposited
      , created
      , deadline = posixToUTCTime deadline
      }
 where
  spendsAll = all (`elem` txIns' tx) . UTxO.inputSet

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
    guard $ depositValue `containsValue` UTxO.totalValue depositedUTxO
    pure depositedUTxO
  pure (headId, deposit, deadline)
 where
  depositValue = txOutValue depositOut
