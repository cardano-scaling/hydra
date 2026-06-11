{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provide infrastructure-independent "handlers" for posting transactions and following the chain.
--
-- This module encapsulates the transformation logic between cardano transactions and `HydraNode` abstractions
-- `PostChainTx` and `OnChainTx`, and maintenance of on-chain relevant state.
module Hydra.Chain.Direct.Handlers where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Core (PParams)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM (modifyTVar, writeTVar)
import Control.Monad.Class.MonadSTM (throwSTM)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Hydra.Cardano.Api (
  Address,
  BlockHeader,
  ByronAddr,
  ChainPoint (..),
  LedgerEra,
  Tx,
  TxId,
  TxIn,
  TxOut,
  UTxO,
  calculateMinimumUTxO,
  chainPointToSlotNo,
  fromCtxUTxOTxOut,
  getChainPoint,
  getTxBody,
  getTxId,
  liftEither,
  shelleyBasedEra,
  throwError,
  txOutAddress,
  pattern ByronAddressInEra,
 )
import Hydra.Chain (
  Chain (..),
  ChainCallback,
  ChainEvent (..),
  ChainStateHistory,
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
  currentState,
  pushNewState,
  rollbackHistory,
 )
import Hydra.Chain.ChainState (
  ChainSlot,
  ChainStateType,
  IsChainState,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainStateAt (..),
  PartialFanoutError (..),
  chainSlotFromPoint,
  close,
  contest,
  decrement,
  fanout,
  finalPartialFanout,
  getKnownUTxO,
  increment,
  initialize,
  partialFanout,
  recover,
 )
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Hydra.Chain.Direct.Wallet (
  ErrCoverFee (..),
  TinyWallet (..),
  TinyWalletLog,
 )
import Hydra.Ledger.Cardano (adjustUTxO, fromChainSlot)
import Hydra.Ledger.Cardano.Evaluate (EvaluationError (..), EvaluationReport, renderEvaluationReport)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node.Util (checkNonADAAssetsUTxO)
import Hydra.Tx (
  CommitBlueprintTx (..),
  HeadParameters (..),
  IsTx (..),
  UTxOType,
  headSeedToTxIn,
 )
import Hydra.Tx.ContestationPeriod (toNominalDiffTime)
import Hydra.Tx.Deposit (DepositObservation (..), depositTx)
import Hydra.Tx.Observe (
  CloseObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  IncrementObservation (..),
  InitObservation (..),
  PartialFanoutObservation (..),
  observeHeadTx,
 )
import Hydra.Tx.Recover (RecoverObservation (..))
import Hydra.Tx.Snapshot (getSnapshot, snapshotUTxO)
import System.IO.Error (userError)

-- | Handle of a mutable local chain state that is kept in the direct chain layer.
data LocalChainState m tx = LocalChainState
  { getLatest :: STM m (ChainStateType tx)
  , pushNew :: ChainStateType tx -> STM m ()
  , rollback :: ChainSlot -> STM m (ChainStateType tx)
  , history :: STM m (ChainStateHistory tx)
  }

-- | Initialize a new local chain state from a given chain state history.
newLocalChainState ::
  forall m tx.
  (IsChainState tx, MonadLabelledSTM m) =>
  ChainStateHistory tx ->
  m (LocalChainState m tx)
newLocalChainState chainState = do
  tv <- newLabelledTVarIO "local-chain-state" chainState
  pure
    LocalChainState
      { getLatest = getLatest tv
      , pushNew = pushNew tv
      , rollback = rollback tv
      , history = readTVar tv
      }
 where
  -- REVIEW: why using `currentState` instead of `lastKnown` ???
  getLatest :: TVar m (ChainStateHistory tx) -> STM m (ChainStateType tx)
  getLatest tv = currentState <$> readTVar tv

  pushNew :: TVar m (ChainStateHistory tx) -> ChainStateType tx -> STM m ()
  pushNew tv cs =
    modifyTVar tv (pushNewState cs)

  rollback :: TVar m (ChainStateHistory tx) -> ChainSlot -> STM m (ChainStateType tx)
  rollback tv chainSlot = do
    rolledBack <-
      readTVar tv
        <&> rollbackHistory chainSlot
    writeTVar tv rolledBack
    pure (currentState rolledBack)

-- * Posting Transactions

-- | A callback used to actually submit a transaction to the chain.
type SubmitTx m = Tx -> m ()

-- | A way to acquire a 'TimeHandle'
type GetTimeHandle m = m TimeHandle

-- | Create a `Chain` component for posting "real" cardano transactions.
--
-- This component does not actually interact with a cardano-node, but creates
-- cardano transactions from `PostChainTx` transactions emitted by a
-- `HydraNode`, balancing and signing them using given `TinyWallet`, before
-- handing it off to the given 'SubmitTx' callback. There is also a 'draftTx'
-- option for drafting a commit tx on behalf of the user using their selected
-- utxo.
--
-- NOTE: Given the constraints on `m` this function should work within `IOSim`
-- and does not require any actual `IO` to happen which makes it highly suitable
-- for simulations and testing.
mkChain ::
  (MonadSTM m, MonadThrow (STM m)) =>
  Tracer m CardanoChainLog ->
  -- | Means to acquire a new 'TimeHandle'.
  GetTimeHandle m ->
  TinyWallet m ->
  ChainContext ->
  LocalChainState m Tx ->
  SubmitTx m ->
  Chain Tx m
mkChain tracer queryTimeHandle wallet ctx LocalChainState{getLatest} submitTx =
  Chain
    { postTx = \tx -> do
        ChainStateAt{spendableUTxO} <- atomically getLatest
        traceWith tracer $ ToPost{toPost = tx}
        timeHandle <- queryTimeHandle
        let TimeHandle{slotFromUTCTime} = timeHandle
            resolveHeadInfo headSeed deadline = do
              slot <- either (\err -> throwIO (ContestationDeadlineOutsideTimeHorizon{failureReason = err} :: PostTxError Tx)) pure $ slotFromUTCTime deadline
              tin <- maybe (throwIO (InvalidSeed{headSeed} :: PostTxError Tx)) pure $ headSeedToTxIn headSeed
              pure (slot, tin)
        vtx <- case tx of
          FanoutTx{utxo, utxoToCommit, utxoToDecommit, utxoForProof, headSeed, contestationDeadline} -> do
            (deadlineSlot, seedTxIn) <- resolveHeadInfo headSeed contestationDeadline
            let fullUTxO = utxo <> fold utxoToCommit <> fold utxoToDecommit
            findFittingFanoutTx
              tracer
              wallet
              ctx
              spendableUTxO
              seedTxIn
              (fanout ctx spendableUTxO seedTxIn utxo utxoToCommit utxoToDecommit utxoForProof deadlineSlot)
              utxoForProof
              fullUTxO
              deadlineSlot
              >>= finalizeTx wallet ctx spendableUTxO mempty
          FinalPartialFanoutTx{utxoToDistribute, utxoForProof, headSeed, contestationDeadline} -> do
            (deadlineSlot, seedTxIn) <- resolveHeadInfo headSeed contestationDeadline
            findFittingFanoutTx
              tracer
              wallet
              ctx
              spendableUTxO
              seedTxIn
              (finalPartialFanout ctx spendableUTxO seedTxIn utxoToDistribute utxoForProof deadlineSlot)
              utxoForProof
              utxoToDistribute
              deadlineSlot
              >>= finalizeTx wallet ctx spendableUTxO mempty
          InitTx{participants, headParameters} -> do
            seedInput <-
              atomically $
                getSeedInput wallet >>= maybe (throwSTM (NoSeedInput @Tx)) pure
            pparams <- getPParams wallet
            finalizeTx wallet ctx spendableUTxO mempty $
              initialize ctx pparams seedInput participants headParameters
          _ ->
            atomically (prepareTxToPost timeHandle ctx spendableUTxO tx)
              >>= finalizeTx wallet ctx spendableUTxO mempty
        submitTx vtx
    , draftDepositTx = \headId pparams commitBlueprintTx deadline changeAddress -> do
        let CommitBlueprintTx{lookupUTxO} = commitBlueprintTx
        ChainStateAt{spendableUTxO} <- atomically getLatest
        TimeHandle{currentPointInTime} <- queryTimeHandle
        -- XXX: What an error handling mess
        runExceptT $
          do
            liftEither $ do
              rejectByronAddresses lookupUTxO
              rejectLowDeposits pparams lookupUTxO
            (currentSlot, currentTime) <- case currentPointInTime of
              Left failureReason -> throwError FailedToConstructDepositTx{failureReason}
              Right (s, t) -> pure (s, t)
            -- NOTE: Use a smaller upper bound than maxGraceTime to allow for
            -- shorter than 200 slot deposit periods. This is only important on
            -- fast moving networks (e.g. in testing). XXX: Making maxGraceTime
            -- configurable would avoid this.
            let untilDeadline = diffUTCTime deadline currentTime
            let graceTime = maxGraceTime `min` untilDeadline / 2
            -- -- NOTE: But also not make it smaller than 10 slots.
            let validBeforeSlot = currentSlot + fromInteger (truncate graceTime `max` 10)
            lift . finalizeTx wallet ctx spendableUTxO lookupUTxO $
              depositTx (networkId ctx) pparams headId commitBlueprintTx validBeforeSlot deadline changeAddress
    , -- Submit a cardano transaction to the cardano-node using the
      -- LocalTxSubmission protocol.
      submitTx
    , checkNonADAAssets = checkNonADAAssetsUTxO . snapshotUTxO . getSnapshot
    }

-- Check each UTxO entry against the minADAUTxO value.
-- Throws 'DepositTooLow' exception.
rejectLowDeposits :: PParams LedgerEra -> UTxO -> Either (PostTxError Tx) ()
rejectLowDeposits pparams utxo = do
  let insAndOuts = UTxO.toList utxo
  let providedValues = (\(i, o) -> (i, UTxO.totalLovelace $ UTxO.singleton i o)) <$> insAndOuts
  let minimumValues = (\(i, o) -> (i, calculateMinimumUTxO shelleyBasedEra pparams $ fromCtxUTxOTxOut o)) <$> insAndOuts
  let results =
        ( \(i, minVal) ->
            case List.find (\(ix, providedVal) -> i == ix && providedVal < minVal) providedValues of
              Nothing -> Right ()
              Just (_, tooLowValue) ->
                Left (DepositTooLow{providedValue = tooLowValue, minimumValue = minVal} :: PostTxError Tx)
        )
          <$> minimumValues
  case lefts results of
    [] -> pure ()
    (e : _) -> Left e

-- | Reject any UTxO containing a Byron address, which cannot be represented
-- in the Hydra head protocol.
rejectByronAddresses :: UTxO -> Either (PostTxError Tx) ()
rejectByronAddresses utxo =
  case foldMap toByronAddr (UTxO.toList utxo) of
    (addr : _) -> Left (UnsupportedLegacyOutput addr)
    [] -> Right ()
 where
  toByronAddr :: forall a era. (a, TxOut era) -> [Address ByronAddr]
  toByronAddr (_, out) = case txOutAddress out of
    ByronAddressInEra addr -> [addr]
    _ -> []

-- | Balance and sign the given partial transaction.
finalizeTx ::
  MonadThrow m =>
  TinyWallet m ->
  ChainContext ->
  UTxO ->
  UTxO ->
  Tx ->
  m Tx
finalizeTx TinyWallet{sign, coverFee} ctx utxo userUTxO partialTx = do
  let headUTxO = getKnownUTxO ctx <> utxo <> userUTxO
  coverFee headUTxO partialTx >>= \case
    Left ErrNoFuelUTxOFound ->
      throwIO NoFuelUTXOFound{failingTx = partialTx}
    Left ErrNotEnoughFunds{} ->
      throwIO NotEnoughFuel{failingTx = partialTx}
    Left ErrScriptExecutionFailed{redeemerPointer, scriptFailure} ->
      throwIO
        ( ScriptFailedInWallet
            { redeemerPtr = redeemerPointer
            , failureReason = scriptFailure
            , failingTx = partialTx
            } ::
            PostTxError Tx
        )
    Left ErrMissingScript{scriptHash, purpose} ->
      throwIO
        ( ScriptFailedInWallet
            { redeemerPtr = purpose
            , failureReason = "Missing script witness for " <> scriptHash
            , failingTx = partialTx
            } ::
            PostTxError Tx
        )
    Left e ->
      throwIO
        ( InternalWalletError
            { headUTxO
            , reason = show e
            , failingTx = partialTx
            } ::
            PostTxError Tx
        )
    Right balancedTx ->
      pure $ sign balancedTx

-- * Following the Chain

-- | A /handler/ that takes care of following the chain.
data ChainSyncHandler m = ChainSyncHandler
  { onRollForward :: BlockHeader -> [Tx] -> m ()
  , onRollBackward :: ChainPoint -> m ()
  }

-- | Conversion of a slot number to a time failed. This can be usually be
-- considered an internal error and may be happening because the used era
-- history is too old.
data TimeConversionException = TimeConversionException
  { slotNo :: SlotNo
  , reason :: Text
  }
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Creates a `ChainSyncHandler` that can notify the given `callback` of events happening
-- on-chain.
--
-- This forms the other half of a `ChainComponent` along with `mkChain` but is decoupled from
-- actual interactions with the chain.
--
-- A `TimeHandle` is needed to do `SlotNo -> POSIXTime` conversions for 'Tick' events.
--
-- Throws 'TimeConversionException' when a received block's 'SlotNo' cannot be
-- converted to a 'UTCTime' with the given 'TimeHandle'.
chainSyncHandler ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  -- | Tracer for logging
  Tracer m CardanoChainLog ->
  ChainCallback Tx m ->
  -- | Means to acquire a new 'TimeHandle'.
  GetTimeHandle m ->
  -- | Contextual information about our chain connection.
  ChainContext ->
  LocalChainState m Tx ->
  -- | A chain-sync handler to use in a local-chain-sync client.
  ChainSyncHandler m
chainSyncHandler tracer callback getTimeHandle ctx localChainState =
  ChainSyncHandler
    { onRollBackward
    , onRollForward
    }
 where
  ChainContext{networkId} = ctx
  LocalChainState{rollback, getLatest, pushNew} = localChainState

  onRollBackward :: ChainPoint -> m ()
  onRollBackward point = do
    traceWith tracer $ RolledBackward{point}
    timeHandle <- getTimeHandle
    let slotNo = fromMaybe 0 (chainPointToSlotNo point)
    case slotToUTCTime timeHandle slotNo of
      Left reason ->
        throwIO TimeConversionException{slotNo, reason}
      Right utcTime -> do
        rolledBackChainState <- atomically $ rollback (chainSlotFromPoint point)
        callback Rollback{rolledBackChainState, chainTime = utcTime}

  onRollForward :: BlockHeader -> [Tx] -> m ()
  onRollForward header receivedTxs = do
    let point = getChainPoint header
    traceWith tracer $
      RolledForward
        { point
        , receivedTxIds = getTxId . getTxBody <$> receivedTxs
        }

    timeHandle <- getTimeHandle

    forM_ receivedTxs $
      maybeObserveSomeTx timeHandle point >=> \case
        Nothing -> pure ()
        Just event -> callback event

    case chainPointToSlotNo point of
      Nothing -> pure ()
      Just slotNo -> do
        case slotToUTCTime timeHandle slotNo of
          Left reason ->
            throwIO TimeConversionException{slotNo, reason}
          Right utcTime -> do
            callback (Tick{chainTime = utcTime, chainPoint = point})

  maybeObserveSomeTx timeHandle point tx = atomically $ do
    ChainStateAt{spendableUTxO} <- getLatest
    let observation = observeHeadTx networkId spendableUTxO tx
    case convertObservation timeHandle observation of
      Nothing -> pure Nothing
      Just observedTx -> do
        let newChainState =
              ChainStateAt
                { spendableUTxO = adjustUTxO tx spendableUTxO
                , recordedAt = Just point
                }
        pushNew newChainState
        pure $ Just Observation{observedTx, newChainState}

convertObservation :: TimeHandle -> HeadObservation -> Maybe (OnChainTx Tx)
convertObservation TimeHandle{slotToUTCTime} = \case
  NoHeadTx -> Nothing
  Init InitObservation{headId, headSeed, headParameters, participants} ->
    pure OnInitTx{headId, headSeed, headParameters, participants}
  Deposit DepositObservation{headId, depositTxId, deposited, created, deadline} -> do
    createdTime <- either (const Nothing) Just $ slotToUTCTime created
    pure $ OnDepositTx{headId, depositTxId, deposited, created = createdTime, deadline}
  Recover RecoverObservation{headId, recoveredTxId, recoveredUTxO} ->
    pure OnRecoverTx{headId, recoveredTxId, recoveredUTxO}
  Increment IncrementObservation{headId, newVersion, depositTxId} ->
    pure OnIncrementTx{headId, newVersion, depositTxId}
  Decrement DecrementObservation{headId, newVersion, distributedUTxO} ->
    pure OnDecrementTx{headId, newVersion, distributedUTxO}
  Close CloseObservation{headId, snapshotNumber, contestationDeadline} ->
    pure OnCloseTx{headId, snapshotNumber, contestationDeadline}
  Contest ContestObservation{contestationDeadline, headId, snapshotNumber} ->
    pure OnContestTx{contestationDeadline, headId, snapshotNumber}
  Fanout FanoutObservation{headId, fanoutUTxO} ->
    pure OnFanoutTx{headId, fanoutUTxO}
  FinalPartialFanout FanoutObservation{headId, fanoutUTxO} ->
    pure OnFanoutTx{headId, fanoutUTxO}
  PartialFanout PartialFanoutObservation{headId, distributedOutputs} ->
    pure OnPartialFanoutTx{headId, distributedOutputs}

prepareTxToPost ::
  forall m.
  (MonadSTM m, MonadThrow (STM m)) =>
  TimeHandle ->
  ChainContext ->
  -- | Spendable UTxO
  UTxOType Tx ->
  PostChainTx Tx ->
  STM m Tx
prepareTxToPost timeHandle ctx spendableUTxO tx =
  case tx of
    -- InitTx is handled in mkChain.postTx before reaching this function.
    InitTx{} -> throwSTM (NoSeedInput @Tx)
    IncrementTx{headSeed, headId, headParameters, incrementingSnapshot, depositTxId} -> do
      (_, currentTime) <- throwLeft currentPointInTime
      let HeadParameters{contestationPeriod} = headParameters
      (upperBound, _) <- calculateTxUpperBoundFromContestationPeriod currentTime contestationPeriod
      case increment ctx spendableUTxO (headSeed, headId) headParameters incrementingSnapshot depositTxId upperBound of
        Left err -> throwIO (FailedToConstructIncrementTx{failureReason = show err} :: PostTxError Tx)
        Right incrementTx' -> pure incrementTx'
    RecoverTx{headId, recoverTxId, deadline} -> do
      case recover ctx headId recoverTxId spendableUTxO (fromChainSlot deadline) of
        Left err -> throwIO (FailedToConstructRecoverTx{failureReason = show err} :: PostTxError Tx)
        Right recoverTx' -> pure recoverTx'
    DecrementTx{headSeed, headId, headParameters, decrementingSnapshot} ->
      case decrement ctx spendableUTxO (headSeed, headId) headParameters decrementingSnapshot of
        Left err -> throwIO (FailedToConstructDecrementTx{failureReason = show err} :: PostTxError Tx)
        Right decrementTx' -> pure decrementTx'
    CloseTx{headId, headParameters, openVersion, closingSnapshot} -> do
      (currentSlot, currentTime) <- throwLeft currentPointInTime
      let HeadParameters{contestationPeriod} = headParameters
      upperBound <- calculateTxUpperBoundFromContestationPeriod currentTime contestationPeriod
      case close ctx spendableUTxO headId headParameters openVersion closingSnapshot currentSlot upperBound of
        Left _ -> throwIO (FailedToConstructCloseTx @Tx)
        Right closeTx -> pure closeTx
    ContestTx{headId, headParameters, openVersion, contestingSnapshot} -> do
      (_, currentTime) <- throwLeft currentPointInTime
      let HeadParameters{contestationPeriod} = headParameters
      upperBound <- calculateTxUpperBoundFromContestationPeriod currentTime contestationPeriod
      case contest ctx spendableUTxO headId contestationPeriod openVersion contestingSnapshot upperBound of
        Left _ -> throwIO (FailedToConstructContestTx @Tx)
        Right contestTx -> pure contestTx
    -- These are handled in mkChain.postTx before reaching this function.
    FanoutTx{} -> throwSTM (FailedToConstructFanoutTx :: PostTxError Tx)
    FinalPartialFanoutTx{} -> throwSTM (FailedToConstructPartialFanoutTx :: PostTxError Tx)
 where
  -- XXX: Might want a dedicated exception type here
  throwLeft :: Either Text a -> STM m a
  throwLeft = either (throwSTM . userError . toString) pure

  TimeHandle{currentPointInTime, slotFromUTCTime} = timeHandle

  -- See ADR21 for context
  calculateTxUpperBoundFromContestationPeriod currentTime contestationPeriod = do
    let effectiveDelay = min (toNominalDiffTime contestationPeriod) maxGraceTime
    let upperBoundTime = addUTCTime effectiveDelay currentTime
    upperBoundSlot <- throwLeft $ slotFromUTCTime upperBoundTime
    pure (upperBoundSlot, upperBoundTime)

-- | Binary search for the largest chunk size in @[1..maxChunk]@ for which
-- 'tryTx' returns 'Just'. Assumes the predicate is monotone: if size @n@ fits,
-- all sizes @< n@ also fit. Uses upper-mid so the search terminates correctly
-- when @hi = lo + 1@. Returns 'Left ()' if no size fits. 'tryTx' may throw to
-- abort the search early.
findLargestFitting ::
  Monad m =>
  -- | Construct and check a transaction; Just tx = fits, Nothing = doesn't fit; may throw on structural failure
  (Int -> m (Maybe tx)) ->
  -- | Upper bound of chunk sizes to search (inclusive)
  Int ->
  m (Either () tx)
findLargestFitting tryTx = go (Left ()) 1
 where
  go best lo hi
    | lo > hi = pure best
    | otherwise = do
        let mid = (lo + hi + 1) `div` 2 -- ceiling division: biases toward hi so we test the larger candidate first
        tryTx mid >>= \case
          Just tx -> go (Right tx) (mid + 1) hi
          Nothing -> go best lo (mid - 1)

-- | Check whether a transaction fits within protocol size and script execution
-- limits. Size check is cheap so it short-circuits before the expensive UPLC
-- evaluation. Structural errors (script failures, protocol parameter conversion
-- errors) are traced via the provided tracer; transient misses (size exceeded,
-- budget overrun) are silent.
fitsTx ::
  Monad m =>
  Tracer m CardanoChainLog ->
  (Tx -> m Bool) ->
  (Tx -> UTxO -> m (Either EvaluationError EvaluationReport)) ->
  UTxO ->
  Tx ->
  m Bool
fitsTx tracer withinSizeLimits evalCosts evalUTxO tx = do
  withinSize <- withinSizeLimits tx
  if withinSize
    then
      evalCosts tx evalUTxO >>= \case
        Left TransactionBudgetOverspent{} -> pure False
        Left (TransactionInvalid err) -> False <$ traceWith tracer PartialFanoutFailed{reason = show err}
        Left (PParamsConversion err) -> False <$ traceWith tracer PartialFanoutFailed{reason = show err}
        Right report ->
          let failures = Map.filter isLeft report
           in if Map.null failures
                then pure True
                else False <$ traceWith tracer PartialFanoutFailed{reason = renderEvaluationReport failures}
    else pure False

-- | Try the preferred transaction first; if it doesn't fit within the script
-- execution budget or exceeds the maximum transaction size, fall back to a
-- binary search over partial fanout chunk sizes. Returns the largest chunk that
-- fits, minimising the number of fanout steps.
--
-- Error mapping:
--   * 'StaleChainState' from 'partialFanout' → 'StalePartialFanoutTx' (race
--     condition; HeadLogic silently ignores it and the chain observation loop
--     triggers the correct next step).
--   * Any other 'PartialFanoutError' → 'FailedToConstructPartialFanoutTx'
--     (structural mismatch that will not resolve on retry).
--   * No chunk fits within budget → 'FailedToConstructPartialFanoutTx'
--     (budget exhaustion; also not a race condition).
findFittingFanoutTx ::
  forall m e.
  MonadThrow m =>
  Tracer m CardanoChainLog ->
  TinyWallet m ->
  ChainContext ->
  -- | Spendable UTxO containing head output
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | Preferred tx to try first (FanoutTx or FinalPartialFanoutTx); Left skips straight to the loop
  Either e Tx ->
  -- | UTxO for the accumulator check in the partial-fanout fallback (matches the on-chain datum)
  UTxO ->
  -- | UTxOs to distribute in the partial-fanout fallback
  UTxO ->
  -- | Contestation deadline as SlotNo
  SlotNo ->
  m Tx
findFittingFanoutTx tracer TinyWallet{evaluateScriptCosts, isTxWithinSizeLimits} ctx spendableUTxO seedTxIn ePreferred proofUTxO fullUTxO deadlineSlot =
  findBest >>= either (const $ throwIO (FailedToConstructPartialFanoutTx @Tx)) pure
 where
  -- Try the preferred tx (full fanout or final partial fanout) first; only
  -- fall back to the binary search if it doesn't fit.
  findBest = either (const findFallback) tryPreferred ePreferred
   where
    tryPreferred tx = fits tx >>= bool findFallback (pure (Right tx))

  findFallback = findLargestFitting tryChunk (UTxO.size fullUTxO - 1)
   where
    tryChunk n = buildTx n >>= \tx -> bool Nothing (Just tx) <$> fits tx

  buildTx n =
    either handleErr pure $ partialFanout ctx spendableUTxO seedTxIn n proofUTxO fullUTxO deadlineSlot
   where
    handleErr err = do
      traceWith tracer PartialFanoutFailed{reason = show err}
      throwIO $ case err of
        StaleChainState -> StalePartialFanoutTx @Tx
        _ -> FailedToConstructPartialFanoutTx @Tx

  fits = fitsTx tracer isTxWithinSizeLimits evaluateScriptCosts evalUTxO

  evalUTxO = spendableUTxO <> getKnownUTxO ctx

-- | Maximum delay we put on the upper bound of transactions to fit into a block.
-- NOTE: This is highly depending on the network. If the security parameter and
-- epoch length result in a short horizon, this is problematic.
maxGraceTime :: NominalDiffTime
maxGraceTime = 200

--
-- Tracing
--

data StartingDecision
  = FromProvided ChainPoint
  | FromTip ChainPoint
  | FromPersisted
      { chainPoint :: ChainPoint
      , startChainFromSet :: Bool
      -- ^ Whether the user-provided --start-chain-from point was set
      -- but ignored, because it was older than persisted points.
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardanoChainLog
  = ToPost {toPost :: PostChainTx Tx}
  | PostingTx {txId :: TxId}
  | PostedTx {txId :: TxId}
  | PostingFailed {tx :: Tx, postTxError :: PostTxError Tx}
  | RolledForward {point :: ChainPoint, receivedTxIds :: [TxId]}
  | RolledBackward {point :: ChainPoint}
  | Wallet TinyWalletLog
  | StartingChainDecision StartingDecision
  | BlockfrostTransientError {reason :: Text, retryDelay :: Int}
  | PartialFanoutFailed {reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
