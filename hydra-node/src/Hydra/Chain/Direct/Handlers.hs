{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provide infrastructure-independent "handlers" for posting transactions and following the chain.
--
-- This module encapsulates the transformation logic between cardano transactions and `HydraNode` abstractions
-- `PostChainTx` and `OnChainTx`, and maintainance of on-chain relevant state.
module Hydra.Chain.Direct.Handlers where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM (modifyTVar, newTVarIO, writeTVar)
import Control.Monad.Class.MonadSTM (throwSTM)
import Hydra.Cardano.Api (
  BlockHeader,
  ChainPoint (..),
  Tx,
  TxId,
  chainPointToSlotNo,
  getChainPoint,
  getTxBody,
  getTxId,
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
  ChainSlot (..),
  ChainStateType,
  IsChainState,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainStateAt (..),
  abort,
  chainSlotFromPoint,
  close,
  collect,
  commit',
  contest,
  decrement,
  fanout,
  getKnownUTxO,
  increment,
  initialize,
  recover,
 )
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Hydra.Chain.Direct.Wallet (
  ErrCoverFee (..),
  TinyWallet (..),
  TinyWalletLog,
 )
import Hydra.Ledger.Cardano (adjustUTxO, fromChainSlot)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Plutus.Extras (posixToUTCTime)
import Hydra.Tx (
  CommitBlueprintTx (..),
  HeadParameters (..),
  UTxOType,
  headSeedToTxIn,
  txInToHeadSeed,
 )
import Hydra.Tx.Close (ClosedThreadOutput (..))
import Hydra.Tx.ContestationPeriod (toNominalDiffTime)
import Hydra.Tx.Deposit (DepositObservation (..), depositTx)
import Hydra.Tx.Observe (
  AbortObservation (..),
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  IncrementObservation (..),
  InitObservation (..),
  observeHeadTx,
 )
import Hydra.Tx.Recover (RecoverObservation (..))
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
  (MonadSTM m, IsChainState tx) =>
  ChainStateHistory tx ->
  m (LocalChainState m tx)
newLocalChainState chainState = do
  tv <- newTVarIO chainState
  pure
    LocalChainState
      { getLatest = getLatest tv
      , pushNew = pushNew tv
      , rollback = rollback tv
      , history = readTVar tv
      }
 where
  getLatest tv = currentState <$> readTVar tv

  pushNew tv cs =
    modifyTVar tv (pushNewState cs)

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
  Tracer m DirectChainLog ->
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
        vtx <-
          atomically (prepareTxToPost timeHandle wallet ctx spendableUTxO tx)
            >>= finalizeTx wallet ctx spendableUTxO mempty
        submitTx vtx
    , -- Handle that creates a draft commit tx using the user utxo and a _blueprint_ transaction.
      -- Possible errors are handled at the api server level.
      draftCommitTx = \headId commitBlueprintTx -> do
        ChainStateAt{spendableUTxO} <- atomically getLatest
        let CommitBlueprintTx{lookupUTxO} = commitBlueprintTx
        traverse (finalizeTx wallet ctx spendableUTxO lookupUTxO) $
          commit' ctx headId spendableUTxO commitBlueprintTx
    , -- Handle that creates a draft **deposit** tx using the user utxo and a deadline.
      -- Possible errors are handled at the api server level.
      draftDepositTx = \headId commitBlueprintTx deadline -> do
        let CommitBlueprintTx{lookupUTxO} = commitBlueprintTx
        ChainStateAt{spendableUTxO} <- atomically getLatest
        traverse (finalizeTx wallet ctx spendableUTxO lookupUTxO) $
          -- TODO: Should we move deposit tx argument verification to `depositTx` function and have Either here?
          Right (depositTx (networkId ctx) headId commitBlueprintTx deadline)
    , -- Submit a cardano transaction to the cardano-node using the
      -- LocalTxSubmission protocol.
      submitTx
    }

-- | Balance and sign the given partial transaction.
finalizeTx ::
  MonadThrow m =>
  TinyWallet m ->
  ChainContext ->
  UTxO.UTxO ->
  UTxO.UTxO ->
  Tx ->
  m Tx
finalizeTx TinyWallet{sign, coverFee} ctx utxo userUTxO partialTx = do
  let headUTxO = getKnownUTxO ctx <> utxo <> userUTxO
  coverFee headUTxO partialTx >>= \case
    Left ErrNoFuelUTxOFound ->
      throwIO (NoFuelUTXOFound :: PostTxError Tx)
    Left ErrNotEnoughFunds{} ->
      throwIO (NotEnoughFuel :: PostTxError Tx)
    Left ErrScriptExecutionFailed{redeemerPointer, scriptFailure} ->
      throwIO
        ( ScriptFailedInWallet
            { redeemerPtr = redeemerPointer
            , failureReason = scriptFailure
            } ::
            PostTxError Tx
        )
    Left e ->
      throwIO
        ( InternalWalletError
            { headUTxO
            , reason = show e
            , tx = partialTx
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
  Tracer m DirectChainLog ->
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
    rolledBackChainState <- atomically $ rollback (chainSlotFromPoint point)
    callback Rollback{rolledBackChainState}

  onRollForward :: BlockHeader -> [Tx] -> m ()
  onRollForward header receivedTxs = do
    let point = getChainPoint header
    traceWith tracer $
      RolledForward
        { point
        , receivedTxIds = getTxId . getTxBody <$> receivedTxs
        }

    case chainPointToSlotNo point of
      Nothing -> pure ()
      Just slotNo -> do
        timeHandle <- getTimeHandle
        case slotToUTCTime timeHandle slotNo of
          Left reason ->
            throwIO TimeConversionException{slotNo, reason}
          Right utcTime -> do
            let chainSlot = ChainSlot . fromIntegral $ unSlotNo slotNo
            callback (Tick{chainTime = utcTime, chainSlot})

    forM_ receivedTxs $
      maybeObserveSomeTx point >=> \case
        Nothing -> pure ()
        Just event -> callback event

  maybeObserveSomeTx point tx = atomically $ do
    ChainStateAt{spendableUTxO} <- getLatest
    let observation = observeHeadTx networkId spendableUTxO tx
    case convertObservation observation of
      Nothing -> pure Nothing
      Just observedTx -> do
        let newChainState =
              ChainStateAt
                { spendableUTxO = adjustUTxO tx spendableUTxO
                , recordedAt = Just point
                }
        pushNew newChainState
        pure $ Just Observation{observedTx, newChainState}

convertObservation :: HeadObservation -> Maybe (OnChainTx Tx)
convertObservation = \case
  NoHeadTx -> Nothing
  Init InitObservation{headId, contestationPeriod, parties, seedTxIn, participants} ->
    pure
      OnInitTx
        { headId
        , headSeed = txInToHeadSeed seedTxIn
        , headParameters = HeadParameters{contestationPeriod, parties}
        , participants
        }
  Abort AbortObservation{headId} ->
    pure OnAbortTx{headId}
  Commit CommitObservation{headId, party, committed} ->
    pure OnCommitTx{headId, party, committed}
  CollectCom CollectComObservation{headId} ->
    pure OnCollectComTx{headId}
  Deposit DepositObservation{headId, deposited, depositTxId, deadline} ->
    pure $ OnDepositTx{headId, deposited, depositTxId, deadline = posixToUTCTime deadline}
  Recover RecoverObservation{headId, recoveredTxId} ->
    pure OnRecoverTx{headId, recoveredTxId}
  Increment IncrementObservation{headId, newVersion, depositTxId} ->
    pure OnIncrementTx{headId, newVersion, depositTxId}
  Decrement DecrementObservation{headId, newVersion, distributedOutputs} ->
    pure OnDecrementTx{headId, newVersion, distributedOutputs}
  -- XXX: Needing ClosedThreadOutput feels weird here
  Close CloseObservation{headId, snapshotNumber, threadOutput = ClosedThreadOutput{closedContestationDeadline}} ->
    pure
      OnCloseTx
        { headId
        , snapshotNumber
        , contestationDeadline = posixToUTCTime closedContestationDeadline
        }
  Contest ContestObservation{contestationDeadline, headId, snapshotNumber} ->
    pure OnContestTx{contestationDeadline, headId, snapshotNumber}
  Fanout FanoutObservation{headId, fanoutUTxO} ->
    pure OnFanoutTx{headId, fanoutUTxO}

prepareTxToPost ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TimeHandle ->
  TinyWallet m ->
  ChainContext ->
  -- | Spendable UTxO
  UTxOType Tx ->
  PostChainTx Tx ->
  STM m Tx
prepareTxToPost timeHandle wallet ctx spendableUTxO tx =
  case tx of
    InitTx{participants, headParameters} ->
      getSeedInput wallet >>= \case
        Just seedInput ->
          pure $ initialize ctx seedInput participants headParameters
        Nothing ->
          throwIO (NoSeedInput @Tx)
    AbortTx{utxo, headSeed} ->
      case headSeedToTxIn headSeed of
        Nothing ->
          throwIO (InvalidSeed{headSeed} :: PostTxError Tx)
        Just seedTxIn ->
          case abort ctx seedTxIn spendableUTxO utxo of
            Left _ -> throwIO (FailedToConstructAbortTx @Tx)
            Right abortTx -> pure abortTx
    -- TODO: We do not rely on the utxo from the collect com tx here because the
    -- chain head-state is already tracking UTXO entries locked by commit scripts,
    -- and thus, can re-construct the committed UTXO for the collectComTx from
    -- the commits' datums.
    --
    -- Perhaps we do want however to perform some kind of sanity check to ensure
    -- that both states are consistent.
    CollectComTx{utxo, headId, headParameters} ->
      case collect ctx headId headParameters utxo spendableUTxO of
        Left _ -> throwIO (FailedToConstructCollectTx @Tx)
        Right collectTx -> pure collectTx
    IncrementTx{headId, headParameters, incrementingSnapshot, depositTxId} -> do
      (_, currentTime) <- throwLeft currentPointInTime
      let HeadParameters{contestationPeriod} = headParameters
      (upperBound, _) <- calculateTxUpperBoundFromContestationPeriod currentTime contestationPeriod
      case increment ctx spendableUTxO headId headParameters incrementingSnapshot depositTxId upperBound of
        Left err -> throwIO (FailedToConstructIncrementTx{failureReason = show err} :: PostTxError Tx)
        Right incrementTx' -> pure incrementTx'
    RecoverTx{headId, recoverTxId, deadline} -> do
      case recover ctx headId recoverTxId spendableUTxO (fromChainSlot deadline) of
        Left err -> throwIO (FailedToConstructRecoverTx{failureReason = show err} :: PostTxError Tx)
        Right recoverTx' -> pure recoverTx'
    DecrementTx{headId, headParameters, decrementingSnapshot} ->
      case decrement ctx spendableUTxO headId headParameters decrementingSnapshot of
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
    FanoutTx{utxo, utxoToCommit, utxoToDecommit, headSeed, contestationDeadline} -> do
      deadlineSlot <- throwLeft $ slotFromUTCTime contestationDeadline
      case headSeedToTxIn headSeed of
        Nothing ->
          throwIO (InvalidSeed{headSeed} :: PostTxError Tx)
        Just seedTxIn ->
          case fanout ctx spendableUTxO seedTxIn utxo utxoToCommit utxoToDecommit deadlineSlot of
            Left _ -> throwIO (FailedToConstructFanoutTx @Tx)
            Right fanoutTx -> pure fanoutTx
 where
  -- XXX: Might want a dedicated exception type here
  throwLeft = either (throwSTM . userError . toString) pure

  TimeHandle{currentPointInTime, slotFromUTCTime} = timeHandle

  -- See ADR21 for context
  calculateTxUpperBoundFromContestationPeriod currentTime contestationPeriod = do
    let effectiveDelay = min (toNominalDiffTime contestationPeriod) maxGraceTime
    let upperBoundTime = addUTCTime effectiveDelay currentTime
    upperBoundSlot <- throwLeft $ slotFromUTCTime upperBoundTime
    pure (upperBoundSlot, upperBoundTime)

-- | Maximum delay we put on the upper bound of transactions to fit into a block.
-- NOTE: This is highly depending on the network. If the security parameter and
-- epoch length result in a short horizon, this is problematic.
maxGraceTime :: NominalDiffTime
maxGraceTime = 200

--
-- Tracing
--

data DirectChainLog
  = ToPost {toPost :: PostChainTx Tx}
  | PostingTx {txId :: TxId}
  | PostedTx {txId :: TxId}
  | PostingFailed {tx :: Tx, postTxError :: PostTxError Tx}
  | RolledForward {point :: ChainPoint, receivedTxIds :: [TxId]}
  | RolledBackward {point :: ChainPoint}
  | Wallet TinyWalletLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary DirectChainLog where
  arbitrary = genericArbitrary
  shrink = genericShrink
