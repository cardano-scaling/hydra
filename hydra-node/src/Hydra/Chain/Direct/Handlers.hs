{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provide infrastructure-independent "handlers" for posting transactions and following the chain.
--
-- This module encapsulates the transformation logic between cardano transactions and `HydraNode` abstractions
-- `PostChainTx` and `OnChainTx`, and maintainance of on-chain relevant state.
module Hydra.Chain.Direct.Handlers where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM (modifyTVar, newTVarIO, writeTVar)
import Control.Monad.Class.MonadSTM (throwSTM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  BlockHeader,
  ChainPoint (..),
  Tx,
  TxId,
  chainPointToSlotNo,
  fromLedgerTxIn,
  getChainPoint,
  getTxBody,
  getTxId,
 )
import Hydra.Chain (
  Chain (..),
  ChainCallback,
  ChainEvent (..),
  ChainStateType,
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.CardanoClient (submitTransaction)
import Hydra.Chain.Direct.State (
  ChainContext (ChainContext, contestationPeriod, networkId),
  ChainState (Closed, Idle, Initial, Open),
  ChainStateAt (..),
  abort,
  close,
  collect,
  commit,
  commit',
  contest,
  fanout,
  getKnownUTxO,
  initialize,
  observeSomeTx,
 )
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Hydra.Chain.Direct.Wallet (
  ErrCoverFee (..),
  TinyWallet (..),
  TinyWalletLog,
 )
import Hydra.ContestationPeriod (toNominalDiffTime)
import Hydra.Ledger (ChainSlot (ChainSlot))
import Hydra.Logging (Tracer, traceWith)
import Plutus.Orphans ()
import System.IO.Error (userError)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

-- | Handle of a local chain state that is kept in the direct chain layer.
data LocalChainState m = LocalChainState
  { getLatest :: STM m ChainStateAt
  , pushNew :: ChainStateAt -> STM m ()
  , rollback :: ChainPoint -> STM m ChainStateAt
  }

-- | Initialize a new local chain state with given 'ChainStateAt' (see also
-- 'initialChainState').
newLocalChainState ::
  MonadSTM m =>
  ChainStateAt ->
  m (LocalChainState m)
newLocalChainState chainStateAt = do
  tv <- newTVarIO chainStateAt
  pure
    LocalChainState
      { getLatest = getLatest tv
      , pushNew = pushNew tv
      , rollback = rollback tv
      }
 where
  getLatest tv = readTVar tv

  pushNew tv cs =
    modifyTVar tv $ \prev ->
      cs{previous = Just prev}

  rollback tv point = do
    latest <- readTVar tv
    let rolledBack = go point latest
    writeTVar tv rolledBack
    pure rolledBack

  go rollbackChainPoint = \case
    cs@ChainStateAt{recordedAt = Just recordPoint}
      | recordPoint <= rollbackChainPoint -> cs
    ChainStateAt{previous = Just prev} -> go rollbackChainPoint prev
    cs -> cs

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
  (MonadSTM m, MonadThrow (STM m), MonadIO m) =>
  Tracer m DirectChainLog ->
  -- | Means to acquire a new 'TimeHandle'.
  GetTimeHandle m ->
  TinyWallet m ->
  ChainContext ->
  LocalChainState m ->
  FilePath ->
  SubmitTx m ->
  Chain Tx m
mkChain tracer queryTimeHandle wallet@TinyWallet{getUTxO} ctx@ChainContext{networkId} LocalChainState{getLatest} nodeSocket submitTx =
  Chain
    { postTx = \tx -> do
        chainState <- atomically getLatest
        traceWith tracer $ ToPost{toPost = tx}
        timeHandle <- queryTimeHandle
        vtx <-
          -- FIXME (MB): cardano keys should really not be here (as this
          -- point they are in the 'chainState' stored in the 'ChainContext')
          -- . They are only required for the init transaction and ought to
          -- come from the _client_ and be part of the init request
          -- altogether. This goes in the direction of 'dynamic heads' where
          -- participants aren't known upfront but provided via the API.
          -- Ultimately, an init request from a client would contain all the
          -- details needed to establish connection to the other peers and
          -- to bootstrap the init transaction. For now, we bear with it and
          -- keep the static keys in context.
          atomically (prepareTxToPost timeHandle wallet ctx chainState tx)
            >>= finalizeTx wallet ctx chainState mempty
        submitTx vtx
    , -- Handle that creates a draft commit tx using the user utxo.
      -- Possible errors are handled at the api server level.
      draftCommitTx = \utxoToCommit -> do
        chainState <- atomically getLatest
        case Hydra.Chain.Direct.State.chainState chainState of
          Initial st -> do
            walletUtxos <- atomically getUTxO
            let walletTxIns = fromLedgerTxIn <$> Map.keys walletUtxos
            let userTxIns = Set.toList $ UTxO.inputSet utxoToCommit
            let matchedWalletUtxo = filter (`elem` walletTxIns) userTxIns
            -- prevent trying to spend internal wallet's utxo
            if null matchedWalletUtxo
              then
                sequenceA $
                  commit' ctx st utxoToCommit
                    <&> finalizeTx wallet ctx chainState (fst <$> utxoToCommit)
              else pure $ Left SpendingNodeUtxoForbidden
          _ -> pure $ Left FailedToDraftTxNotInitializing
    , -- Post a signed transaction on behalf of the user.
      postUserTx = liftIO . submitTransaction networkId nodeSocket
    }

-- | Balance and sign the given partial transaction.
finalizeTx ::
  (MonadThrow m) =>
  TinyWallet m ->
  ChainContext ->
  ChainStateType Tx ->
  UTxO.UTxO ->
  Tx ->
  m Tx
finalizeTx TinyWallet{sign, coverFee} ctx ChainStateAt{chainState} userUTxO partialTx = do
  let headUTxO = getKnownUTxO ctx <> getKnownUTxO chainState <> userUTxO
  coverFee headUTxO partialTx >>= \case
    Left ErrNoFuelUTxOFound ->
      throwIO (NoFuelUTXOFound :: PostTxError Tx)
    Left ErrNotEnoughFunds{} ->
      throwIO (NotEnoughFuel :: PostTxError Tx)
    Left ErrScriptExecutionFailed{scriptFailure = (redeemerPtr, scriptFailure)} ->
      throwIO
        ( ScriptFailedInWallet
            { redeemerPtr = show redeemerPtr
            , failureReason = show scriptFailure
            } ::
            PostTxError Tx
        )
    Left e -> do
      throwIO
        ( InternalWalletError
            { headUTxO
            , reason = show e
            , tx = partialTx
            } ::
            PostTxError Tx
        )
    Right balancedTx -> do
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
  deriving (Eq, Show, Exception)

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
  LocalChainState m ->
  -- | A chain-sync handler to use in a local-chain-sync client.
  ChainSyncHandler m
chainSyncHandler tracer callback getTimeHandle ctx localChainState =
  ChainSyncHandler
    { onRollBackward
    , onRollForward
    }
 where
  LocalChainState{rollback, getLatest, pushNew} = localChainState

  onRollBackward :: ChainPoint -> m ()
  onRollBackward point = do
    traceWith tracer $ RolledBackward{point}
    rolledBackChainState <- atomically $ rollback point
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

    forM_ receivedTxs $ \tx -> do
      maybeObserveSomeTx point tx >>= \case
        Nothing -> pure ()
        Just event -> callback event

  maybeObserveSomeTx point tx = atomically $ do
    csa@ChainStateAt{chainState} <- getLatest
    case observeSomeTx ctx chainState tx of
      Nothing -> pure Nothing
      Just (observedTx, cs') -> do
        let newChainState =
              ChainStateAt
                { chainState = cs'
                , recordedAt = Just point
                , previous = Just csa
                }
        pushNew newChainState
        pure $ Just Observation{observedTx, newChainState}

prepareTxToPost ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TimeHandle ->
  TinyWallet m ->
  ChainContext ->
  ChainStateType Tx ->
  PostChainTx Tx ->
  STM m Tx
prepareTxToPost timeHandle wallet ctx cst@ChainStateAt{chainState} tx =
  case (tx, chainState) of
    (InitTx params, Idle) ->
      getSeedInput wallet >>= \case
        Just seedInput ->
          pure $ initialize ctx params seedInput
        Nothing ->
          throwIO (NoSeedInput @Tx)
    (AbortTx{utxo}, Initial st) ->
      pure $ abort ctx st utxo
    -- NOTE / TODO: 'CommitTx' also contains a 'Party' which seems redundant
    -- here. The 'Party' is already part of the state and it is the only party
    -- which can commit from this Hydra node.
    (CommitTx{committed}, Initial st) ->
      -- NOTE: Eventually we will deprecate the internal 'CommitTx' command and
      -- only have external commits via 'draftCommitTx'.
      either throwIO pure (commit ctx st committed)
    -- TODO: We do not rely on the utxo from the collect com tx here because the
    -- chain head-state is already tracking UTXO entries locked by commit scripts,
    -- and thus, can re-construct the committed UTXO for the collectComTx from
    -- the commits' datums.
    --
    -- Perhaps we do want however to perform some kind of sanity check to ensure
    -- that both states are consistent.
    (CollectComTx{}, Initial st) ->
      pure $ collect ctx st
    (CloseTx{confirmedSnapshot}, Open st) -> do
      (currentSlot, currentTime) <- throwLeft currentPointInTime
      upperBound <- calculateTxUpperBoundFromContestationPeriod currentTime
      pure (close ctx st confirmedSnapshot currentSlot upperBound)
    (ContestTx{confirmedSnapshot}, Closed st) -> do
      (_, currentTime) <- throwLeft currentPointInTime
      upperBound <- calculateTxUpperBoundFromContestationPeriod currentTime
      pure (contest ctx st confirmedSnapshot upperBound)
    (FanoutTx{utxo, contestationDeadline}, Closed st) -> do
      deadlineSlot <- throwLeft $ slotFromUTCTime contestationDeadline
      pure (fanout ctx st utxo deadlineSlot)
    (_, _) -> throwIO $ InvalidStateToPost{txTried = tx, chainState = cst}
 where
  -- XXX: Might want a dedicated exception type here
  throwLeft = either (throwSTM . userError . toString) pure

  TimeHandle{currentPointInTime, slotFromUTCTime} = timeHandle

  -- See ADR21 for context
  calculateTxUpperBoundFromContestationPeriod currentTime = do
    let effectiveDelay = min (toNominalDiffTime $ contestationPeriod ctx) maxGraceTime
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
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary DirectChainLog where
  arbitrary = genericArbitrary
