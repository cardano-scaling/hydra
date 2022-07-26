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

import Cardano.Ledger.Babbage.Tx (ValidatedTx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (SupportsSegWit (fromTxSeq))
import Cardano.Ledger.Shelley.API (TxId)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (readTVarIO, throwSTM, writeTVar)
import Data.Sequence.Strict (StrictSeq)
import Hydra.Cardano.Api (
  ChainPoint (..),
  LedgerEra,
  PaymentKey,
  SlotNo,
  Tx,
  VerificationKey,
  fromConsensusPointHF,
  fromLedgerTx,
  fromLedgerTxIn,
  fromLedgerUTxO,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct.State (
  SomeOnChainHeadState (..),
  TokHeadState (..),
  abort,
  close,
  collect,
  commit,
  contest,
  fanout,
  getContestationDeadline,
  getKnownUTxO,
  initialize,
  observeSomeTx,
  reifyState,
 )
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Hydra.Chain.Direct.Util (Block, SomePoint (..))
import Hydra.Chain.Direct.Wallet (
  ErrCoverFee (..),
  TinyWallet (..),
  TinyWalletLog,
  getFuelUTxO,
  getTxId,
 )
import Hydra.Data.ContestationPeriod (posixToUTCTime)
import Hydra.Logging (Tracer, traceWith)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..))
import Ouroboros.Network.Block (Point (..), blockPoint)
import Plutus.Orphans ()
import System.IO.Error (userError)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

-- * Posting Transactions

-- | A callback used to actually submit a transaction to the chain.
type SubmitTx m = ValidatedTx LedgerEra -> m ()

-- | Create a `Chain` component for posting "real" cardano transactions.
--
-- This component does not actually interact with a cardano-node, but creates
-- cardano transactions from `PostChainTx` transactions emitted by a
-- `HydraNode`, balancing them using given `TinyWallet`, while maintaining some
-- state in the `headState` variable and before handing it off to the given
-- 'SubmitTx' callback.
--
-- NOTE: Given the constraints on `m` this function should work within `IOSim` and does not
-- require any actual `IO`  to happen which makes it highly suitable for simulations and testing.
mkChain ::
  (MonadSTM m, MonadTimer m, MonadThrow (STM m)) =>
  Tracer m DirectChainLog ->
  m TimeHandle ->
  [VerificationKey PaymentKey] ->
  TinyWallet m ->
  TVar m SomeOnChainHeadStateAt ->
  SubmitTx m ->
  Chain Tx m
mkChain tracer queryTimeHandle cardanoKeys wallet headState submitTx =
  Chain
    { postTx = \tx -> do
        traceWith tracer $ ToPost{toPost = tx}
        timeHandle <- queryTimeHandle
        vtx <-
          atomically
            ( -- FIXME (MB): 'cardanoKeys' should really not be here. They
              -- are only required for the init transaction and ought to
              -- come from the _client_ and be part of the init request
              -- altogether. This goes in the direction of 'dynamic
              -- heads' where participants aren't known upfront but
              -- provided via the API. Ultimately, an init request from
              -- a client would contain all the details needed to
              -- establish connection to the other peers and to
              -- bootstrap the init transaction.
              -- For now, we bear with it and keep the static keys in
              -- context.
              fromPostChainTx timeHandle cardanoKeys wallet headState tx
                >>= finalizeTx wallet headState . toLedgerTx
            )
        submitTx vtx
    }

-- | Balance and sign the given partial transaction.
finalizeTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TinyWallet m ->
  TVar m SomeOnChainHeadStateAt ->
  ValidatedTx LedgerEra ->
  STM m (ValidatedTx LedgerEra)
finalizeTx TinyWallet{sign, getUTxO, coverFee} headState partialTx = do
  someSt <- currentOnChainHeadState <$> readTVar headState
  let headUTxO = (\(SomeOnChainHeadState st) -> getKnownUTxO st) someSt
  walletUTxO <- fromLedgerUTxO . Ledger.UTxO <$> getUTxO
  coverFee (Ledger.unUTxO $ toLedgerUTxO headUTxO) partialTx >>= \case
    Left ErrUnknownInput{input} -> do
      throwIO
        ( CannotSpendInput
            { input = show input
            , walletUTxO
            , headUTxO
            } ::
            PostTxError Tx
        )
    Left e ->
      throwIO
        ( CannotCoverFees
            { walletUTxO
            , headUTxO
            , reason = show e
            , tx = fromLedgerTx partialTx
            } ::
            PostTxError Tx
        )
    Right validatedTx -> do
      pure $ sign validatedTx

-- * Following the Chain

-- | The on-chain head state is maintained in a mutually-recursive data-structure,
-- pointing to the previous chain state; This allows to easily rewind the state
-- to a point in the past when rolling backward due to a change of chain fork by
-- our local cardano-node.
data SomeOnChainHeadStateAt = SomeOnChainHeadStateAt
  { currentOnChainHeadState :: SomeOnChainHeadState
  , recordedAt :: RecordedAt
  }
  deriving (Eq, Show)

-- | Records when a state was seen on-chain. 'AtStart' is used for states that
-- simply exist out of any chain events (e.g. the 'Idle' state).
data RecordedAt
  = AtStart
  | AtPoint ChainPoint SomeOnChainHeadStateAt
  deriving (Eq, Show)

-- | A /handler/ that takes care of following the chain.
-- TODO: replace use of `Block` with `Tx`
data ChainSyncHandler m = ChainSyncHandler
  { onRollForward :: Block -> m ()
  , onRollBackward :: Point Block -> m ()
  }

-- | Creates a `ChainSyncHandler` that can notify the given `callback` of events happening
-- on-chain.
--
-- This forms the other half of a `ChainComponent` along with `mkChain` but is decoupled from
-- actual interactions with the chain.
chainSyncHandler ::
  forall m.
  (MonadSTM m, MonadTime m) =>
  -- | Tracer for logging
  Tracer m DirectChainLog ->
  -- | Chain callback
  (ChainEvent Tx -> m ()) ->
  -- | On-chain head-state.
  TVar m SomeOnChainHeadStateAt ->
  -- | A chain-sync handler to use in a local-chain-sync client.
  ChainSyncHandler m
chainSyncHandler tracer callback headState =
  ChainSyncHandler
    { onRollBackward
    , onRollForward
    }
 where
  onRollBackward :: Point Block -> m ()
  onRollBackward point = do
    traceWith tracer $ RolledBackward $ SomePoint point
    (st, depth) <- rollback (fromConsensusPointHF point) <$> readTVarIO headState
    atomically $ writeTVar headState st
    callback (Rollback depth)

  onRollForward :: Block -> m ()
  onRollForward blk = do
    let receivedTxs = toList $ getBabbageTxs blk
    now <- getCurrentTime
    onChainTxs <- reverse <$> atomically (foldM (withNextTx now (blockPoint blk)) [] receivedTxs)
    unless (null receivedTxs) $
      traceWith tracer $
        ReceivedTxs
          { onChainTxs
          , receivedTxs = map (\tx -> (getTxId tx, tx)) receivedTxs
          }
    mapM_ (callback . Observation) onChainTxs

  -- NOTE: We pass 'now' or current time because we need it for observing passing of time in the
  -- contestation phase.
  withNextTx :: UTCTime -> Point Block -> [OnChainTx Tx] -> ValidatedTx LedgerEra -> STM m [OnChainTx Tx]
  withNextTx now point observed (fromLedgerTx -> tx) = do
    st <- readTVar headState
    case observeSomeTx tx (currentOnChainHeadState st) of
      Just (onChainTx, st'@(SomeOnChainHeadState nextState)) -> do
        writeTVar headState $
          SomeOnChainHeadStateAt
            { currentOnChainHeadState = st'
            , recordedAt = AtPoint (fromConsensusPointHF point) st
            }
        -- FIXME: The right thing to do is probably to decouple the observation from the
        -- transformation into an `OnChainTx`
        let event = case (onChainTx, reifyState nextState) of
              (OnCloseTx{snapshotNumber}, TkClosed) ->
                let remainingTimeWithBuffer = 1 + diffUTCTime (posixToUTCTime $ getContestationDeadline nextState) now
                 in OnCloseTx{snapshotNumber, remainingContestationPeriod = remainingTimeWithBuffer}
              _ -> onChainTx
        pure $ event : observed
      Nothing ->
        pure observed

-- | Rewind some head state back to the first known state that is strictly
-- before the provided 'ChainPoint'.
--
-- This function also computes the /rollback depth/, e.g the number of observed states
-- that needs to be discarded given some rollback 'chainPoint'. This depth is used
-- in 'Hydra.HeadLogic.rollback' to discard corresponding off-chain state. Consequently
-- the two states /must/ be kept in sync in order for rollbacks to be handled properly.
rollback :: ChainPoint -> SomeOnChainHeadStateAt -> (SomeOnChainHeadStateAt, Word)
rollback chainPoint = backward 0
 where
  backward n st =
    case recordedAt st of
      AtStart ->
        (st, n)
      AtPoint at previous ->
        if at <= chainPoint
          then (st, n)
          else backward (succ n) previous

-- | Hardcoded grace time for close transaction to be valid.
-- TODO: make it a node configuration parameter
closeGraceTime :: SlotNo
closeGraceTime = 100

fromPostChainTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TimeHandle ->
  [VerificationKey PaymentKey] ->
  TinyWallet m ->
  TVar m SomeOnChainHeadStateAt ->
  PostChainTx Tx ->
  STM m Tx
fromPostChainTx timeHandle cardanoKeys wallet someHeadState tx = do
  pointInTime <- throwLeft currentPointInTime
  SomeOnChainHeadState st <- currentOnChainHeadState <$> readTVar someHeadState
  case (tx, reifyState st) of
    (InitTx params, TkIdle) -> do
      getFuelUTxO wallet >>= \case
        Just (fromLedgerTxIn -> seedInput, _) -> do
          pure $ initialize params cardanoKeys seedInput st
        Nothing ->
          throwIO (NoSeedInput @Tx)
    (AbortTx{}, TkInitialized) -> do
      pure (abort st)
    -- NOTE / TODO: 'CommitTx' also contains a 'Party' which seems redundant
    -- here. The 'Party' is already part of the state and it is the only party
    -- which can commit from this Hydra node.
    (CommitTx{committed}, TkInitialized) -> do
      either throwIO pure (commit committed st)
    -- TODO: We do not rely on the utxo from the collect com tx here because the
    -- chain head-state is already tracking UTXO entries locked by commit scripts,
    -- and thus, can re-construct the committed UTXO for the collectComTx from
    -- the commits' datums.
    --
    -- Perhaps we do want however to perform some kind of sanity check to ensure
    -- that both states are consistent.
    (CollectComTx{}, TkInitialized) -> do
      pure (collect st)
    (CloseTx{confirmedSnapshot}, TkOpen) -> do
      shifted <- throwLeft $ adjustPointInTime closeGraceTime pointInTime
      pure (close confirmedSnapshot shifted st)
    (ContestTx{confirmedSnapshot}, TkClosed) -> do
      shifted <- throwLeft $ adjustPointInTime closeGraceTime pointInTime
      pure (contest confirmedSnapshot shifted st)
    (FanoutTx{utxo}, TkClosed) -> do
      -- NOTE: It's a bit weir that we inspect the state here, but handing
      -- errors around while we want the possibly failing "time -> slot"
      -- conversion to be done here is not prettier.
      deadlineSlot <- throwLeft . slotFromPOSIXTime $ getContestationDeadline st
      pure (fanout utxo deadlineSlot st)
    (_, _) ->
      throwIO $ InvalidStateToPost tx
 where
  -- XXX: Might want a dedicated exception type here
  throwLeft = either (throwSTM . userError . toString) pure

  TimeHandle{currentPointInTime, adjustPointInTime, slotFromPOSIXTime} = timeHandle

--
-- Helpers
--

-- | This extract __Babbage__ transactions from a block. If the block wasn't
-- produced in the Babbage era, it returns an empty sequence.
getBabbageTxs :: Block -> StrictSeq (ValidatedTx LedgerEra)
getBabbageTxs = \case
  BlockBabbage (ShelleyBlock (Ledger.Block _ txsSeq) _) ->
    fromTxSeq txsSeq
  _ ->
    mempty

--
-- Tracing
--

data DirectChainLog
  = ToPost {toPost :: PostChainTx Tx}
  | PostingTx {postedTx :: (TxId StandardCrypto, ValidatedTx LedgerEra)}
  | PostedTx {postedTxId :: TxId StandardCrypto}
  | ReceivedTxs {onChainTxs :: [OnChainTx Tx], receivedTxs :: [(TxId StandardCrypto, ValidatedTx LedgerEra)]}
  | RolledBackward {point :: SomePoint}
  | Wallet TinyWalletLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary DirectChainLog where
  arbitrary = genericArbitrary
