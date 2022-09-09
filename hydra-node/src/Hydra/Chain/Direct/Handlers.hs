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
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (readTVarIO, throwSTM, writeTVar)
import Data.Sequence.Strict (StrictSeq)
import Hydra.Cardano.Api (
  ChainPoint (..),
  LedgerEra,
  SlotNo,
  Tx,
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
  ChainState (Closed, Idle, Initial, Open),
  IdleState (IdleState, ctx),
  abort,
  close,
  collect,
  commit,
  contest,
  fanout,
  getKnownUTxO,
  initialize,
  observeSomeTx,
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
  -- | Means to acquire a new 'TimeHandle'.
  m TimeHandle ->
  TinyWallet m ->
  TVar m ChainStateAt ->
  SubmitTx m ->
  Chain Tx m
mkChain tracer queryTimeHandle wallet headState submitTx =
  Chain
    { postTx = \tx -> do
        traceWith tracer $ ToPost{toPost = tx}
        timeHandle <- queryTimeHandle
        vtx <-
          atomically
            ( -- FIXME (MB): cardano keys should really not be here (as this
              -- point they are in the 'headState' stored in the 'ChainContext')
              -- . They are only required for the init transaction and ought to
              -- come from the _client_ and be part of the init request
              -- altogether. This goes in the direction of 'dynamic heads' where
              -- participants aren't known upfront but provided via the API.
              -- Ultimately, an init request from a client would contain all the
              -- details needed to establish connection to the other peers and
              -- to bootstrap the init transaction. For now, we bear with it and
              -- keep the static keys in context.
              fromPostChainTx timeHandle wallet headState tx
                >>= finalizeTx wallet headState . toLedgerTx
            )
        submitTx vtx
    }

-- | Balance and sign the given partial transaction.
finalizeTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TinyWallet m ->
  TVar m ChainStateAt ->
  ValidatedTx LedgerEra ->
  STM m (ValidatedTx LedgerEra)
finalizeTx TinyWallet{sign, getUTxO, coverFee} headState partialTx = do
  someSt <- currentChainState <$> readTVar headState
  let headUTxO = getKnownUTxO someSt
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
data ChainStateAt = ChainStateAt
  { currentChainState :: ChainState
  , recordedAt :: RecordedAt
  }
  deriving (Eq, Show)

-- | Records when a state was seen on-chain. 'AtStart' is used for states that
-- simply exist out of any chain events (e.g. the 'Idle' state).
data RecordedAt
  = AtStart
  | AtPoint ChainPoint ChainStateAt
  deriving (Eq, Show)

-- | A /handler/ that takes care of following the chain.
data ChainSyncHandler m = ChainSyncHandler
  { onRollForward :: Block -> m ()
  , onRollBackward :: Point Block -> m ()
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
-- Throws 'TimeConversionException' when a received block's 'SlotNo' cannot be
-- converted to a 'UTCTime' with the given 'TimeHandle'.
chainSyncHandler ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  -- | Tracer for logging
  Tracer m DirectChainLog ->
  -- | Chain callback
  (ChainEvent Tx -> m ()) ->
  -- | On-chain head-state.
  TVar m ChainStateAt ->
  -- | A handle on time to do `SlotNo -> POSIXTime` conversions for 'Tick' events.
  TimeHandle ->
  -- | A chain-sync handler to use in a local-chain-sync client.
  ChainSyncHandler m
chainSyncHandler tracer callback headState timeHandle =
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
    let point = blockPoint blk
    traceWith tracer $ RolledForward $ SomePoint point

    let chainPoint = fromConsensusPointHF point
        slotNo = case chainPoint of
          ChainPointAtGenesis -> 0
          ChainPoint s _ -> s
    case slotToUTCTime timeHandle slotNo of
      Left reason ->
        throwIO TimeConversionException{slotNo, reason}
      Right utcTime ->
        callback (Tick utcTime)

    let receivedTxs = toList $ getBabbageTxs blk
    onChainTxs <- reverse <$> atomically (foldM (withNextTx chainPoint) [] receivedTxs)
    unless (null receivedTxs) $
      traceWith tracer $
        ReceivedTxs
          { onChainTxs
          , receivedTxs = map getTxId receivedTxs
          }
    mapM_ (callback . Observation) onChainTxs

  withNextTx :: ChainPoint -> [OnChainTx Tx] -> ValidatedTx LedgerEra -> STM m [OnChainTx Tx]
  withNextTx point observed (fromLedgerTx -> tx) = do
    st <- readTVar headState
    case observeSomeTx tx (currentChainState st) of
      Just (event, st') -> do
        writeTVar headState $
          ChainStateAt
            { currentChainState = st'
            , recordedAt = AtPoint point st
            }
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
rollback :: ChainPoint -> ChainStateAt -> (ChainStateAt, Word)
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
  TinyWallet m ->
  TVar m ChainStateAt ->
  PostChainTx Tx ->
  STM m Tx
fromPostChainTx timeHandle wallet someHeadState tx = do
  pointInTime <- throwLeft currentPointInTime
  cst <- currentChainState <$> readTVar someHeadState
  case (tx, cst) of
    (InitTx params, Idle IdleState{ctx}) ->
      getFuelUTxO wallet >>= \case
        Just (fromLedgerTxIn -> seedInput, _) -> do
          pure $ initialize ctx params seedInput
        Nothing ->
          throwIO (NoSeedInput @Tx)
    (AbortTx{}, Initial st) ->
      pure $ abort st
    -- NOTE / TODO: 'CommitTx' also contains a 'Party' which seems redundant
    -- here. The 'Party' is already part of the state and it is the only party
    -- which can commit from this Hydra node.
    (CommitTx{committed}, Initial st) ->
      either throwIO pure (commit st committed)
    -- TODO: We do not rely on the utxo from the collect com tx here because the
    -- chain head-state is already tracking UTXO entries locked by commit scripts,
    -- and thus, can re-construct the committed UTXO for the collectComTx from
    -- the commits' datums.
    --
    -- Perhaps we do want however to perform some kind of sanity check to ensure
    -- that both states are consistent.
    (CollectComTx{}, Initial st) ->
      pure $ collect st
    (CloseTx{confirmedSnapshot}, Open st) -> do
      shifted <- throwLeft $ adjustPointInTime closeGraceTime pointInTime
      pure (close st confirmedSnapshot shifted)
    (ContestTx{confirmedSnapshot}, Closed st) -> do
      shifted <- throwLeft $ adjustPointInTime closeGraceTime pointInTime
      pure (contest st confirmedSnapshot shifted)
    (FanoutTx{utxo, contestationDeadline}, Closed st) -> do
      deadlineSlot <- throwLeft $ slotFromUTCTime contestationDeadline
      pure (fanout st utxo deadlineSlot)
    (_, _) -> throwIO $ InvalidStateToPost tx
 where
  -- XXX: Might want a dedicated exception type here
  throwLeft = either (throwSTM . userError . toString) pure

  TimeHandle{currentPointInTime, adjustPointInTime, slotFromUTCTime} = timeHandle

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
  | PostingTx {txId :: Ledger.TxId StandardCrypto}
  | PostedTx {txId :: Ledger.TxId StandardCrypto}
  | PostingFailed {tx :: ValidatedTx LedgerEra, postTxError :: PostTxError Tx}
  | ReceivedTxs {onChainTxs :: [OnChainTx Tx], receivedTxs :: [Ledger.TxId StandardCrypto]}
  | RolledForward {point :: SomePoint}
  | RolledBackward {point :: SomePoint}
  | Wallet TinyWalletLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary DirectChainLog where
  arbitrary = genericArbitrary
