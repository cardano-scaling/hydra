{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.HandlersSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Ledger.Block as Ledger
import Cardano.Ledger.Era (toTxSeq)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad.Class.MonadSTM (MonadSTM (..), newTVarIO)
import Control.Tracer (nullTracer)
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import Hydra.Cardano.Api (
  SlotNo (..),
  Tx,
  toLedgerTx,
 )
import Hydra.Chain (
  ChainCallback,
  ChainEvent (..),
  ChainSlot (..),
  HeadParameters,
 )
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  GetTimeHandle,
  TimeConversionException (..),
  chainSyncHandler,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (Idle),
  ChainStateAt (..),
  InitialState (..),
  ctxHeadParameters,
  deriveChainContexts,
  genChainStateWithTx,
  genCommit,
  genHydraContext,
  initialize,
  observeCommit,
  observeSomeTx,
  unsafeCommit,
  unsafeObserveInit,
 )
import Hydra.Chain.Direct.TimeHandle (TimeHandle (slotToUTCTime), TimeHandleParams (..), genTimeParams, mkTimeHandle)
import Hydra.Chain.Direct.Util (Block)
import Hydra.Ledger.Cardano (genTxIn)
import Hydra.Options (maximumNumberOfParties)
import Ouroboros.Consensus.Block (Point (BlockPoint, GenesisPoint), blockPoint, pointSlot)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (
  counterexample,
  elements,
  label,
  (===),
 )
import Test.QuickCheck.Monadic (
  PropertyM,
  assert,
  monadicIO,
  monitor,
  pick,
  run,
  stop,
 )

genTimeHandleWithSlotInsideHorizon :: Gen (TimeHandle, SlotNo)
genTimeHandleWithSlotInsideHorizon = do
  TimeHandleParams{systemStart, eraHistory, horizonSlot, currentSlot} <- genTimeParams
  let timeHandle = mkTimeHandle currentSlot systemStart eraHistory
  pure (timeHandle, horizonSlot - 1)

genTimeHandleWithSlotPastHorizon :: Gen (TimeHandle, SlotNo)
genTimeHandleWithSlotPastHorizon = do
  TimeHandleParams{systemStart, eraHistory, horizonSlot, currentSlot} <- genTimeParams
  let timeHandle = mkTimeHandle currentSlot systemStart eraHistory
  pure (timeHandle, horizonSlot + 1)

spec :: Spec
spec = do
  prop "roll forward results in Tick events" $
    monadicIO $ do
      (timeHandle, slot) <- pickBlind genTimeHandleWithSlotInsideHorizon
      blk <- pickBlind $ genBlockAt slot []

      chainContext <- pickBlind arbitrary
      chainState <- pickBlind arbitrary
      (handler, getEvents) <- run $ recordEventsHandler chainContext chainState (pure timeHandle)

      run $ onRollForward handler blk

      events <- run getEvents
      monitor $ counterexample ("events: " <> show events)

      expectedUTCTime <-
        run $
          either (failure . ("Time conversion failed: " <>) . toString) pure $
            slotToUTCTime timeHandle slot
      void . stop $ events === [Tick expectedUTCTime]

  prop "roll forward fails with outdated TimeHandle" $
    monadicIO $ do
      (timeHandle, slot) <- pickBlind genTimeHandleWithSlotPastHorizon
      blk <- pickBlind $ genBlockAt slot []

      chainContext <- pickBlind arbitrary
      let chainSyncCallback = \_cont -> failure "Unexpected callback"
          handler = chainSyncHandler nullTracer chainSyncCallback (pure timeHandle) chainContext

      run $
        onRollForward handler blk
          `shouldThrow` \TimeConversionException{slotNo} -> slotNo == slot

  prop "yields observed transactions rolling forward" . monadicIO $ do
    -- Generate a state and related transaction and a block containing it
    (ctx, st, tx, transition) <- pick genChainStateWithTx
    let chainState = ChainStateAt{chainState = st, recordedAt = Nothing}
    blk <- pickBlind $ genBlockAt 1 [tx]
    monitor (label $ show transition)

    timeHandle <- pickBlind arbitrary
    let callback cont =
          -- Give chain state in which we expect the 'tx' to yield an 'Observation'.
          case cont chainState of
            Nothing ->
              -- XXX: We need this to debug as 'failure' (via 'run') does not
              -- yield counter examples.
              failure . toString $
                unlines
                  [ "expected continuation to yield an event"
                  , "transition: " <> show transition
                  , "chainState: " <> show st
                  ]
            Just Rollback{} ->
              failure "rolled back but expected roll forward."
            Just Tick{} -> pure ()
            Just Observation{observedTx} ->
              fst <$> observeSomeTx ctx st tx `shouldBe` Just observedTx

    let handler = chainSyncHandler nullTracer callback (pure timeHandle) ctx
    run $ onRollForward handler blk

  prop "yields rollback events onRollBackward" . monadicIO $ do
    (chainContext, chainState, blocks) <- pickBlind genSequenceOfObservableBlocks
    (rollbackSlot, rollbackPoint) <- pick $ genRollbackPoint blocks
    monitor $ label ("Rollback to: " <> show rollbackSlot <> " / " <> show (length blocks))
    timeHandle <- pickBlind arbitrary
    -- Mock callback which keeps the chain state in a tvar
    stateVar <- run $ newTVarIO chainState
    rolledBackTo <- run newEmptyTMVarIO
    let callback cont = do
          cs <- readTVarIO stateVar
          case cont cs of
            Nothing -> do
              failure "expected continuation to yield observation"
            Just Tick{} -> pure ()
            Just (Rollback slot) -> atomically $ putTMVar rolledBackTo slot
            Just Observation{newChainState} -> atomically $ writeTVar stateVar newChainState
    let handler = chainSyncHandler nullTracer callback (pure timeHandle) chainContext
    -- Simulate some chain following
    run $ mapM_ (onRollForward handler) blocks
    -- Inject the rollback to somewhere between any of the previous state
    result <- run $ try @_ @SomeException $ onRollBackward handler rollbackPoint
    monitor . counterexample $ "try onRollBackward: " <> show result
    assert $ isRight result

    mSlot <- run . atomically $ tryReadTMVar rolledBackTo
    monitor . counterexample $ "rolledBackTo: " <> show mSlot
    assert $ mSlot == Just rollbackSlot

-- | Create a chain sync handler which records events as they are called back.
-- NOTE: This 'ChainSyncHandler' does not handle chain state updates, but uses
-- the given 'ChainState' constantly.
recordEventsHandler :: ChainContext -> ChainStateAt -> GetTimeHandle IO -> IO (ChainSyncHandler IO, IO [ChainEvent Tx])
recordEventsHandler ctx cs getTimeHandle = do
  eventsVar <- newTVarIO []
  let handler = chainSyncHandler nullTracer (recordEvents eventsVar) getTimeHandle ctx
  pure (handler, getEvents eventsVar)
 where
  getEvents = readTVarIO

  recordEvents :: TVar IO [ChainEvent Tx] -> ChainCallback Tx IO
  recordEvents var cont = do
    case cont cs of
      Nothing -> pure ()
      Just e -> atomically $ modifyTVar var (e :)

withCounterExample :: [Block] -> TVar IO ChainStateAt -> IO a -> PropertyM IO a
withCounterExample blks headState step = do
  stBefore <- run $ readTVarIO headState
  a <- run step
  stAfter <- run $ readTVarIO headState
  a <$ do
    monitor $
      counterexample $
        toString $
          unlines
            [ "Chain state at (before rollback): " <> show stBefore
            , "Chain state at (after rollback):  " <> show stAfter
            , "Block sequence: \n"
                <> unlines
                  ( fmap
                      ("    " <>)
                      [show (blockPoint blk) | blk <- blks]
                  )
            ]

genBlockAt :: SlotNo -> [Tx] -> Gen Block
genBlockAt sl txs = do
  header <- adjustSlot <$> arbitrary
  let body = toTxSeq $ StrictSeq.fromList (toLedgerTx <$> txs)
  pure $ BlockBabbage $ mkShelleyBlock $ Ledger.Block header body
 where
  adjustSlot (Praos.Header body sig) =
    let body' = body{Praos.hbSlotNo = sl}
     in Praos.Header body' sig

-- | Pick a block point in a list of blocks and return it along with the
-- corresponding 'ChainSlot'.
genRollbackPoint :: [Block] -> Gen (ChainSlot, Point Block)
genRollbackPoint blocks = do
  block <- elements blocks
  let rollbackPoint = blockPoint block
  let slot = case rollbackPoint of
        GenesisPoint -> ChainSlot 0
        BlockPoint (SlotNo s) _ -> ChainSlot $ fromIntegral s
  pure (slot, rollbackPoint)

-- | Generate a non-sparse sequence of blocks each containing an observable
-- transaction, starting from the returned on-chain head state.
--
-- Note that this does not generate the entire spectrum of observable
-- transactions in Hydra, but only init and commits, which is already sufficient
-- to observe at least one state transition and different levels of rollback.
genSequenceOfObservableBlocks :: Gen (ChainContext, ChainStateAt, [Block])
genSequenceOfObservableBlocks = do
  ctx <- genHydraContext maximumNumberOfParties
  -- NOTE: commits must be generated from each participant POV, and thus, we
  -- need all their respective ChainContext to move on.
  allContexts <- deriveChainContexts ctx
  -- Pick a peer context which will perform the init
  cctx <- elements allContexts
  blks <- flip execStateT [] $ do
    initTx <- stepInit cctx (ctxHeadParameters ctx)
    -- Commit using all contexts
    void $ stepCommits initTx allContexts
  let chainState =
        ChainStateAt
          { chainState = Idle
          , recordedAt = Nothing
          }
  pure (cctx, chainState, reverse blks)
 where
  nextSlot :: Monad m => StateT [Block] m SlotNo
  nextSlot = do
    get <&> \case
      [] -> 1
      x : _ -> SlotNo . succ . unSlotNo . blockSlotNo $ x

  blockSlotNo pt =
    case pointSlot (blockPoint pt) of
      Origin -> 0
      At sl -> sl

  putNextBlock :: Tx -> StateT [Block] Gen ()
  putNextBlock tx = do
    sl <- nextSlot
    blk <- lift $ genBlockAt sl [tx]
    modify' (blk :)

  stepInit ::
    ChainContext ->
    HeadParameters ->
    StateT [Block] Gen Tx
  stepInit ctx params = do
    initTx <- lift $ initialize ctx params <$> genTxIn
    initTx <$ putNextBlock initTx

  stepCommits ::
    Tx ->
    [ChainContext] ->
    StateT [Block] Gen [InitialState]
  stepCommits initTx = \case
    [] ->
      pure []
    ctx : rest -> do
      stInitialized <- stepCommit ctx initTx
      (stInitialized :) <$> stepCommits initTx rest

  stepCommit ::
    ChainContext ->
    Tx ->
    StateT [Block] Gen InitialState
  stepCommit ctx initTx = do
    let stInitial = unsafeObserveInit ctx initTx
    utxo <- lift genCommit
    let commitTx = unsafeCommit ctx stInitial utxo
    putNextBlock commitTx
    pure $ snd $ fromJust $ observeCommit ctx stInitial commitTx

showRollbackInfo :: (Word, Point Block) -> String
showRollbackInfo (rollbackDepth, rollbackPoint) =
  toString $
    unlines
      [ "Rollback depth: " <> show rollbackDepth
      , "Rollback point: " <> show rollbackPoint
      ]
