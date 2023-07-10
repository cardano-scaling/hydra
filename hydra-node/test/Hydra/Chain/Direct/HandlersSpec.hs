{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.HandlersSpec where

import Hydra.Prelude hiding (label)

import Control.Concurrent.Class.MonadSTM (MonadSTM (..), newTVarIO)
import Control.Tracer (nullTracer)
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  BlockHeader (..),
  ChainPoint (ChainPointAtGenesis),
  SlotNo (..),
  Tx,
  genTxIn,
  getChainPoint,
 )

import Hydra.Chain (
  ChainEvent (..),
  HeadParameters,
  chainStateSlot,
 )
import Hydra.Chain.Direct (initialChainState)
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  GetTimeHandle,
  TimeConversionException (..),
  chainSyncHandler,
  getLatest,
  newLocalChainState,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainStateAt (..),
  HydraContext,
  InitialState (..),
  chainSlotFromPoint,
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
import Hydra.HeadLogic (HeadStateEvent (..), toChainStateEvents)
import Hydra.Ledger (
  ChainSlot (..),
 )
import Hydra.Options (maximumNumberOfParties)
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncrementalView)
import Hydra.Snapshot (ConfirmedSnapshot (..))
import Test.Consensus.Cardano.Generators ()
import Test.Hydra.Prelude
import Test.QuickCheck (
  counterexample,
  elements,
  label,
  oneof,
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
  describe "chainSyncHanlder" $ do
    prop "roll forward results in Tick events" $ do
      monadicIO $ do
        (timeHandle, slot) <- pickBlind genTimeHandleWithSlotInsideHorizon
        TestBlock header txs <- pickBlind $ genBlockAt slot []

        chainContext <- pickBlind arbitrary
        chainState <- pickBlind arbitrary
        localChainState <- run $ newLocalChainState chainState

        chainStateEvents <- run $ newTVarIO []
        let persistence =
              PersistenceIncremental
                { append = \event -> do
                    atomically $ modifyTVar chainStateEvents (event :)
                , loadAll = readTVarIO chainStateEvents
                }

        (handler, getEvents) <- run $ recordEventsHandler chainContext chainState (pure timeHandle) persistence

        run $ do
          onRollForward handler header txs
          latest' <- atomically $ getLatest localChainState
          -- XXX: Here we need to simulate events being persisted during onRollForward.
          -- Also note that we do not care the head-state event wrapping the chain-state.
          append persistence (HeadOpened (InitialSnapshot mempty) mempty latest')

        events <- run getEvents
        monitor $ counterexample ("events: " <> show events)

        expectedUTCTime <-
          run $
            either (failure . ("Time conversion failed: " <>) . toString) pure $
              slotToUTCTime timeHandle slot
        void . stop $ events === [Tick expectedUTCTime (ChainSlot . fromIntegral $ unSlotNo slot)]

    prop "roll forward fails with outdated TimeHandle" $
      monadicIO $ do
        (timeHandle, slot) <- pickBlind genTimeHandleWithSlotPastHorizon
        TestBlock header txs <- pickBlind $ genBlockAt slot []

        chainContext <- pickBlind arbitrary
        chainState <- pickBlind arbitrary
        localChainState <- run $ newLocalChainState chainState

        chainStateEvents <- run $ newTVarIO []
        let persistence =
              PersistenceIncremental
                { append = \event -> do
                    atomically $ modifyTVar chainStateEvents (event :)
                , loadAll = readTVarIO chainStateEvents
                }
        let persistenceView = createPersistenceIncrementalView persistence toChainStateEvents
        let chainSyncCallback = \_cont -> failure "Unexpected callback"
            handler =
              chainSyncHandler
                nullTracer
                chainSyncCallback
                (pure timeHandle)
                chainContext
                localChainState
                persistenceView
        run $ do
          onRollForward handler header txs
            `shouldThrow` \TimeConversionException{slotNo} -> slotNo == slot
          latest' <- atomically $ getLatest localChainState
          -- XXX: Here we need to simulate events being persisted during onRollForward.
          -- Also note that we do not care the head-state event wrapping the chain-state.
          append persistence (HeadOpened (InitialSnapshot mempty) mempty latest')

    prop "observes transactions onRollForward" . monadicIO $ do
      -- Generate a state and related transaction and a block containing it
      (ctx, st, tx, transition) <- pick genChainStateWithTx
      TestBlock header txs <- pickBlind $ genBlockAt 1 [tx]
      monitor (label $ show transition)
      localChainState <-
        run $
          newLocalChainState
            ChainStateAt
              { chainState = st
              , recordedAt = Nothing
              }
      timeHandle <- pickBlind arbitrary
      let callback = \case
            Rollback{} ->
              failure "rolled back but expected roll forward."
            Tick{} -> pure ()
            Observation{observedTx} ->
              if (fst <$> observeSomeTx ctx st tx) /= Just observedTx
                then failure $ show (fst <$> observeSomeTx ctx st tx) <> " /= " <> show (Just observedTx)
                else pure ()

      chainStateEvents <- run $ newTVarIO []
      let persistence =
            PersistenceIncremental
              { append = \event -> do
                  atomically $ modifyTVar chainStateEvents (event :)
              , loadAll = readTVarIO chainStateEvents
              }
      let persistenceView = createPersistenceIncrementalView persistence toChainStateEvents
      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              ctx
              localChainState
              persistenceView
      run $ do
        onRollForward handler header txs
        latest' <- atomically $ getLatest localChainState
        -- XXX: Here we need to simulate events being persisted during onRollForward.
        -- Also note that we do not care the head-state event wrapping the chain-state.
        append persistence (HeadOpened (InitialSnapshot mempty) mempty latest')

    prop "rollbacks state onRollBackward" . monadicIO $ do
      (chainContext, chainStateAt, blocks) <- pickBlind genSequenceOfObservableBlocks
      rollbackPoint <- pick $ genRollbackPoint blocks
      monitor $ label ("Rollback to: " <> show (chainSlotFromPoint rollbackPoint) <> " / " <> show (length blocks))
      timeHandle <- pickBlind arbitrary

      -- Stub for recording Rollback events
      rolledBackTo <- run newEmptyTMVarIO
      let callback = \case
            Rollback{rolledBackChainState} ->
              atomically $ putTMVar rolledBackTo rolledBackChainState
            _ -> pure ()
      localChainState <- run $ newLocalChainState chainStateAt

      chainStateEvents <- run $ newTVarIO []
      let persistence =
            PersistenceIncremental
              { append = \event -> do
                  atomically $ modifyTVar chainStateEvents (event :)
              , loadAll = readTVarIO chainStateEvents
              }
      let persistenceView = createPersistenceIncrementalView persistence toChainStateEvents
      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              chainContext
              localChainState
              persistenceView

      -- Simulate some chain following
      run $ forM_ blocks $ \(TestBlock header txs) -> do
        onRollForward handler header txs
        latest' <- atomically $ getLatest localChainState
        -- XXX: Here we need to simulate events being persisted during onRollForward.
        -- Also note that we do not care the head-state event wrapping the chain-state.
        append persistence (HeadOpened (InitialSnapshot mempty) mempty latest')

      -- Inject the rollback to somewhere between any of the previous state
      result <- run $ try @_ @SomeException $ onRollBackward handler rollbackPoint
      monitor . counterexample $ "try onRollBackward: " <> show result
      assert $ isRight result

      mRolledBackChainState <- run . atomically $ tryReadTMVar rolledBackTo
      monitor . counterexample $ "rolledBackTo: " <> show mRolledBackChainState
      pure $ (chainStateSlot <$> mRolledBackChainState) === Just (chainSlotFromPoint rollbackPoint)

  describe "LocalChainState" $ do
    prop "can resume from chain state" . monadicIO $ do
      (chainContext, chainStateAt, blocks) <- pickBlind genSequenceOfObservableBlocks
      timeHandle <- pickBlind arbitrary
      -- Use the handler to evolve the chain state to some new, latest version
      localChainState <- run $ newLocalChainState chainStateAt

      chainStateEvents <- run $ newTVarIO []
      let persistence =
            PersistenceIncremental
              { append = \event -> do
                  atomically $ modifyTVar chainStateEvents (event :)
              , loadAll = readTVarIO chainStateEvents
              }
      let persistenceView = createPersistenceIncrementalView persistence toChainStateEvents
      let handler =
            chainSyncHandler
              nullTracer
              (\_ -> pure ())
              (pure timeHandle)
              chainContext
              localChainState
              persistenceView

      run $ forM_ blocks $ \(TestBlock header txs) -> do
        onRollForward handler header txs
        latest' <- atomically $ getLatest localChainState
        -- XXX: Here we need to simulate events being persisted during onRollForward.
        -- Also note that we do not care the head-state event wrapping the chain-state.
        append persistence (HeadOpened (InitialSnapshot mempty) mempty latest')

      latestChainState <- run . atomically $ getLatest localChainState

      assert $ latestChainState /= chainStateAt

      -- Provided the latest chain state the LocalChainState must be able to
      -- rollback and forward
      resumedLocalChainState <- run $ newLocalChainState latestChainState
      let resumedHandler =
            chainSyncHandler
              nullTracer
              (\_ -> pure ())
              (pure timeHandle)
              chainContext
              resumedLocalChainState
              persistenceView

      (rollbackPoint, blocksAfter) <- pickBlind $ genRollbackBlocks blocks
      monitor $ label $ "Rollback " <> show (length blocksAfter) <> " blocks"

      run $ onRollBackward resumedHandler rollbackPoint
      -- NOTE: Sanity check that the rollback was affecting the local state
      rolledBackChainState <- run . atomically $ getLatest resumedLocalChainState
      assert $ null blocksAfter || rolledBackChainState /= latestChainState

      run $ forM_ blocksAfter $ \(TestBlock header txs) -> do
        onRollForward resumedHandler header txs
        latest' <- atomically $ getLatest localChainState
        -- XXX: Here we need to simulate events being persisted during onRollForward.
        -- Also note that we do not care the head-state event wrapping the chain-state.
        append persistence (HeadOpened (InitialSnapshot mempty) mempty latest')

      latestResumedChainState <- run . atomically $ getLatest resumedLocalChainState
      pure $ latestResumedChainState === latestChainState

-- | Create a chain sync handler which records events as they are called back.
recordEventsHandler ::
  ChainContext ->
  ChainStateAt ->
  GetTimeHandle IO ->
  PersistenceIncremental (HeadStateEvent Tx) IO ->
  IO (ChainSyncHandler IO, IO [ChainEvent Tx])
recordEventsHandler ctx cs getTimeHandle persistence = do
  eventsVar <- newTVarIO []
  localChainState <- newLocalChainState cs
  let persistenceView = createPersistenceIncrementalView persistence toChainStateEvents
  let handler = chainSyncHandler nullTracer (recordEvents eventsVar) getTimeHandle ctx localChainState persistenceView
  pure (handler, getEvents eventsVar)
 where
  getEvents = readTVarIO

  recordEvents var event = do
    atomically $ modifyTVar var (event :)

-- | A block used for testing. This is a simpler version of the cardano-api
-- 'Block' and can be de-/constructed easily.
data TestBlock = TestBlock BlockHeader [Tx]

withCounterExample :: [TestBlock] -> TVar IO ChainStateAt -> IO a -> PropertyM IO a
withCounterExample blocks headState step = do
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
                      [show (getChainPoint header) | TestBlock header _ <- blocks]
                  )
            ]

-- | Thin wrapper which generates a 'TestBlock' at some specific slot.
genBlockAt :: SlotNo -> [Tx] -> Gen TestBlock
genBlockAt sl txs = do
  header <- adjustSlot <$> arbitrary
  pure $ TestBlock header txs
 where
  adjustSlot (BlockHeader _ hash blockNo) =
    BlockHeader sl hash blockNo

-- | Pick a block point in a list of blocks.
genRollbackPoint :: [TestBlock] -> Gen ChainPoint
genRollbackPoint blocks =
  oneof
    [ pickFromBlocks
    , pure ChainPointAtGenesis
    ]
 where
  pickFromBlocks = do
    TestBlock header _ <- elements blocks
    pure $ getChainPoint header

-- | Pick a rollback point from a list of blocks and also yield the tail of
-- blocks to be replayed.
genRollbackBlocks :: [TestBlock] -> Gen (ChainPoint, [TestBlock])
genRollbackBlocks blocks =
  oneof
    [ pickFromBlocks
    , rollbackFromGenesis
    ]
 where
  rollbackFromGenesis =
    pure (ChainPointAtGenesis, blocks)

  pickFromBlocks = do
    toReplay <- elements $ tails blocks
    case toReplay of
      [] -> rollbackFromGenesis
      ((TestBlock header _) : blocksAfter) ->
        pure (getChainPoint header, blocksAfter)

-- | Generate a non-sparse sequence of blocks each containing an observable
-- transaction, starting from the returned on-chain head state.
--
-- Note that this does not generate the entire spectrum of observable
-- transactions in Hydra, but only init and commits, which is already sufficient
-- to observe at least one state transition and different levels of rollback.
genSequenceOfObservableBlocks :: Gen (ChainContext, ChainStateAt, [TestBlock])
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
    void $ stepCommits ctx initTx allContexts
  pure (cctx, initialChainState, reverse blks)
 where
  nextSlot :: Monad m => StateT [TestBlock] m SlotNo
  nextSlot = do
    get <&> \case
      [] -> 1
      block : _ -> 1 + blockSlotNo block

  blockSlotNo (TestBlock (BlockHeader slotNo _ _) _) = slotNo

  putNextBlock :: Tx -> StateT [TestBlock] Gen ()
  putNextBlock tx = do
    sl <- nextSlot
    blk <- lift $ genBlockAt sl [tx]
    modify' (blk :)

  stepInit ::
    ChainContext ->
    HeadParameters ->
    StateT [TestBlock] Gen Tx
  stepInit ctx params = do
    initTx <- lift $ initialize ctx params <$> genTxIn
    initTx <$ putNextBlock initTx

  stepCommits ::
    HydraContext ->
    Tx ->
    [ChainContext] ->
    StateT [TestBlock] Gen [InitialState]
  stepCommits hydraCtx initTx = \case
    [] ->
      pure []
    ctx : rest -> do
      stInitialized <- stepCommit ctx initTx
      (stInitialized :) <$> stepCommits hydraCtx initTx rest

  stepCommit ::
    ChainContext ->
    Tx ->
    StateT [TestBlock] Gen InitialState
  stepCommit ctx initTx = do
    let stInitial = unsafeObserveInit ctx initTx
    utxo <- lift genCommit
    let commitTx = unsafeCommit ctx stInitial utxo
    putNextBlock commitTx
    pure $ snd $ fromJust $ observeCommit ctx stInitial commitTx

showRollbackInfo :: (Word, ChainPoint) -> String
showRollbackInfo (rollbackDepth, rollbackPoint) =
  toString $
    unlines
      [ "Rollback depth: " <> show rollbackDepth
      , "Rollback point: " <> show rollbackPoint
      ]
