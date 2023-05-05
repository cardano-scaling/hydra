{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.HandlersSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (..), newTVarIO)
import Control.Tracer (nullTracer)
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  BlockHeader (..),
  ChainPoint,
  SlotNo (..),
  Tx,
  chainPointToSlotNo,
  genTxIn,
  getChainPoint,
 )
import Hydra.Chain (
  ChainCallback,
  ChainEvent (..),
  ChainSlot(..),
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
import Hydra.Options (maximumNumberOfParties)
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
      TestBlock header txs <- pickBlind $ genBlockAt slot []

      chainContext <- pickBlind arbitrary
      chainState <- pickBlind arbitrary

      (handler, getEvents) <- run $ recordEventsHandler chainContext chainState (pure timeHandle)

      run $ onRollForward handler header txs

      events <- run getEvents
      monitor $ counterexample ("events: " <> show events)

      expectedUTCTime <-
        run $
          either (failure . ("Time conversion failed: " <>) . toString) pure $
            slotToUTCTime timeHandle slot

      void . stop $ events === [Tick expectedUTCTime (ChainSlot . fromIntegral . unSlotNo $ slot)]

  prop "roll forward fails with outdated TimeHandle" $
    monadicIO $ do
      (timeHandle, slot) <- pickBlind genTimeHandleWithSlotPastHorizon
      TestBlock header txs <- pickBlind $ genBlockAt slot []

      chainContext <- pickBlind arbitrary
      let chainSyncCallback = \_cont -> failure "Unexpected callback"
          handler = chainSyncHandler nullTracer chainSyncCallback (pure timeHandle) chainContext

      run $
        onRollForward handler header txs
          `shouldThrow` \TimeConversionException{slotNo} -> slotNo == slot

  prop "yields observed transactions rolling forward" . monadicIO $ do
    -- Generate a state and related transaction and a block containing it
    (ctx, st, tx, transition) <- pick genChainStateWithTx
    let chainState = ChainStateAt{chainState = st, recordedAt = Nothing}
    TestBlock header txs <- pickBlind $ genBlockAt 1 [tx]
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
    run $ onRollForward handler header txs

  prop "yields rollback events onRollBackward" . monadicIO $ do
    (chainContext, chainState, blocks) <- pickBlind genSequenceOfObservableBlocks
    rollbackPoint <- pick $ genRollbackPoint blocks
    monitor $ label ("Rollback to: " <> show (chainPointToSlotNo rollbackPoint) <> " / " <> show (length blocks))
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

    let handler =
          chainSyncHandler
            nullTracer
            callback
            (pure timeHandle)
            chainContext
    -- Simulate some chain following
    run $ forM_ blocks $ \(TestBlock header txs) -> onRollForward handler header txs
    -- Inject the rollback to somewhere between any of the previous state
    result <- run $ try @_ @SomeException $ onRollBackward handler rollbackPoint
    monitor . counterexample $ "try onRollBackward: " <> show result
    assert $ isRight result

    mSlot <- run . atomically $ tryReadTMVar rolledBackTo
    monitor . counterexample $ "rolledBackTo: " <> show mSlot
    assert $ mSlot == Just (chainSlotFromPoint rollbackPoint)

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
genRollbackPoint blocks = do
  TestBlock header _ <- elements blocks
  pure $ getChainPoint header

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
  let chainState =
        ChainStateAt
          { chainState = Idle
          , recordedAt = Nothing
          }
  pure (cctx, chainState, reverse blks)
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
