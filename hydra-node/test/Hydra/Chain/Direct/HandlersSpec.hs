{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.HandlersSpec where

import Hydra.Prelude hiding (label)

import Cardano.Ledger.Api (IsValid (..), isValidTxL)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Lens ((.~))
import Control.Tracer (nullTracer)
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  BlockHeader (..),
  ChainPoint (ChainPointAtGenesis),
  PaymentKey,
  SlotNo (..),
  Tx,
  VerificationKey,
  fromLedgerTx,
  genTxIn,
  getChainPoint,
  toLedgerTx,
 )
import Test.Gen.Cardano.Api.Typed (genBlockHeader)
import Test.QuickCheck.Hedgehog (hedgehog)

import Cardano.Ledger.Api (IsValid (..), isValidTxL)
import Control.Lens ((.~))
import Hydra.Chain (ChainEvent (..), OnChainTx (..), currentState, initHistory, maximumNumberOfParties)
import Hydra.Chain.ChainState (ChainSlot (..), chainStateSlot)
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  GetTimeHandle,
  TimeConversionException (..),
  chainSyncHandler,
  getLatest,
  history,
  newLocalChainState,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainStateAt (..),
  HydraContext,
  InitialState (..),
  chainSlotFromPoint,
  ctxHeadParameters,
  ctxParticipants,
  ctxVerificationKeys,
  deriveChainContexts,
  genChainStateWithTx,
  genCommit,
  genHydraContext,
  getKnownUTxO,
  initialChainState,
  initialize,
  observeCommit,
  unsafeCommit,
  unsafeObserveInit,
 )
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.Chain.Direct.TimeHandle (TimeHandle (slotToUTCTime), TimeHandleParams (..), genTimeParams, mkTimeHandle)
import Hydra.Chain.SyncedStatus (unSynced)
import Hydra.Tx.HeadParameters (HeadParameters)
import Hydra.Tx.OnChainId (OnChainId)
import Test.Hydra.Prelude
import Test.QuickCheck (
  counterexample,
  elements,
  label,
  oneof,
  (===),
 )
import Test.QuickCheck.Monadic (
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
  describe "chainSyncHandler" $ do
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
        void . stop $ events === [Tick expectedUTCTime (ChainSlot . fromIntegral $ unSlotNo slot)]

    prop "roll forward fails with outdated TimeHandle" $
      monadicIO $ do
        (timeHandle, slot) <- pickBlind genTimeHandleWithSlotPastHorizon
        TestBlock header txs <- pickBlind $ genBlockAt slot []

        chainContext <- pickBlind arbitrary
        chainState <- pickBlind arbitrary
        localChainState <- run $ newLocalChainState chainState
        let getCurrentTip = pure ChainPointAtGenesis
        tip <- run getCurrentTip
        syncedStatus <- run $ newLabelledTVarIO "chain-sync-status" (unSynced tip)
        let chainSyncCallback :: ChainEvent Tx -> IO ()
            chainSyncCallback = const $ failure "Unexpected callback"
            handler =
              chainSyncHandler
                nullTracer
                chainSyncCallback
                (pure timeHandle)
                chainContext
                localChainState
                syncedStatus
                getCurrentTip
        run $
          onRollForward handler header txs
            `shouldThrow` \TimeConversionException{slotNo} -> slotNo == slot

    prop "observes valid transactions onRollForward" . monadicIO $ do
      -- Generate a state and related transaction and a block containing it
      (ctx, st, utxo', tx, transition) <- pick genChainStateWithTx
      let utxo = getKnownUTxO st <> utxo'
      TestBlock header txs <- pickBlind $ genBlockAt 1 [tx]
      monitor (label $ show transition)
      localChainState <-
        run $ newLocalChainState (initHistory ChainStateAt{spendableUTxO = utxo, recordedAt = Nothing})
      timeHandle <- pickBlind arbitrary
      let callback = \case
            Rollback{} -> failure "rolled back but expected roll forward."
            PostTxError{} -> failure "Unexpected PostTxError event"
            Tick{} -> pure ()
            Observation{observedTx} -> do
              let observedTransition =
                    case observedTx of
                      OnInitTx{} -> Transition.Init
                      OnCommitTx{} -> Transition.Commit
                      OnAbortTx{} -> Transition.Abort
                      OnCollectComTx{} -> Transition.Collect
                      OnDecrementTx{} -> Transition.Decrement
                      OnIncrementTx{} -> Transition.Increment
                      OnCloseTx{} -> Transition.Close
                      OnContestTx{} -> Transition.Contest
                      OnFanoutTx{} -> Transition.Fanout
                      OnDepositTx{} -> error "OnDepositTx not expected"
                      OnRecoverTx{} -> error "OnRecoverTx not expected"
              observedTransition `shouldBe` transition
      let getCurrentTip = pure ChainPointAtGenesis
      tip <- run getCurrentTip
      syncedStatus <- run $ newLabelledTVarIO "chain-sync-status" (unSynced tip)
      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              ctx
              localChainState
              syncedStatus
              getCurrentTip
      run $ onRollForward handler header txs

    prop "ignores invalid transactions onRollForward" . monadicIO $ do
      -- NOTE: Generate a valid state transition, but then mark it as invalid.
      -- This is sufficient to simulate an where and adversary would create a
      -- transaction that looks like a proper transaction, but not entirely and
      -- scripts would fail, but deliberately marks the tx as invalid (only at
      -- the expense of collateral) to trick the hydra-node into thinking the
      -- state transition happened.
      (ctx, st, utxo', validTx, transition) <- pick genChainStateWithTx
      let tx = fromLedgerTx $ toLedgerTx validTx & isValidTxL .~ IsValid False
      let utxo = getKnownUTxO st <> utxo'

      TestBlock header txs <- pickBlind $ genBlockAt 1 [tx]
      monitor (label $ show transition)

      localChainState <-
        run $ newLocalChainState (initHistory ChainStateAt{spendableUTxO = utxo, recordedAt = Nothing})
      timeHandle <- pickBlind arbitrary
      let callback = \case
            Rollback{} -> failure "rolled back but expected roll forward."
            PostTxError{} -> failure "Unexpected PostTxError event"
            Tick{} -> pure ()
            Observation{observedTx} -> failure $ "Unexpected observation: " <> show observedTx
      let getCurrentTip = pure ChainPointAtGenesis
      tip <- run getCurrentTip
      syncedStatus <- run $ newLabelledTVarIO "chain-sync-status" (unSynced tip)
      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              ctx
              localChainState
              syncedStatus
              getCurrentTip
      run $ onRollForward handler header txs

    prop "rollbacks state onRollBackward" . monadicIO $ do
      (chainContext, chainStateAt, blocks) <- pickBlind genSequenceOfObservableBlocks
      rollbackPoint <- pick $ genRollbackPoint blocks
      monitor $ label ("Rollback to: " <> show (chainSlotFromPoint rollbackPoint) <> " / " <> show (length blocks))
      timeHandle <- pickBlind arbitrary

      -- Stub for recording Rollback events
      rolledBackTo <- run $ newLabelledEmptyTMVarIO "rolled-back-to"
      let callback = \case
            Rollback{rolledBackChainState} ->
              atomically $ putTMVar rolledBackTo (initHistory rolledBackChainState)
            _ -> pure ()
      localChainState <- run $ newLocalChainState (initHistory chainStateAt)
      let getCurrentTip = pure ChainPointAtGenesis
      tip <- run getCurrentTip
      syncedStatus <- run $ newLabelledTVarIO "chain-sync-status" (unSynced tip)
      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              chainContext
              localChainState
              syncedStatus
              getCurrentTip

      -- Simulate some chain following
      run $ forM_ blocks $ \(TestBlock header txs) -> onRollForward handler header txs
      -- Inject the rollback to somewhere between any of the previous state
      result <- run $ try @_ @SomeException $ onRollBackward handler rollbackPoint
      monitor . counterexample $ "try onRollBackward: " <> show result
      assert $ isRight result

      mRolledBackChainStateHistory <- run . atomically $ tryReadTMVar rolledBackTo
      let mRolledBackChainState = fmap currentState mRolledBackChainStateHistory
      monitor . counterexample $ "rolledBackTo: " <> show mRolledBackChainState
      pure $ (chainStateSlot <$> mRolledBackChainState) === Just (chainSlotFromPoint rollbackPoint)

  describe "LocalChainState" $ do
    prop "can resume from chain state" . monadicIO $ do
      (chainContext, chainStateAt, blocks) <- pickBlind genSequenceOfObservableBlocks
      timeHandle <- pickBlind arbitrary

      -- Use the handler to evolve the chain state to some new, latest version
      localChainState <- run $ newLocalChainState (initHistory chainStateAt)
      let getCurrentTip = pure ChainPointAtGenesis
      tip <- run getCurrentTip
      syncedStatus <- run $ newLabelledTVarIO "chain-sync-status" (unSynced tip)
      let handler =
            chainSyncHandler
              nullTracer
              (\_ -> pure ())
              (pure timeHandle)
              chainContext
              localChainState
              syncedStatus
              getCurrentTip
      run $ forM_ blocks $ \(TestBlock header txs) -> onRollForward handler header txs
      latestChainState <- run . atomically $ getLatest localChainState
      assert $ latestChainState /= chainStateAt

      -- Provided the latest chain state the LocalChainState must be able to
      -- rollback and forward
      prevAdvancedChainState <- run . atomically $ history localChainState
      resumedLocalChainState <- run $ newLocalChainState prevAdvancedChainState
      tip' <- run getCurrentTip
      syncedStatus' <- run $ newLabelledTVarIO "chain-sync-status" (unSynced tip')
      let resumedHandler =
            chainSyncHandler
              nullTracer
              (\_ -> pure ())
              (pure timeHandle)
              chainContext
              resumedLocalChainState
              syncedStatus'
              getCurrentTip
      (rollbackPoint, blocksAfter) <- pickBlind $ genRollbackBlocks blocks
      monitor $ label $ "Rollback " <> show (length blocksAfter) <> " blocks"

      run $ onRollBackward resumedHandler rollbackPoint
      -- NOTE: Sanity check that the rollback was affecting the local state
      rolledBackChainState <- run . atomically $ getLatest resumedLocalChainState
      assert $ null blocksAfter || rolledBackChainState /= latestChainState
      run $ forM_ blocksAfter $ \(TestBlock header txs) -> onRollForward resumedHandler header txs
      latestResumedChainState <- run . atomically $ getLatest resumedLocalChainState
      pure $ latestResumedChainState === latestChainState

-- | Create a chain sync handler which records events as they are called back.
recordEventsHandler :: ChainContext -> ChainStateAt -> GetTimeHandle IO -> IO (ChainSyncHandler IO, IO [ChainEvent Tx])
recordEventsHandler ctx cs getTimeHandle = do
  eventsVar <- newLabelledTVarIO "events-recorded" []
  localChainState <- newLocalChainState (initHistory cs)
  let getCurrentTip = pure ChainPointAtGenesis
  tip <- getCurrentTip
  syncedStatus <- newLabelledTVarIO "chain-sync-status" (unSynced tip)
  let handler =
        chainSyncHandler
          nullTracer
          (recordEvents eventsVar)
          getTimeHandle
          ctx
          localChainState
          syncedStatus
          getCurrentTip
  pure (handler, getEvents eventsVar)
 where
  getEvents :: TVar IO [ChainEvent Tx] -> IO [ChainEvent Tx]
  getEvents = readTVarIO

  recordEvents :: TVar IO [ChainEvent Tx] -> ChainEvent Tx -> IO ()
  recordEvents var event = do
    atomically $ modifyTVar var (event :)

-- | A block used for testing. This is a simpler version of the cardano-api
-- 'Block' and can be de-/constructed easily.
data TestBlock = TestBlock BlockHeader [Tx]

-- | Thin wrapper which generates a 'TestBlock' at some specific slot.
genBlockAt :: SlotNo -> [Tx] -> Gen TestBlock
genBlockAt sl txs = do
  header <- adjustSlot <$> hedgehog genBlockHeader
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
    initTx <- stepInit cctx (ctxParticipants ctx) (ctxHeadParameters ctx)
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
    [OnChainId] ->
    HeadParameters ->
    StateT [TestBlock] Gen Tx
  stepInit ctx participants params = do
    seedTxIn <- lift genTxIn
    let initTx = initialize ctx seedTxIn participants params
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
      stInitialized <- stepCommit ctx (ctxVerificationKeys hydraCtx) initTx
      (stInitialized :) <$> stepCommits hydraCtx initTx rest

  stepCommit ::
    ChainContext ->
    [VerificationKey PaymentKey] ->
    Tx ->
    StateT [TestBlock] Gen InitialState
  stepCommit ctx allVerificationKeys initTx = do
    let stInitial@InitialState{headId} = unsafeObserveInit ctx allVerificationKeys initTx
    utxo <- lift genCommit
    let commitTx = unsafeCommit ctx headId (getKnownUTxO stInitial) utxo
    putNextBlock commitTx
    pure $ snd $ fromJust $ observeCommit ctx stInitial commitTx
