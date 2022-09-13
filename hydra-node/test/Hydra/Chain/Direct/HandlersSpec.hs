{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.HandlersSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Ledger.Block as Ledger
import Cardano.Ledger.Era (toTxSeq)
import Control.Monad.Class.MonadSTM (MonadSTM (..), newTVarIO)
import Control.Tracer (nullTracer)
import Data.List ((\\))
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import Hydra.Cardano.Api (
  SlotNo (..),
  Tx,
  blockSlotNo,
  toLedgerTx,
 )
import Hydra.Chain (
  ChainEvent (..),
  HeadParameters,
 )
import Hydra.Chain.Direct.Context (
  ctxHeadParameters,
  deriveChainContexts,
  genCommit,
  genHydraContext,
  unsafeCommit,
 )
import Hydra.Chain.Direct.Handlers (
  ChainStateAt (..),
  ChainSyncHandler (..),
  RecordedAt (..),
  chainSyncHandler,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (Idle),
  IdleState (..),
  InitialState (..),
  initialize,
  observeCommit,
  observeInit,
  observeSomeTx,
 )
import Hydra.Chain.Direct.StateSpec (genChainState, genChainStateWithTx)
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Hydra.Chain.Direct.Util (Block)
import Hydra.Ledger.Cardano (genTxIn)
import Ouroboros.Consensus.Block (Point, blockPoint)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (
  choose,
  counterexample,
  elements,
  forAll,
  forAllBlind,
  forAllShow,
  label,
  (===),
 )
import Test.QuickCheck.Monadic (
  PropertyM (MkPropertyM),
  assert,
  monadicIO,
  monitor,
  pick,
  run,
  stop,
 )
import qualified Prelude

spec :: Spec
spec = do
  prop "roll forward results in Tick events" $
    monadicIO $ do
      chainState <- pickBlind genChainState
      timeHandle <- pickBlind arbitrary
      (handler, getEvents) <- run $ recordEventsHandler chainState timeHandle

      -- Pick a random slot and expect the 'Tick' event to correspond
      slot <- pick arbitrary
      expectedUTCTime <-
        run $
          either (failure . ("Time conversion failed: " <>) . toString) pure $
            slotToUTCTime timeHandle slot

      blk <- pickBlind $ genBlockAt slot []
      run $ onRollForward handler blk

      events <- run getEvents
      monitor $ counterexample ("events: " <> show events)
      void . stop $ events === [Tick expectedUTCTime]

  prop "yields observed transactions rolling forward" $ do
    forAll genChainStateWithTx $ \(st, tx, _) -> do
      let callback = \case
            Rollback{} ->
              fail "rolled back but expected roll forward."
            Observation onChainTx ->
              fst <$> observeSomeTx tx st `shouldBe` Just onChainTx
            Tick{} -> pure ()

      forAllBlind (genBlockAt 1 [tx]) $ \blk -> monadicIO $ do
        headState <- run $ newTVarIO $ stAtGenesis st
        timeHandle <- pickBlind arbitrary
        let handler = chainSyncHandler nullTracer callback headState timeHandle
        run $ onRollForward handler blk

  prop "can replay chain on (benign) rollback" $
    forAllBlind genSequenceOfObservableBlocks $ \(st, blks) ->
      forAllShow (genRollbackPoint blks) showRollbackInfo $ \(rollbackDepth, rollbackPoint) -> do
        let callback = \case
              Observation{} -> pure ()
              Tick{} -> pure ()
              Rollback n -> n `shouldBe` rollbackDepth

        monadicIO $ do
          monitor $ label ("Rollback depth: " <> show rollbackDepth)
          headState <- run $ newTVarIO st
          timeHandle <- pickBlind arbitrary
          let handler = chainSyncHandler nullTracer callback headState timeHandle

          -- 1/ Simulate some chain following
          st' <- run $ mapM_ (onRollForward handler) blks *> readTVarIO headState

          -- 2/ Inject a rollback to somewhere between any of the previous state
          result <- withCounterExample blks headState $ do
            try @_ @SomeException $ onRollBackward handler rollbackPoint
          assert (isRight result)

          -- 3/ Simulate chain-following replaying rolled back blocks, should re-apply
          let toReplay = blks \\ [blk | blk <- blks, blockPoint blk <= rollbackPoint]
          st'' <- run $ mapM_ (onRollForward handler) toReplay *> readTVarIO headState
          assert (st' == st'')

recordEventsHandler :: ChainState -> TimeHandle -> IO (ChainSyncHandler IO, IO [ChainEvent Tx])
recordEventsHandler st th = do
  headState <- newTVarIO $ stAtGenesis st
  eventsVar <- newTVarIO []
  let handler = chainSyncHandler nullTracer (recordEvents eventsVar) headState th
  pure (handler, getEvents eventsVar)
 where
  getEvents = atomically . readTVar

  recordEvents var e = atomically $ modifyTVar var (e :)

-- | Like 'pick' but using 'forAllBlind' under the hood.
pickBlind :: Monad m => Gen a -> PropertyM m a
pickBlind gen = MkPropertyM $ \k -> do
  a <- gen
  mp <- k a
  pure (forAllBlind (return a) . const <$> mp)

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
            [ "Head state at (before rollback): " <> showChainStateAt stBefore
            , "Head state at (after rollback):  " <> showChainStateAt stAfter
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

genRollbackPoint :: [Block] -> Gen (Word, Point Block)
genRollbackPoint blks = do
  let maxSlotNo = blockSlotNo (Prelude.last blks)
  ix <- SlotNo <$> choose (1, unSlotNo maxSlotNo)
  let rollbackDepth = length blks - length [blk | blk <- blks, blockSlotNo blk <= ix]
  rollbackPoint <- blockPoint <$> genBlockAt ix []
  pure (fromIntegral rollbackDepth, rollbackPoint)

-- | Generate a non-sparse sequence of blocks each containing an observable
-- transaction, starting from the returned on-chain head state.
--
-- Note that this does not generate the entire spectrum of observable
-- transactions in Hydra, but only init and commits, which is already sufficient
-- to observe at least one state transition and different levels of rollback.
genSequenceOfObservableBlocks :: Gen (ChainStateAt, [Block])
genSequenceOfObservableBlocks = do
  ctx <- genHydraContext 3
  -- NOTE: commits must be generated from each participant POV, and thus, we
  -- need all their respective ChainContext to move on.
  allContexts <- deriveChainContexts ctx
  -- Pick a peer context which will perform the init
  cctx <- elements allContexts
  blks <- flip execStateT [] $ do
    initTx <- stepInit cctx (ctxHeadParameters ctx)
    void $ stepCommits initTx (map IdleState allContexts)

  pure (stAtGenesis (Idle IdleState{ctx = cctx}), reverse blks)
 where
  nextSlot :: Monad m => StateT [Block] m SlotNo
  nextSlot = do
    get <&> \case
      [] -> 1
      x : _ -> SlotNo . succ . unSlotNo . blockSlotNo $ x

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
    [IdleState] ->
    StateT [Block] Gen [InitialState]
  stepCommits initTx = \case
    [] ->
      pure []
    stIdle : rest -> do
      stInitialized <- stepCommit initTx stIdle
      (stInitialized :) <$> stepCommits initTx rest

  stepCommit ::
    Tx ->
    IdleState ->
    StateT [Block] Gen InitialState
  stepCommit initTx IdleState{ctx} = do
    let (_, stInitial) = fromJust $ observeInit ctx initTx
    utxo <- lift genCommit
    let commitTx = unsafeCommit stInitial utxo
    putNextBlock commitTx
    pure $ snd $ fromJust $ observeCommit stInitial commitTx

stAtGenesis :: ChainState -> ChainStateAt
stAtGenesis currentChainState =
  ChainStateAt
    { currentChainState
    , recordedAt = AtStart
    }

showRollbackInfo :: (Word, Point Block) -> String
showRollbackInfo (rollbackDepth, rollbackPoint) =
  toString $
    unlines
      [ "Rollback depth: " <> show rollbackDepth
      , "Rollback point: " <> show rollbackPoint
      ]

showChainStateAt :: ChainStateAt -> Text
showChainStateAt ChainStateAt{recordedAt, currentChainState} =
  case recordedAt of
    AtStart -> "AtStart " <> show currentChainState
    AtPoint pt _ -> "AtPoint " <> show pt <> " " <> show currentChainState
