{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import Cardano.Ledger.Era (toTxSeq)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadSTM (MonadSTM (..))
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intersect, (\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  SlotNo (..),
  Tx,
  UTxO,
  blockSlotNo,
  renderUTxO,
  toLedgerTx,
  txInputSet,
  txOutValue,
  txOuts',
  valueSize,
  pattern ByronAddressInEra,
  pattern ReferenceScriptNone,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import Hydra.Chain (
  ChainEvent (..),
  HeadParameters,
  OnChainTx (OnCloseTx, remainingContestationPeriod),
  PostTxError (..),
  snapshotNumber,
 )
import Hydra.Chain.Direct.Context (
  HydraContext (..),
  ctxHeadParameters,
  ctxParties,
  deriveChainContexts,
  genCloseTx,
  genCommit,
  genCommits,
  genFanoutTx,
  genHydraContext,
  genHydraContextFor,
  genInitTx,
  genStInitial,
  genStOpen,
  pickChainContext,
  unsafeCommit,
  unsafeObserveInitAndCommits,
 )
import Hydra.Chain.Direct.Handlers (
  ChainStateAt (..),
  ChainSyncHandler (..),
  RecordedAt (..),
  chainSyncHandler,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (Closed, Idle, Initial, Open),
  ChainTransition (Close, Collect, Commit, Contest, Fanout, Init),
  ClosedState,
  HasKnownUTxO (getKnownUTxO),
  IdleState (..),
  InitialState (..),
  OpenState,
  abort,
  close,
  collect,
  commit,
  contest,
  getContestationDeadline,
  getKnownUTxO,
  initialize,
  observeAbort,
  observeClose,
  observeCommit,
  observeInit,
  observeSomeTx,
 )
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Util (Block)
import Hydra.ContestationPeriod (toNominalDiffTime)
import Hydra.Ledger.Cardano (
  genTxIn,
  genValue,
  renderTx,
  renderTxWithUTxO,
 )
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx',
  genPointInTime,
  genPointInTimeBefore,
  maxTxExecutionUnits,
  maxTxSize,
  renderEvaluationReportFailures,
 )
import Hydra.Snapshot (genConfirmedSnapshot, getSnapshot, number)
import Ouroboros.Consensus.Block (Point, blockPoint)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Consensus.Cardano.Generators ()
import Test.Hspec (shouldBe)
import Test.Hydra.Prelude (
  Spec,
  SpecWith,
  describe,
  forAll2,
  genericCoverTable,
  parallel,
  prop,
 )
import Test.QuickCheck (
  Positive (Positive),
  Property,
  Testable (property),
  checkCoverage,
  choose,
  classify,
  conjoin,
  counterexample,
  discard,
  elements,
  forAll,
  forAllBlind,
  forAllShow,
  label,
  oneof,
  sized,
  sublistOf,
  tabulate,
  (=/=),
  (===),
  (==>),
 )
import Test.QuickCheck.Monadic (
  PropertyM,
  assert,
  monadicIO,
  monitor,
  run,
 )
import qualified Prelude

spec :: Spec
spec = parallel $ do
  describe "observeTx" $ do
    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAll genChainStateWithTx $ \(st, tx, transition) ->
          genericCoverTable [transition] $
            isJust (observeSomeTx tx st)
              & counterexample "observeSomeTx returned Nothing"

  describe "init" $ do
    propBelowSizeLimit maxTxSize forAllInit
    -- propIsValid forAllInit XXX: not possible because it spends an "outside" UTxO

    prop "is not observed if not invited" $
      forAll2 (genHydraContext 3) (genHydraContext 3) $ \(ctxA, ctxB) ->
        null (ctxParties ctxA `intersect` ctxParties ctxB)
          ==> forAll2 (pickChainContext ctxA) (pickChainContext ctxB)
          $ \(cctxA, cctxB) ->
            forAll genTxIn $ \seedInput ->
              let tx = initialize cctxA (ctxHeadParameters ctxA) seedInput
               in isNothing (observeInit cctxB tx)

  describe "commit" $ do
    propBelowSizeLimit maxTxSize forAllCommit
    -- propIsValid forAllCommit XXX: not possible because it spends an "outside" UTxO

    prop "consumes all inputs that are committed" $
      forAllCommit $ \st tx ->
        case observeCommit st tx of
          Just (_, st') ->
            let knownInputs = UTxO.inputSet (getKnownUTxO st')
             in knownInputs `Set.disjoint` txInputSet tx
          Nothing ->
            False

    prop "can only be applied / observed once" $
      forAllCommit $ \st tx ->
        case observeCommit st tx of
          Just (_, st') ->
            case observeCommit st' tx of
              Just{} -> False
              Nothing -> True
          Nothing ->
            False

    prop "reject Commits of Byron outputs" $
      forAllNonEmptyByronCommit $ \case
        UnsupportedLegacyOutput{} -> property True
        _ -> property False

  describe "abort" $ do
    propBelowSizeLimit maxTxSize forAllAbort
    propIsValid forAllAbort

    prop "ignore aborts of other heads" $ do
      let twoDistinctHeads = do
            ctx <- genHydraContext 3
            st1@InitialState{initialHeadId = h1} <- genStInitial ctx
            st2@InitialState{initialHeadId = h2} <- genStInitial ctx
            when (h1 == h2) discard
            pure (st1, st2)
      forAll twoDistinctHeads $ \(stHead1, stHead2) ->
        let observedIn1 = observeAbort stHead1 (abort stHead1)
            observedIn2 = observeAbort stHead2 (abort stHead1)
         in conjoin
              [ observedIn1 =/= Nothing
              , observedIn2 === Nothing
              ]

  describe "collectCom" $ do
    propBelowSizeLimit maxTxSize forAllCollectCom
    propIsValid forAllCollectCom

  describe "close" $ do
    propBelowSizeLimit maxTxSize forAllClose
    propIsValid forAllClose

  describe "contest" $ do
    propBelowSizeLimit maxTxSize forAllContest
    propIsValid forAllContest

  describe "fanout" $ do
    propBelowSizeLimit maxTxSize forAllFanout
    propIsValid forAllFanout

  describe "ChainSyncHandler" $ do
    prop "yields observed transactions rolling forward" $ do
      forAll genChainStateWithTx $ \(st, tx, _) -> do
        let callback = \case
              Rollback{} ->
                fail "rolled back but expected roll forward."
              Observation OnCloseTx{snapshotNumber} ->
                -- FIXME: Special case for `OnCloseTx` because we don't directly observe the remaining contestation period,
                -- it's the result of a computation that involves current time
                fst <$> observeSomeTx tx st `shouldBe` Just OnCloseTx{snapshotNumber, remainingContestationPeriod = 0}
              Observation onChainTx ->
                fst <$> observeSomeTx tx st `shouldBe` Just onChainTx
        forAllBlind (genBlockAt 1 [tx]) $ \blk -> monadicIO $ do
          headState <- run $ newTVarIO $ stAtGenesis st
          let handler = chainSyncHandler nullTracer callback headState
          run $ onRollForward handler blk

    prop "can replay chain on (benign) rollback" $
      forAllBlind genSequenceOfObservableBlocks $ \(st, blks) ->
        forAllShow (genRollbackPoint blks) showRollbackInfo $ \(rollbackDepth, rollbackPoint) -> do
          let callback = \case
                Observation{} -> do
                  pure ()
                Rollback n -> n `shouldBe` rollbackDepth

          monadicIO $ do
            monitor $ label ("Rollback depth: " <> show rollbackDepth)
            headState <- run $ newTVarIO st
            let handler = chainSyncHandler nullTracer callback headState

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

--
-- Generic Properties
--

propBelowSizeLimit ::
  Natural ->
  ((a -> Tx -> Property) -> Property) ->
  SpecWith ()
propBelowSizeLimit txSizeLimit forAllTx =
  prop ("transaction size is below " <> showKB txSizeLimit) $
    forAllTx $ \_ tx ->
      let cbor = serialize tx
          len = LBS.length cbor
       in len < fromIntegral txSizeLimit
            & label (showKB len)
            & counterexample (renderTx tx)
            & counterexample ("Actual size: " <> show len)
 where
  showKB nb = show (nb `div` 1024) <> "kB"

-- TODO: DRY with Hydra.Chain.Direct.Contract.Mutation.propTransactionValidates?
propIsValid ::
  HasKnownUTxO a =>
  ((a -> Tx -> Property) -> Property) ->
  SpecWith ()
propIsValid forAllTx =
  prop "validates within maxTxExecutionUnits" $
    forAllTx $ \st tx -> do
      let lookupUTxO = getKnownUTxO st
      case evaluateTx' maxTxExecutionUnits tx lookupUTxO of
        Left validityError ->
          property False
            & counterexample ("Tx: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample ("Evaluation failed: " <> show validityError)
        Right evaluationReport ->
          all isRight (Map.elems evaluationReport)
            & counterexample ("Tx: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample (toString $ "Failures: " <> renderEvaluationReportFailures evaluationReport)
            & counterexample "Phase-2 validation failed"

--
-- QuickCheck Extras
--

genChainStateWithTx :: Gen (ChainState, Tx, ChainTransition)
genChainStateWithTx =
  oneof
    [ genInitWithState
    , genCommitWithState
    , genCollectWithState
    , genCloseWithState
    , genContestWithState
    , genFanoutWithState
    ]
 where
  genInitWithState :: Gen (ChainState, Tx, ChainTransition)
  genInitWithState = do
    ctx <- genHydraContext 3
    cctx <- pickChainContext ctx
    seedInput <- genTxIn
    let tx = initialize cctx (ctxHeadParameters ctx) seedInput
    pure (Idle $ IdleState cctx, tx, Init)

  genCommitWithState :: Gen (ChainState, Tx, ChainTransition)
  genCommitWithState = do
    ctx <- genHydraContext 3
    stInitial <- genStInitial ctx
    utxo <- genCommit
    let tx = unsafeCommit stInitial utxo
    pure (Initial stInitial, tx, Commit)

  genCollectWithState :: Gen (ChainState, Tx, ChainTransition)
  genCollectWithState = do
    (_, st, tx) <- genCollectComTx
    pure (Initial st, tx, Collect)

  genCloseWithState :: Gen (ChainState, Tx, ChainTransition)
  genCloseWithState = do
    (st, tx, _) <- genCloseTx 3
    pure (Open st, tx, Close)

  genContestWithState :: Gen (ChainState, Tx, ChainTransition)
  genContestWithState = do
    (_, _, st, tx) <- genContestTx
    pure (Closed st, tx, Contest)

  genFanoutWithState :: Gen (ChainState, Tx, ChainTransition)
  genFanoutWithState = do
    Positive numParties <- arbitrary
    Positive numOutputs <- arbitrary
    (st, tx) <- genFanoutTx numParties numOutputs
    pure (Closed st, tx, Fanout)

-- TODO: These forAllXX functions are hard to use and understand. Maybe simple
-- 'Gen' or functions in 'PropertyM' are better combinable?

forAllInit ::
  (Testable property) =>
  (IdleState -> Tx -> property) ->
  Property
forAllInit action =
  forAllBlind (genHydraContext 3) $ \ctx ->
    forAll (pickChainContext ctx) $ \cctx -> do
      forAll genTxIn $ \seedInput -> do
        let tx = initialize cctx (ctxHeadParameters ctx) seedInput
         in action (IdleState cctx) tx
              & classify
                (length (peerVerificationKeys cctx) == 0)
                "1 party"
              & classify
                (length (peerVerificationKeys cctx) > 0)
                "2+ parties"

forAllCommit ::
  (Testable property) =>
  (InitialState -> Tx -> property) ->
  Property
forAllCommit action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStInitial ctx) $ \stInitial ->
      forAllShow genCommit renderUTxO $ \utxo ->
        let tx = unsafeCommit stInitial utxo
         in action stInitial tx
              & classify
                (null utxo)
                "Empty commit"
              & classify
                (not (null utxo))
                "Non-empty commit"
              & counterexample ("tx: " <> renderTx tx)

forAllNonEmptyByronCommit ::
  (PostTxError Tx -> Property) ->
  Property
forAllNonEmptyByronCommit action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStInitial ctx) $ \stInitial ->
      forAllShow genByronCommit renderUTxO $ \utxo ->
        case commit stInitial utxo of
          Right{} -> property False
          Left e -> action e

forAllAbort ::
  (Testable property) =>
  (InitialState -> Tx -> property) ->
  Property
forAllAbort action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (pickChainContext ctx) $ \cctx ->
      forAllBlind (genInitTx ctx) $ \initTx -> do
        forAllBlind (sublistOf =<< genCommits ctx initTx) $ \commits ->
          let (_, stInitialized) = unsafeObserveInitAndCommits cctx initTx commits
           in action stInitialized (abort stInitialized)
                & classify
                  (null commits)
                  "Abort immediately, after 0 commits"
                & classify
                  (not (null commits) && length commits < length (ctxParties ctx))
                  "Abort after some (but not all) commits"
                & classify
                  (length commits == length (ctxParties ctx))
                  "Abort after all commits"

forAllCollectCom ::
  (Testable property) =>
  (InitialState -> Tx -> property) ->
  Property
forAllCollectCom action =
  forAllBlind genCollectComTx $ \(committedUTxO, stInitialized, tx) ->
    action stInitialized tx
      & counterexample ("Committed UTxO: " <> show committedUTxO)

genCollectComTx :: Gen ([UTxO], InitialState, Tx)
genCollectComTx = do
  ctx <- genHydraContextFor 3
  initTx <- genInitTx ctx
  commits <- genCommits ctx initTx
  cctx <- pickChainContext ctx
  let (committedUTxO, stInitialized) = unsafeObserveInitAndCommits cctx initTx commits
  pure (committedUTxO, stInitialized, collect stInitialized)

forAllClose ::
  (Testable property) =>
  (OpenState -> Tx -> property) ->
  Property
forAllClose action = do
  -- FIXME: we should not hardcode number of parties but generate it within bounds
  forAll (genCloseTx 3) $ \(st, tx, sn) ->
    action st tx
      & label (Prelude.head . Prelude.words . show $ sn)

forAllContest ::
  (Testable property) =>
  (ClosedState -> Tx -> property) ->
  Property
forAllContest action =
  forAllBlind genContestTx $ \(HydraContext{ctxContestationPeriod}, closePointInTime, stClosed, tx) ->
    action stClosed tx
      & counterexample ("Contestation deadline: " <> show (getContestationDeadline stClosed))
      & counterexample ("Contestation period: " <> show ctxContestationPeriod)
      & counterexample ("Close point: " <> show closePointInTime)
      & tabulate "Contestation deadline" (tabulateNum $ getContestationDeadline stClosed)
      & tabulate "Contestation period" (tabulateContestationPeriod ctxContestationPeriod)
      & tabulate "Close point (slot)" (tabulateNum $ fst closePointInTime)
 where
  tabulateNum x
    | x > 0 = ["> 0"]
    | x < 0 = ["< 0"]
    | otherwise = ["== 0"]

  tabulateContestationPeriod (toNominalDiffTime -> cp)
    | cp == confirmedHorizon = ["k blocks on mainnet"]
    | cp == oneDay = ["one day"]
    | cp == oneWeek = ["one week"]
    | cp == oneMonth = ["one month"]
    | cp == oneYear = ["one year"]
    | cp < confirmedHorizon = ["< k blocks"]
    | otherwise = ["> k blocks"]

  confirmedHorizon = 2160 * 20 -- k blocks on mainnet
  oneDay = 3600 * 24
  oneWeek = oneDay * 7
  oneMonth = oneDay * 30
  oneYear = oneDay * 365

genContestTx ::
  Gen
    ( HydraContext
    , PointInTime
    , ClosedState
    , Tx
    )
genContestTx = do
  ctx <- genHydraContextFor 3
  (u0, stOpen) <- genStOpen ctx
  confirmed <- genConfirmedSnapshot 0 u0 []
  closePointInTime <- genPointInTime
  let closeTx = close stOpen confirmed closePointInTime
  let stClosed = snd $ fromJust $ observeClose stOpen closeTx
  utxo <- arbitrary
  contestSnapshot <- genConfirmedSnapshot (succ $ number $ getSnapshot confirmed) utxo (ctxHydraSigningKeys ctx)
  contestPointInTime <- genPointInTimeBefore (getContestationDeadline stClosed)
  pure (ctx, closePointInTime, stClosed, contest stClosed contestSnapshot contestPointInTime)

forAllFanout ::
  (Testable property) =>
  (ClosedState -> Tx -> property) ->
  Property
forAllFanout action =
  -- TODO: The utxo to fanout should be more arbitrary to have better test coverage
  forAll (sized $ \n -> genFanoutTx 3 (n `min` maxSupported)) $ \(stClosed, tx) ->
    action stClosed tx
      & label ("Fanout size: " <> prettyLength (countAssets $ txOuts' tx))
 where
  maxSupported = 70

  countAssets = getSum . foldMap (Sum . valueSize . txOutValue)

  prettyLength len
    | len > maxSupported = "> " <> show maxSupported <> " ???"
    | len >= 50 = "50-" <> show maxSupported
    | len >= 10 = "10-49"
    | otherwise = "00-10"

--
-- Generators
--

genByronCommit :: Gen UTxO
genByronCommit = do
  input <- arbitrary
  addr <- ByronAddressInEra <$> arbitrary
  value <- genValue
  pure $ UTxO.singleton (input, TxOut addr value TxOutDatumNone ReferenceScriptNone)

genBlockAt :: SlotNo -> [Tx] -> Gen Block
genBlockAt sl txs = do
  header <- adjustSlot <$> arbitrary
  let body = toTxSeq $ StrictSeq.fromList (toLedgerTx <$> txs)
  pure $ BlockBabbage $ mkShelleyBlock $ Ledger.Block header body
 where
  adjustSlot (Praos.Header body sig) =
    let body' = body{Praos.hbSlotNo = sl}
     in Praos.Header body' sig

--
-- Prettifier
--

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
