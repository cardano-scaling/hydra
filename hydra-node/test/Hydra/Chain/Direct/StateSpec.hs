{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
-- Fourmolu chokes on type-applications of promoted constructors (e.g.
-- @'StInitialized) and is unable to format properly after that. Hence this
-- option to allow using unticked constructor and save Fourmolu from dying.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import Cardano.Ledger.Era (toTxSeq)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadSTM (MonadSTM (..))
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import Data.List (elemIndex, intersect, (!!), (\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Type.Equality (testEquality, (:~:) (..))
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
import Hydra.Chain (ChainEvent (..), OnChainTx (OnCloseTx, remainingContestationPeriod), PostTxError (..), snapshotNumber)
import Hydra.Chain.Direct.Context (
  HydraContext (..),
  ctxHeadParameters,
  ctxParties,
  executeCommits,
  genCloseTx,
  genCommit,
  genCommits,
  genFanoutTx,
  genHydraContext,
  genHydraContextFor,
  genInitTx,
  genStIdle,
  genStInitialized,
  genStOpen,
  unsafeCommit,
  unsafeObserveTx,
 )
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  RecordedAt (..),
  SomeOnChainHeadStateAt (..),
  chainSyncHandler,
 )
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry)
import Hydra.Chain.Direct.State (
  HasTransition (..),
  HeadStateKind (..),
  HeadStateKindVal (..),
  ObserveTx (..),
  OnChainHeadState,
  SomeOnChainHeadState (..),
  TransitionFrom (..),
  abort,
  close,
  collect,
  commit,
  contest,
  getContestationDeadline,
  getKnownUTxO,
  idleOnChainHeadState,
  initialize,
  observeSomeTx,
 )
import Hydra.Chain.Direct.Util (Block)
import Hydra.ContestationPeriod (toNominalDiffTime)
import Hydra.Ledger.Cardano (
  genTxIn,
  genValue,
  renderTx,
  renderTxs,
 )
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx',
  genPointInTime,
  genPointInTimeBefore,
  maxTxExecutionUnits,
  maxTxSize,
  renderRedeemerReportFailures,
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
  Property,
  Testable (property),
  checkCoverage,
  choose,
  classify,
  conjoin,
  counterexample,
  elements,
  forAll,
  forAllBlind,
  forAllShow,
  label,
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
import Type.Reflection (typeOf)
import qualified Prelude

spec :: Spec
spec = parallel $ do
  describe "observeTx" $ do
    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAllSt $ \st tx ->
          isJust (observeSomeTx tx (SomeOnChainHeadState st))

  describe "init" $ do
    prop "is not observed if not invited" $
      forAll2 (genHydraContext 3) (genHydraContext 3) $ \(ctxA, ctxB) ->
        null (ctxParties ctxA `intersect` ctxParties ctxB)
          ==> forAll2 (genStIdle ctxA) (genStIdle ctxB)
          $ \(stIdleA, stIdleB) ->
            forAll genTxIn $ \seedInput ->
              let tx =
                    initialize
                      (ctxHeadParameters ctxA)
                      (ctxVerificationKeys ctxA)
                      seedInput
                      stIdleA
               in isNothing (observeTx @_ @StInitialized tx stIdleB)

  describe "commit" $ do
    propBelowSizeLimit maxTxSize forAllCommit

    prop "consumes all inputs that are committed" $
      forAllCommit $ \st tx ->
        case observeTx @_ @StInitialized tx st of
          Just (_, st') ->
            let knownInputs = UTxO.inputSet (getKnownUTxO st')
             in knownInputs `Set.disjoint` txInputSet tx
          Nothing ->
            False

    prop "can only be applied / observed once" $
      forAllCommit $ \st tx ->
        case observeTx @_ @StInitialized tx st of
          Just (_, st') ->
            case observeTx @_ @StInitialized tx st' of
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
            ctx <- genHydraContext 1
            st1 <- genStInitialized ctx
            st2 <- genStInitialized ctx -- TODO: ensure they are distinct
            pure (st1, st2)
      forAll twoDistinctHeads $ \(stHead1, stHead2) ->
        let observedIn1 = observeTx @StInitialized @StIdle (abort stHead1) stHead1
            observedIn2 = observeTx @StInitialized @StIdle (abort stHead1) stHead2
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
      forAllSt $ \(SomeOnChainHeadState -> st) tx -> do
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

withCounterExample :: [Block] -> TVar IO SomeOnChainHeadStateAt -> IO a -> PropertyM IO a
withCounterExample blks headState step = do
  stBefore <- run $ readTVarIO headState
  a <- run step
  stAfter <- run $ readTVarIO headState
  a <$ do
    monitor $
      counterexample $
        toString $
          unlines
            [ "Head state at (before rollback): " <> showStateRecordedAt stBefore
            , "Head state at (after rollback):  " <> showStateRecordedAt stAfter
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
genSequenceOfObservableBlocks :: Gen (SomeOnChainHeadStateAt, [Block])
genSequenceOfObservableBlocks = do
  scriptRegistry <- genScriptRegistry
  ctx <- genHydraContext 3

  -- NOTE: commits must be generated from each participant POV, and thus, we
  -- need all their respective StIdle to move on.
  let stIdles = flip map (zip (ctxVerificationKeys ctx) (ctxParties ctx)) $ \(vk, p) ->
        let peerVerificationKeys = ctxVerificationKeys ctx \\ [vk]
         in idleOnChainHeadState (ctxNetworkId ctx) peerVerificationKeys vk p scriptRegistry

  stIdle <- elements stIdles
  blks <- flip execStateT [] $ do
    initTx <- stepInit ctx stIdle
    void $ stepCommits ctx initTx stIdles

  pure (stAtGenesis (SomeOnChainHeadState stIdle), reverse blks)
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
    HydraContext ->
    OnChainHeadState StIdle ->
    StateT [Block] Gen Tx
  stepInit ctx stIdle = do
    txIn <- lift genTxIn
    let initTx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) txIn stIdle
    initTx <$ putNextBlock initTx

  stepCommits ::
    HydraContext ->
    Tx ->
    [OnChainHeadState 'StIdle] ->
    StateT [Block] Gen [OnChainHeadState 'StInitialized]
  stepCommits ctx initTx = \case
    [] ->
      pure []
    stIdle : rest -> do
      stInitialized <- stepCommit initTx stIdle
      (stInitialized :) <$> stepCommits ctx initTx rest

  stepCommit ::
    Tx ->
    OnChainHeadState 'StIdle ->
    StateT [Block] Gen (OnChainHeadState 'StInitialized)
  stepCommit initTx stIdle = do
    let (_, stInitialized) = unsafeObserveTx @_ @StInitialized initTx stIdle
    utxo <- lift genCommit
    let commitTx = unsafeCommit utxo stInitialized
    putNextBlock commitTx
    pure $ snd $ unsafeObserveTx @_ @StInitialized commitTx stInitialized

stAtGenesis :: SomeOnChainHeadState -> SomeOnChainHeadStateAt
stAtGenesis currentOnChainHeadState =
  SomeOnChainHeadStateAt
    { currentOnChainHeadState
    , recordedAt = AtStart
    }

--
-- Generic Properties
--

propBelowSizeLimit ::
  forall st.
  Natural ->
  ((OnChainHeadState st -> Tx -> Property) -> Property) ->
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
  forall st.
  ((OnChainHeadState st -> Tx -> Property) -> Property) ->
  SpecWith ()
propIsValid forAllTx =
  prop "validates within maxTxExecutionUnits" $
    forAllTx $ \st tx -> do
      let lookupUTxO = getKnownUTxO st
      case evaluateTx' maxTxExecutionUnits tx lookupUTxO of
        Left basicFailure ->
          property False
            & counterexample ("Tx: " <> renderTx tx)
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
            & counterexample ("Phase-1 validation failed: " <> show basicFailure)
        Right redeemerReport ->
          all isRight (Map.elems redeemerReport)
            & counterexample ("Tx: " <> renderTx tx)
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
            & counterexample (toString $ "Redeemer report: " <> renderRedeemerReportFailures redeemerReport)
            & counterexample "Phase-2 validation failed"

--
-- QuickCheck Extras
--

-- XXX: This is very fancy, but does not prevent us of not aligning forAll
-- generators with Transition labels. Ideally we would would use the actual
-- states/transactions or observed states/transactions for labeling.
forAllSt ::
  (Testable property) =>
  (forall st. (HasTransition st) => OnChainHeadState st -> Tx -> property) ->
  Property
forAllSt action =
  forAllBlind
    ( elements
        [
          ( forAllInit action
          , Transition @ 'StIdle (TransitionTo (Proxy @ 'StInitialized))
          )
        ,
          ( forAllCommit action
          , Transition @ 'StInitialized (TransitionTo (Proxy @ 'StInitialized))
          )
        ,
          ( forAllAbort action
          , Transition @ 'StInitialized (TransitionTo (Proxy @ 'StIdle))
          )
        ,
          ( forAllCollectCom action
          , Transition @ 'StInitialized (TransitionTo (Proxy @ 'StOpen))
          )
        ,
          ( forAllClose action
          , Transition @ 'StOpen (TransitionTo (Proxy @ 'StClosed))
          )
        ,
          ( forAllContest action
          , Transition @ 'StClosed (TransitionTo (Proxy @ 'StClosed))
          )
        ,
          ( forAllFanout action
          , Transition @ 'StClosed (TransitionTo (Proxy @ 'StIdle))
          )
        ]
    )
    (\(p, lbl) -> genericCoverTable [lbl] p)

forAllInit ::
  (Testable property) =>
  (OnChainHeadState 'StIdle -> Tx -> property) ->
  Property
forAllInit action =
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStIdle ctx) $ \stIdle ->
      forAll genTxIn $ \seedInput -> do
        let tx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
         in action stIdle tx
              & classify
                (length (ctxParties ctx) == 1)
                "1 party"
              & classify
                (length (ctxParties ctx) > 1)
                "2+ parties"

forAllCommit ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllCommit action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStInitialized ctx) $ \stInitialized ->
      forAllShow genCommit renderUTxO $ \utxo ->
        let tx = unsafeCommit utxo stInitialized
         in action stInitialized tx
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
    forAll (genStInitialized ctx) $ \stInitialized ->
      forAllShow genByronCommit renderUTxO $ \utxo ->
        case commit utxo stInitialized of
          Right{} -> property False
          Left e -> action e

forAllAbort ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllAbort action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStIdle ctx) $ \stIdle ->
      forAllShow (genInitTx ctx) renderTx $ \initTx -> do
        forAllShow (sublistOf =<< genCommits ctx initTx) renderTxs $ \commits ->
          let (_, stInitialized) = executeCommits initTx commits stIdle
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
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllCollectCom action =
  forAllBlind genCollectComTx $ \(committedUTxO, stInitialized, tx) ->
    action stInitialized tx
      & counterexample ("Committed UTxO: " <> show committedUTxO)
 where
  genCollectComTx = do
    ctx <- genHydraContextFor 3
    initTx <- genInitTx ctx
    commits <- genCommits ctx initTx
    stIdle <- genStIdle ctx
    let (committedUTxO, stInitialized) = executeCommits initTx commits stIdle
    pure (committedUTxO, stInitialized, collect stInitialized)

forAllClose ::
  (Testable property) =>
  (OnChainHeadState 'StOpen -> Tx -> property) ->
  Property
forAllClose action = do
  -- FIXME: we should not hardcode number of parties but generate it within bounds
  forAll (genCloseTx 3) $ \(st, tx, sn) ->
    action st tx
      & label (Prelude.head . Prelude.words . show $ sn)

forAllContest ::
  (Testable property) =>
  (OnChainHeadState 'StClosed -> Tx -> property) ->
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

  genContestTx = do
    ctx <- genHydraContextFor 3
    (u0, stOpen) <- genStOpen ctx
    confirmed <- genConfirmedSnapshot 0 u0 []
    closePointInTime <- genPointInTime
    let closeTx = close confirmed closePointInTime stOpen
    let stClosed = snd $ unsafeObserveTx @_ @ 'StClosed closeTx stOpen
    utxo <- arbitrary
    contestSnapshot <- genConfirmedSnapshot (succ $ number $ getSnapshot confirmed) utxo (ctxHydraSigningKeys ctx)
    contestPointInTime <- genPointInTimeBefore (getContestationDeadline stClosed)
    pure (ctx, closePointInTime, stClosed, contest contestSnapshot contestPointInTime stClosed)

forAllFanout ::
  (Testable property) =>
  (OnChainHeadState 'StClosed -> Tx -> property) ->
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
-- Wrapping Transition for easy labelling
--

allTransitions :: [Transition]
allTransitions =
  mconcat
    [ Transition <$> transitions (Proxy @ 'StIdle)
    , Transition <$> transitions (Proxy @ 'StInitialized)
    , Transition <$> transitions (Proxy @ 'StOpen)
    , Transition <$> transitions (Proxy @ 'StClosed)
    ]

data Transition where
  Transition ::
    forall (st :: HeadStateKind).
    (HeadStateKindVal st, Typeable st) =>
    TransitionFrom st ->
    Transition
deriving instance Typeable Transition

instance Show Transition where
  show (Transition t) = show t

instance Eq Transition where
  (Transition from) == (Transition from') =
    case testEquality (typeOf from) (typeOf from') of
      Just Refl -> from == from'
      Nothing -> False

instance Enum Transition where
  toEnum = (!!) allTransitions
  fromEnum = fromJust . (`elemIndex` allTransitions)

instance Bounded Transition where
  minBound = Prelude.head allTransitions
  maxBound = Prelude.last allTransitions

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

showStateRecordedAt :: SomeOnChainHeadStateAt -> Text
showStateRecordedAt SomeOnChainHeadStateAt{recordedAt} =
  case recordedAt of
    AtStart -> "Start"
    AtPoint pt _ -> show pt
