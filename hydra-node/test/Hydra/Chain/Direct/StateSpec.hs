{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intersect)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  Tx,
  UTxO,
  renderUTxO,
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
  PostTxError (..),
 )
import Hydra.Chain.Direct.Context (
  HydraContext (..),
  ctxHeadParameters,
  ctxParties,
  genCloseTx,
  genCommit,
  genCommits,
  genFanoutTx,
  genHydraContext,
  genHydraContextFor,
  genInitTx,
  genStInitial,
  genStOpen,
  getContestationDeadline,
  pickChainContext,
  unsafeCommit,
  unsafeObserveInitAndCommits,
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
  getKnownUTxO,
  initialize,
  observeAbort,
  observeClose,
  observeCommit,
  observeInit,
  observeSomeTx,
 )
import Hydra.Chain.Direct.TimeHandle (PointInTime)
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
import Test.Consensus.Cardano.Generators ()
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
  classify,
  conjoin,
  counterexample,
  discard,
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

genChainState :: Gen ChainState
genChainState =
  oneof
    [ Idle <$> arbitrary
    , Initial <$> genInitialState
    , Open <$> genOpenState
    , Closed <$> genClosedState
    ]
 where
  genInitialState = do
    ctx <- genHydraContext 3
    genStInitial ctx

  genOpenState = do
    ctx <- genHydraContext 3
    snd <$> genStOpen ctx

  genClosedState = do
    -- XXX: Untangle the whole generator mess here
    fst <$> genFanoutTx 3 70

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
