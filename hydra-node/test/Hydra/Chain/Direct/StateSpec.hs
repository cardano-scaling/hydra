{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intersect)
import qualified Data.List as List
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  Tx,
  UTxO,
  hashScript,
  renderUTxO,
  scriptPolicyId,
  toPlutusCurrencySymbol,
  txInputSet,
  txOutValue,
  txOuts',
  valueSize,
  pattern ByronAddressInEra,
  pattern PlutusScript,
  pattern PlutusScriptSerialised,
  pattern ReferenceScriptNone,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain (
  OnChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (ChangeMintingPolicy, ChangeOutput, Changes),
  applyMutation,
  changeHeadOutputDatum,
  propTransactionEvaluates,
  propTransactionFailsEvaluation,
  replaceHeadId,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState,
  HasKnownUTxO (getKnownUTxO),
  HydraContext (..),
  InitialState (..),
  abort,
  close,
  closedThreadOutput,
  collect,
  commit,
  ctxHeadParameters,
  ctxParties,
  fanout,
  genChainStateWithTx,
  genCloseTx,
  genCollectComTx,
  genCommit,
  genCommits,
  genCommits',
  genContestTx,
  genFanoutTx,
  genHydraContext,
  genInitTx,
  genStInitial,
  getContestationDeadline,
  getKnownUTxO,
  initialize,
  observeAbort,
  observeClose,
  observeCollect,
  observeCommit,
  observeInit,
  observeSomeTx,
  pickChainContext,
  unsafeCommit,
  unsafeObserveInitAndCommits,
 )
import Hydra.Chain.Direct.Tx (ClosedThreadOutput (closedContesters), NotAnInitReason (..))
import Hydra.ContestationPeriod (toNominalDiffTime)
import Hydra.Ledger.Cardano (
  genOutput,
  genTxIn,
  genTxOutAdaOnly,
  genUTxOAdaOnlyOfSize,
  genValue,
 )
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx,
  genValidityBoundsFromContestationPeriod,
  maxTxSize,
 )
import qualified Hydra.Ledger.Cardano.Evaluate as Fixture
import Hydra.Options (maximumNumberOfParties)
import Hydra.Snapshot (ConfirmedSnapshot (InitialSnapshot, initialUTxO))
import qualified Plutus.V1.Ledger.Examples as Plutus
import qualified Plutus.V2.Ledger.Api as Plutus
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (
  Property,
  Testable (property),
  checkCoverage,
  classify,
  conjoin,
  counterexample,
  cover,
  discard,
  forAll,
  forAllBlind,
  forAllShow,
  label,
  sized,
  sublistOf,
  tabulate,
  (.||.),
  (=/=),
  (===),
  (==>),
 )
import Test.QuickCheck.Monadic (monadicIO, monadicST, pick)
import qualified Prelude

spec :: Spec
spec = parallel $ do
  describe "ChainState" $
    roundtripAndGoldenSpecs (Proxy @ChainState)

  describe "Plutus.PubKeyHash" $
    roundtripAndGoldenSpecs (Proxy @Plutus.PubKeyHash)

  describe "observeTx" $ do
    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAll genChainStateWithTx $ \(ctx, st, tx, transition) ->
          genericCoverTable [transition] $
            isJust (observeSomeTx ctx st tx)
              & counterexample "observeSomeTx returned Nothing"

  describe "init" $ do
    propBelowSizeLimit maxTxSize forAllInit
    propIsValid forAllInit

    it "only proper head is observed" $
      monadicIO $ do
        ctx <- pickBlind (genHydraContext maximumNumberOfParties)
        cctx <- pickBlind $ pickChainContext ctx
        seedInput <- pickBlind arbitrary
        seedTxOut <- pickBlind genTxOutAdaOnly

        let tx = initialize cctx (ctxHeadParameters ctx) seedInput
            originalIsObserved = property $ isRight (observeInit cctx tx)
        -- We do replace the minting policy and datum of a head output to
        -- simulate a faked init transaction.
        let alwaysSucceedsV2 = PlutusScriptSerialised $ Plutus.alwaysSucceedingNAryFunction 2
        let fakeHeadId = toPlutusCurrencySymbol . scriptPolicyId $ PlutusScript alwaysSucceedsV2
        let headTxOut = List.head (txOuts' tx)
        let mutation =
              Changes
                [ ChangeMintingPolicy alwaysSucceedsV2
                , ChangeOutput 0 $ changeHeadOutputDatum (replaceHeadId fakeHeadId) headTxOut
                ]
        let utxo = UTxO.singleton (seedInput, seedTxOut)
        let (tx', utxo') = applyMutation mutation (tx, utxo)
            -- We expected mutated transaction to still be valid, but not observed.
            mutatedIsValid = property $
              case evaluateTx tx' utxo' of
                Left _ -> False
                Right ok
                  | all isRight ok -> True
                  | otherwise -> False
            mutatedIsNotObserved =
              observeInit cctx tx' === Left NotAHeadPolicy

        pure $
          conjoin
            [ originalIsObserved
                & counterexample (renderTx tx)
                & counterexample "Original transaction is not observed."
            , mutatedIsValid
                & counterexample (renderTx tx')
                & counterexample "Mutated transaction is not valid."
            , mutatedIsNotObserved
                & counterexample (renderTx tx')
                & counterexample "Should not observe mutated transaction"
            ]
            & counterexample ("new minting policy: " <> show (hashScript $ PlutusScript alwaysSucceedsV2))

    prop "is not observed if not invited" $
      forAll2 (genHydraContext maximumNumberOfParties) (genHydraContext maximumNumberOfParties) $ \(ctxA, ctxB) ->
        null (ctxParties ctxA `intersect` ctxParties ctxB)
          ==> forAll2 (pickChainContext ctxA) (pickChainContext ctxB)
          $ \(cctxA, cctxB) ->
            forAll genTxIn $ \seedInput ->
              let tx = initialize cctxA (ctxHeadParameters ctxA) seedInput
               in isLeft (observeInit cctxB tx)

  describe "commit" $ do
    propBelowSizeLimit maxTxSize forAllCommit
    propIsValid forAllCommit

    prop "consumes all inputs that are committed" $
      forAllCommit' $ \ctx st _ tx ->
        case observeCommit ctx st tx of
          Just (_, st') ->
            let knownInputs = UTxO.inputSet (getKnownUTxO st')
             in knownInputs `Set.disjoint` txInputSet tx
          Nothing ->
            False

    prop "can only be applied / observed once" $
      forAllCommit' $ \ctx st _ tx ->
        case observeCommit ctx st tx of
          Just (_, st') ->
            case observeCommit ctx st' tx of
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
            ctx <- genHydraContext maximumNumberOfParties
            (ctx1, st1@InitialState{headId = h1}) <- genStInitial ctx
            (ctx2, st2@InitialState{headId = h2}) <- genStInitial ctx
            when (h1 == h2) discard
            pure ((ctx1, st1), (ctx2, st2))
      forAll twoDistinctHeads $ \((ctx1, stHead1), (ctx2, stHead2)) ->
        let observedIn1 = observeAbort stHead1 (abort ctx1 stHead1 mempty)
            observedIn2 = observeAbort stHead2 (abort ctx2 stHead1 mempty)
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

  describe "acceptance" $ do
    it "can close & fanout every collected head" $ do
      prop_canCloseFanoutEveryCollect

-- * Properties

prop_canCloseFanoutEveryCollect :: Property
prop_canCloseFanoutEveryCollect = monadicST $ do
  let maxParties = 20
  ctx@HydraContext{ctxContestationPeriod} <- pick $ genHydraContext maxParties
  cctx <- pick $ pickChainContext ctx
  -- Init
  txInit <- pick $ genInitTx ctx
  -- Commits
  commits <- pick $ genCommits' (genUTxOAdaOnlyOfSize 1) ctx txInit
  let (committed, stInitial) = unsafeObserveInitAndCommits cctx txInit commits
  -- Collect
  let initialUTxO = fold committed
  let txCollect = collect cctx stInitial
  stOpen <- mfail $ snd <$> observeCollect stInitial txCollect
  -- Close
  (closeLower, closeUpper) <- pick $ genValidityBoundsFromContestationPeriod ctxContestationPeriod
  let txClose = close cctx stOpen InitialSnapshot{initialUTxO} closeLower closeUpper
  (deadline, stClosed) <- case observeClose stOpen txClose of
    Just (OnCloseTx{contestationDeadline}, st) -> pure (contestationDeadline, st)
    _ -> fail "not observed close"
  -- Fanout
  let txFanout = fanout stClosed initialUTxO (Fixture.slotNoFromUTCTime deadline)

  -- Properties
  let collectFails =
        propTransactionFailsEvaluation (txCollect, getKnownUTxO stInitial)
          & counterexample "collect passed, but others failed?"
          & cover 10 True "collect failed already"
  let collectCloseAndFanoutPass =
        conjoin
          [ propTransactionEvaluates (txCollect, getKnownUTxO stInitial)
              & counterexample "collect failed"
          , propTransactionEvaluates (txClose, getKnownUTxO stOpen)
              & counterexample "close failed"
          , propTransactionEvaluates (txFanout, getKnownUTxO stClosed)
              & counterexample "fanout failed"
          ]
          & cover 10 True "collect, close and fanout passed"
  pure $
    checkCoverage
      (collectFails .||. collectCloseAndFanoutPass)

--
-- Generic Properties
--

propBelowSizeLimit ::
  Natural ->
  ((UTxO -> Tx -> Property) -> Property) ->
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

propIsValid ::
  ((UTxO -> Tx -> Property) -> Property) ->
  SpecWith ()
propIsValid forAllTx =
  prop "validates within maxTxExecutionUnits" $
    forAllTx $ \utxo tx -> propTransactionEvaluates (tx, utxo)

--
-- QuickCheck Extras
--

-- TODO: These forAllXX functions are hard to use and understand. Maybe simple
-- 'Gen' or functions in 'PropertyM' are better combinable?

forAllInit ::
  (Testable property) =>
  (UTxO -> Tx -> property) ->
  Property
forAllInit action =
  forAllBlind (genHydraContext maximumNumberOfParties) $ \ctx ->
    forAll (pickChainContext ctx) $ \cctx -> do
      forAll ((,) <$> genTxIn <*> genOutput (ownVerificationKey cctx)) $ \(seedIn, seedOut) -> do
        let tx = initialize cctx (ctxHeadParameters ctx) seedIn
            utxo = UTxO.singleton (seedIn, seedOut) <> getKnownUTxO cctx
         in action utxo tx
              & classify
                (length (peerVerificationKeys cctx) == 0)
                "1 party"
              & classify
                (length (peerVerificationKeys cctx) > 0)
                "2+ parties"

forAllCommit ::
  (Testable property) =>
  (UTxO -> Tx -> property) ->
  Property
forAllCommit action =
  forAllCommit' $ \ctx st toCommit tx ->
    let utxo = getKnownUTxO st <> toCommit <> getKnownUTxO ctx
     in action utxo tx

forAllCommit' ::
  (Testable property) =>
  (ChainContext -> InitialState -> UTxO -> Tx -> property) ->
  Property
forAllCommit' action = do
  forAll (genHydraContext maximumNumberOfParties) $ \hctx ->
    forAll (genStInitial hctx) $ \(ctx, stInitial) ->
      forAllShow genCommit renderUTxO $ \toCommit ->
        let tx = unsafeCommit ctx stInitial toCommit
         in action ctx stInitial toCommit tx
              & classify
                (null toCommit)
                "Empty commit"
              & classify
                (not (null toCommit))
                "Non-empty commit"
              & counterexample ("tx: " <> renderTx tx)

forAllNonEmptyByronCommit ::
  (PostTxError Tx -> Property) ->
  Property
forAllNonEmptyByronCommit action = do
  forAll (genHydraContext maximumNumberOfParties) $ \hctx ->
    forAll (genStInitial hctx) $ \(ctx, stInitial) ->
      forAllShow genByronCommit renderUTxO $ \utxo ->
        case commit ctx stInitial utxo of
          Right{} -> property False
          Left e -> action e

forAllAbort ::
  (Testable property) =>
  (UTxO -> Tx -> property) ->
  Property
forAllAbort action = do
  forAll (genHydraContext maximumNumberOfParties) $ \ctx ->
    forAll (pickChainContext ctx) $ \cctx ->
      forAllBlind (genInitTx ctx) $ \initTx -> do
        forAllBlind (sublistOf =<< genCommits ctx initTx) $ \commits ->
          let (committed, stInitialized) = unsafeObserveInitAndCommits cctx initTx commits
              utxo = getKnownUTxO stInitialized <> getKnownUTxO cctx
           in action utxo (abort cctx stInitialized (fold committed))
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
  (UTxO -> Tx -> property) ->
  Property
forAllCollectCom action =
  forAllBlind genCollectComTx $ \(ctx, committedUTxO, stInitialized, tx) ->
    let utxo = getKnownUTxO stInitialized <> getKnownUTxO ctx
     in action utxo tx
          & counterexample ("Committed UTxO: " <> show committedUTxO)

forAllClose ::
  (Testable property) =>
  (UTxO -> Tx -> property) ->
  Property
forAllClose action = do
  -- FIXME: we should not hardcode number of parties but generate it within bounds
  forAll (genCloseTx maximumNumberOfParties) $ \(ctx, st, tx, sn) ->
    let utxo = getKnownUTxO st <> getKnownUTxO ctx
     in action utxo tx
          & label (Prelude.head . Prelude.words . show $ sn)

forAllContest ::
  (Testable property) =>
  (UTxO -> Tx -> property) ->
  Property
forAllContest action =
  forAllBlind genContestTx $ \(hctx@HydraContext{ctxContestationPeriod}, closePointInTime, stClosed, tx) ->
    -- XXX: Pick an arbitrary context to contest. We will stumble over this when
    -- we make contests only possible once per party.
    forAllBlind (pickChainContext hctx) $ \ctx ->
      let utxo = getKnownUTxO stClosed <> getKnownUTxO ctx
       in action utxo tx
            & counterexample ("Contestation deadline: " <> show (getContestationDeadline stClosed))
            & counterexample ("Contestation period: " <> show ctxContestationPeriod)
            & counterexample ("Close point: " <> show closePointInTime)
            & counterexample ("Closed contesters: " <> show (getClosedContesters stClosed))
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

  getClosedContesters stClosed = closedContesters . closedThreadOutput $ stClosed

forAllFanout ::
  (Testable property) =>
  (UTxO -> Tx -> property) ->
  Property
forAllFanout action =
  -- TODO: The utxo to fanout should be more arbitrary to have better test coverage
  forAll (sized $ \n -> genFanoutTx maximumNumberOfParties (n `min` maxSupported)) $ \(hctx, stClosed, tx) ->
    forAllBlind (pickChainContext hctx) $ \ctx ->
      let utxo = getKnownUTxO stClosed <> getKnownUTxO ctx
       in action utxo tx
            & label ("Fanout size: " <> prettyLength (countAssets $ txOuts' tx))
 where
  maxSupported = 35

  countAssets = getSum . foldMap (Sum . valueSize . txOutValue)

  prettyLength len
    | len > maxSupported = "> " <> show maxSupported <> " ???"
    | len >= 40 = "40-" <> show maxSupported
    | len >= 10 = "10-40"
    | len >= 1 = "1-10"
    | otherwise = "0"

--
-- Generators
--

genByronCommit :: Gen UTxO
genByronCommit = do
  input <- arbitrary
  addr <- ByronAddressInEra <$> arbitrary
  value <- genValue
  pure $ UTxO.singleton (input, TxOut addr value TxOutDatumNone ReferenceScriptNone)

-- * Helpers

mfail :: MonadFail m => Maybe a -> m a
mfail = \case
  Nothing -> fail "encountered Nothing"
  Just a -> pure a
