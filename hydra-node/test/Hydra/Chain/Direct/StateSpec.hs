{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Hydra.Cardano.Api (
  NetworkId (Mainnet),
  PlutusScriptV2,
  Tx,
  TxIn,
  UTxO,
  findRedeemerSpending,
  fromPlutusScript,
  genTxIn,
  hashScript,
  isScriptTxOut,
  lovelaceToValue,
  mkScriptAddress,
  modifyTxOutAddress,
  modifyTxOutValue,
  scriptPolicyId,
  toPlutusCurrencySymbol,
  toScriptData,
  txInputSet,
  txIns',
  txOutValue,
  txOuts',
  utxoFromTx,
  valueSize,
  pattern PlutusScript,
  pattern PlutusScriptSerialised,
 )
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain (OnChainTx (..), PostTxError (..), maxMainnetLovelace, maximumNumberOfParties)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  applyMutation,
  modifyInlineDatum,
  replaceHeadId,
  replacePolicyIdWith,
 )
import Hydra.Chain.Direct.Fixture (slotLength, systemStart, testNetworkId)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (..),
  ClosedState (..),
  HasKnownUTxO (getKnownUTxO),
  HydraContext (..),
  InitialState (..),
  OpenState (..),
  abort,
  closedThreadOutput,
  commit,
  ctxHeadParameters,
  ctxParticipants,
  ctxParties,
  genChainStateWithTx,
  genCloseTx,
  genCollectComTx,
  genCommitFor,
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
  observeClose,
  observeCollect,
  observeCommit,
  pickChainContext,
  unsafeAbort,
  unsafeClose,
  unsafeCollect,
  unsafeCommit,
  unsafeFanout,
  unsafeObserveInitAndCommits,
 )
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.Chain.Direct.Tx (
  AbortObservation (..),
  CloseObservation (..),
  ClosedThreadOutput (closedContesters),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  NotAnInitReason (..),
  observeCommitTx,
  observeHeadTx,
  observeInitTx,
 )
import Hydra.ContestationPeriod (toNominalDiffTime)
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
import Hydra.Ledger.Cardano (
  genOutput,
  genTxOut,
  genTxOutAdaOnly,
  genTxOutByron,
  genUTxO1,
  genUTxOSized,
 )
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx,
  genValidityBoundsFromContestationPeriod,
  maxTxSize,
  propTransactionEvaluates,
  propTransactionFailsEvaluation,
 )
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Snapshot (ConfirmedSnapshot (InitialSnapshot, initialUTxO))
import Hydra.Snapshot qualified as Snapshot
import PlutusLedgerApi.Test.Examples qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (
  Property,
  Testable (property),
  checkCoverage,
  classify,
  conjoin,
  counterexample,
  cover,
  forAll,
  forAllBlind,
  forAllShow,
  getPositive,
  label,
  sized,
  sublistOf,
  tabulate,
  (.&&.),
  (.||.),
  (===),
 )
import Test.QuickCheck.Monadic (monadicIO, monadicST, pick)
import Prelude qualified

spec :: Spec
spec = parallel $ do
  describe "ChainState" $
    roundtripAndGoldenSpecs (Proxy @ChainState)

  describe "Plutus.PubKeyHash" $
    roundtripAndGoldenSpecs (Proxy @Plutus.PubKeyHash)

  describe "observeTx" $ do
    prop "All valid transitions for all possible states can be observed." prop_observeAnyTx

  describe "init" $ do
    propBelowSizeLimit maxTxSize forAllInit
    propIsValid forAllInit

    -- XXX: This is testing observeInitTx (we will get rid of 'observeInit')
    it "only proper head is observed" $
      monadicIO $ do
        ctx <- pickBlind (genHydraContext maximumNumberOfParties)
        cctx <- pickBlind $ pickChainContext ctx
        seedInput <- pickBlind arbitrary
        vk <- pickBlind arbitrary
        seedTxOut <- pickBlind $ genTxOutAdaOnly vk

        let tx = initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)
        (mutation, cex, expected) <- pickBlind $ genInitTxMutation seedInput tx
        let utxo = UTxO.singleton (seedInput, seedTxOut)
        let (tx', utxo') = applyMutation mutation (tx, utxo)

            originalIsObserved = property $ isRight (observeInitTx tx)

            -- We expected mutated transaction to still be valid, but not observed.
            mutatedIsValid = property $
              case evaluateTx tx' utxo' of
                Left _ -> False
                Right ok
                  | all isRight ok -> True
                  | otherwise -> False

            mutatedIsNotObserved =
              observeInitTx tx' === Left expected

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
            & counterexample cex
            & label (show expected)

  describe "commit" $ do
    propBelowSizeLimit maxTxSize forAllCommit
    propIsValid forAllCommit

    -- XXX: This is testing observeCommitTx. Eventually we will get rid of the
    -- state-ful layer anyways.
    it "only proper head is observed" $
      forAllCommit' $ \ctx st committedUtxo tx ->
        monadicIO $ do
          let utxo = getKnownUTxO ctx <> getKnownUTxO st <> committedUtxo
          mutation <- pick $ genCommitTxMutation utxo tx
          let (tx', utxo') = applyMutation mutation (tx, utxo)

              originalIsObserved = property $ isJust $ observeCommitTx testNetworkId utxo tx

              -- We expected mutated transaction to still be valid, but not observed.
              mutatedIsValid =
                case evaluateTx tx' utxo' of
                  Left err -> property False & counterexample (show err)
                  Right ok
                    | all isRight ok -> property True
                    | otherwise -> property False & counterexample (show ok)

              mutatedIsNotObserved =
                isNothing $ observeCommitTx testNetworkId utxo' tx'

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

    prop "reject committing outputs with byron addresses" $
      monadicST $ do
        hctx <- pickBlind $ genHydraContext maximumNumberOfParties
        (ctx, stInitial@InitialState{headId}) <- pickBlind $ genStInitial hctx
        utxo <- pick $ genUTxO1 genTxOutByron
        pure $
          case commit ctx headId (getKnownUTxO stInitial) utxo of
            Left UnsupportedLegacyOutput{} -> property True
            _ -> property False

    prop "reject Commits with more than maxMainnetLovelace Lovelace" $
      monadicST $ do
        hctx <- pickBlind $ genHydraContext maximumNumberOfParties
        (ctx, stInitial@InitialState{headId}) <- pickBlind $ genStInitial hctx
        utxo <- pickBlind genAdaOnlyUTxOOnMainnetWithAmountBiggerThanOutLimit
        let mainnetChainContext = ctx{networkId = Mainnet}
        pure $
          case commit mainnetChainContext headId (getKnownUTxO stInitial) utxo of
            Left CommittedTooMuchADAForMainnet{userCommittedLovelace, mainnetLimitLovelace} ->
              -- check that user committed more than our limit but also use 'maxMainnetLovelace'
              -- to be sure we didn't construct 'CommittedTooMuchADAForMainnet' wrongly
              property $ userCommittedLovelace > mainnetLimitLovelace && userCommittedLovelace > maxMainnetLovelace
            _ -> property False

  describe "abort" $ do
    propBelowSizeLimit maxTxSize forAllAbort
    propIsValid forAllAbort

    -- XXX: This is something we should test for all tx creation functions.
    -- Maybe extend the forAllXXX generators to work on artificially duplicated,
    -- compatible UTxOs.
    prop "can create valid abort transactions for any observed head" $
      monadicST $ do
        hctx <- pickBlind $ genHydraContext maximumNumberOfParties
        ctx <- pickBlind $ pickChainContext hctx
        -- Generate a head in initialized state
        (initTx1, seed1) <- pickBlind $ genInitTxWithSeed hctx
        -- Generate another head in initialized state
        (initTx2, seed2) <- pickBlind $ genInitTxWithSeed hctx
        -- Expect to create abort transactions for either head
        let utxo = getKnownUTxO ctx <> utxoFromTx initTx1 <> utxoFromTx initTx2
        let propIsValidAbortTx res =
              case res of
                Left err -> property False & counterexample ("Failed to create abort: " <> show err)
                Right tx -> propTransactionEvaluates (tx, utxo)
        pure $
          conjoin
            [ propIsValidAbortTx (abort ctx seed1 utxo mempty)
                & counterexample "AbortTx of head 1"
            , propIsValidAbortTx (abort ctx seed2 utxo mempty)
                & counterexample "AbortTx of head 2"
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

genInitTxMutation :: TxIn -> Tx -> Gen (Mutation, String, NotAnInitReason)
genInitTxMutation seedInput tx =
  genChangeMintingPolicy
 where
  genChangeMintingPolicy =
    pure
      ( Changes $
          ChangeMintingPolicy alwaysSucceedsV2
            : fmap changeMintingPolicy (zip changedOutputsValue [0 ..])
      , "new minting policy: " <> show (hashScript $ PlutusScript alwaysSucceedsV2)
      , NotAHeadPolicy
      )

  -- We do replace the minting policy of all tokens and datum of a head output to
  -- simulate a faked init transaction.
  alwaysSucceedsV2 = PlutusScriptSerialised $ Plutus.alwaysSucceedingNAryFunction 2
  originalPolicyId = HeadTokens.headPolicyId seedInput
  fakePolicyId = scriptPolicyId $ PlutusScript alwaysSucceedsV2
  changeMintingPolicy (out, idx)
    | idx == 0 = ChangeOutput idx $ modifyInlineDatum (replaceHeadId $ toPlutusCurrencySymbol fakePolicyId) out
    | otherwise = ChangeOutput idx out
  changedOutputsValue = replacePolicyIdWith originalPolicyId fakePolicyId <$> txOuts' tx

genCommitTxMutation :: UTxO -> Tx -> Gen Mutation
genCommitTxMutation utxo tx =
  mutateInitialAddress
 where
  mutateInitialAddress = do
    let mutatedTxOut = modifyTxOutAddress (const fakeScriptAddress) initialTxOut
    pure $
      Changes
        [ ChangeInput initialTxIn mutatedTxOut (Just $ toScriptData initialRedeemer)
        , AddScript fakeScript
        ]

  (initialTxIn, initialTxOut) =
    fromMaybe (error "not found initial script") $
      UTxO.find (isScriptTxOut @PlutusScriptV2 initialScript) resolvedInputs

  resolvedInputs =
    UTxO.fromPairs $
      mapMaybe (\txIn -> (txIn,) <$> UTxO.resolve txIn utxo) (txIns' tx)

  initialRedeemer =
    fromMaybe (error "not found redeemer") $
      findRedeemerSpending @Initial.RedeemerType tx initialTxIn

  initialScript = fromPlutusScript Initial.validatorScript

  fakeScriptAddress = mkScriptAddress @PlutusScriptV2 testNetworkId fakeScript

  fakeScript = fromPlutusScript $ Plutus.alwaysSucceedingNAryFunction 3

genAdaOnlyUTxOOnMainnetWithAmountBiggerThanOutLimit :: Gen UTxO
genAdaOnlyUTxOOnMainnetWithAmountBiggerThanOutLimit = do
  adaAmount <- (+ maxMainnetLovelace) . getPositive <$> arbitrary
  genUTxO1 (modifyTxOutValue (const $ lovelaceToValue adaAmount) <$> genTxOut)

-- * Properties

-- | Given any Head protocol state and the transaction corresponding a protocol
-- transition we should be able to observe this transition correctly even in
-- presence of other valid Hydra Head protocol states in the used lookup utxo.
prop_observeAnyTx :: Property
prop_observeAnyTx =
  checkCoverage $ do
    forAllShow genChainStateWithTx showTransition $ \(ctx, st, tx, transition) ->
      forAllShow genChainStateWithTx showTransition $ \(_, otherSt, _, _) ->
        genericCoverTable [transition] $ do
          let expectedHeadId = chainStateHeadId st
          case observeHeadTx (networkId ctx) (getKnownUTxO st <> getKnownUTxO otherSt) tx of
            NoHeadTx ->
              False & counterexample ("observeHeadTx ignored transaction: " <> show tx)
            -- NOTE: we don't have the generated headId easily accessible in the initial state
            Init{} -> transition === Transition.Init
            Commit CommitObservation{headId} -> transition === Transition.Commit .&&. Just headId === expectedHeadId
            Abort AbortObservation{headId} -> transition === Transition.Abort .&&. Just headId === expectedHeadId
            CollectCom CollectComObservation{headId} -> transition === Transition.Collect .&&. Just headId === expectedHeadId
            Decrement DecrementObservation{headId} -> transition === Transition.Decrement .&&. Just headId === expectedHeadId
            Close CloseObservation{headId} -> transition === Transition.Close .&&. Just headId === expectedHeadId
            Contest ContestObservation{headId} -> transition === Transition.Contest .&&. Just headId === expectedHeadId
            Fanout FanoutObservation{headId} -> transition === Transition.Fanout .&&. Just headId === expectedHeadId
 where
  showTransition (_, _, _, t) = show t

  chainStateHeadId = \case
    Idle{} -> Nothing
    Initial InitialState{headId} -> Just headId
    Open OpenState{headId} -> Just headId
    Closed ClosedState{headId} -> Just headId

prop_canCloseFanoutEveryCollect :: Property
prop_canCloseFanoutEveryCollect = monadicST $ do
  let maxParties = 10
  ctx@HydraContext{ctxContestationPeriod} <- pickBlind $ genHydraContext maxParties
  cctx <- pickBlind $ pickChainContext ctx
  -- Init
  txInit <- pickBlind $ genInitTx ctx
  -- Commits
  commits <- pickBlind $ genCommits' (genUTxOSized 1) ctx txInit
  let (committed, stInitial) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) txInit commits
  let InitialState{headId = initialHeadId} = stInitial
  -- Collect
  let initialUTxO = fold committed
  let spendableUTxO = getKnownUTxO stInitial
  let txCollect = unsafeCollect cctx initialHeadId (ctxHeadParameters ctx) initialUTxO spendableUTxO
  stOpen@OpenState{seedTxIn, headId} <- mfail $ snd <$> observeCollect stInitial txCollect
  -- Close
  (closeLower, closeUpper) <- pickBlind $ genValidityBoundsFromContestationPeriod ctxContestationPeriod
  let closeUTxO = getKnownUTxO stOpen
      txClose = unsafeClose cctx closeUTxO headId (ctxHeadParameters ctx) InitialSnapshot{headId, initialUTxO} closeLower closeUpper
  (deadline, stClosed) <- case observeClose stOpen txClose of
    Just (OnCloseTx{contestationDeadline}, st) -> pure (contestationDeadline, st)
    _ -> fail "not observed close"
  -- Fanout
  let fanoutUTxO = getKnownUTxO stClosed
  let txFanout = unsafeFanout cctx fanoutUTxO seedTxIn initialUTxO (slotNoFromUTCTime systemStart slotLength deadline)

  -- Properties
  let collectFails =
        propTransactionFailsEvaluation (txCollect, getKnownUTxO cctx <> getKnownUTxO stInitial)
          & counterexample "collect passed, but others failed?"
          & cover 10 True "collect failed already"
  let collectCloseAndFanoutPass =
        conjoin
          [ propTransactionEvaluates (txCollect, getKnownUTxO cctx <> getKnownUTxO stInitial)
              & counterexample "collect failed"
          , propTransactionEvaluates (txClose, getKnownUTxO cctx <> getKnownUTxO stOpen)
              & counterexample "close failed"
          , propTransactionEvaluates (txFanout, getKnownUTxO cctx <> getKnownUTxO stClosed)
              & counterexample "fanout failed"
          ]
          & cover 10 True "collect, close and fanout passed"
  pure $
    -- XXX: Coverage does not work if we only collectFails
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
    forAllTx $
      \utxo tx -> propTransactionEvaluates (tx, utxo)

-- * Generators

-- TODO: These forAllXX functions are hard to use and understand. Maybe simple
-- 'Gen' or functions in 'PropertyM' are better combinable?

forAllInit ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllInit action =
  forAllBlind (genHydraContext maximumNumberOfParties) $ \ctx ->
    forAll (pickChainContext ctx) $ \cctx -> do
      forAll ((,) <$> genTxIn <*> genOutput (ownVerificationKey cctx)) $ \(seedIn, seedOut) -> do
        let tx = initialize cctx seedIn (ctxParticipants ctx) (ctxHeadParameters ctx)
            utxo = UTxO.singleton (seedIn, seedOut) <> getKnownUTxO cctx
         in action utxo tx
              & classify
                (null (ctxVerificationKeys ctx))
                "1 party"
              & classify
                (not (null (ctxVerificationKeys ctx)))
                "2+ parties"

forAllCommit ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllCommit action =
  forAllCommit' $ \ctx st toCommit tx ->
    let utxo = getKnownUTxO st <> toCommit <> getKnownUTxO ctx
     in action utxo tx

forAllCommit' ::
  Testable property =>
  (ChainContext -> InitialState -> UTxO -> Tx -> property) ->
  Property
forAllCommit' action = do
  forAllBlind (genHydraContext maximumNumberOfParties) $ \hctx ->
    forAllBlind (genStInitial hctx) $ \(ctx, stInitial) ->
      forAllBlind (genCommitFor $ ownVerificationKey ctx) $ \toCommit ->
        -- TODO: generate script inputs here? <- SB: what script inputs?
        let InitialState{headId} = stInitial
            tx = unsafeCommit ctx headId (getKnownUTxO ctx <> getKnownUTxO stInitial) toCommit
         in action ctx stInitial toCommit tx
              & classify
                (null toCommit)
                "Empty commit"
              & classify
                (not (null toCommit))
                "Non-empty commit"

forAllAbort ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllAbort action = do
  forAll (genHydraContext maximumNumberOfParties) $ \ctx ->
    forAll (pickChainContext ctx) $ \cctx ->
      forAllBlind (genInitTx ctx) $ \initTx -> do
        forAllBlind (sublistOf =<< genCommits ctx initTx) $ \commits ->
          let (committed, stInitialized) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) initTx commits
              utxo = getKnownUTxO stInitialized <> getKnownUTxO cctx
              InitialState{seedTxIn} = stInitialized
           in action utxo (unsafeAbort cctx seedTxIn utxo (fold committed))
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
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllCollectCom action =
  forAllBlind genCollectComTx $ \(ctx, committedUTxO, stInitialized, tx) ->
    let utxo = getKnownUTxO stInitialized <> getKnownUTxO ctx
     in action utxo tx
          & counterexample ("Committed UTxO: " <> show committedUTxO)

forAllClose ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllClose action = do
  -- FIXME: we should not hardcode number of parties but generate it within bounds
  forAll (genCloseTx maximumNumberOfParties) $ \(ctx, st, tx, sn) ->
    let utxo = getKnownUTxO st <> getKnownUTxO ctx
     in action utxo tx
          & label (Prelude.head . Prelude.words . show $ sn)

forAllContest ::
  Testable property =>
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

  getClosedContesters = closedContesters . closedThreadOutput

forAllFanout ::
  Testable property =>
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
  maxSupported = 58

  countAssets = getSum . foldMap (Sum . valueSize . txOutValue)

  prettyLength len
    | len > maxSupported = "> " <> show maxSupported <> " ???"
    | len >= 40 = "40-" <> show maxSupported
    | len >= 10 = "10-40"
    | len >= 1 = "1-10"
    | otherwise = "0"

-- | Generate an init tx with the used seed TxIn.
genInitTxWithSeed :: HydraContext -> Gen (Tx, TxIn)
genInitTxWithSeed ctx = do
  cctx <- pickChainContext ctx
  seedTxIn <- genTxIn
  pure (initialize cctx seedTxIn (ctxParticipants ctx) (ctxHeadParameters ctx), seedTxIn)

-- * Helpers

mfail :: MonadFail m => Maybe a -> m a
mfail = \case
  Nothing -> fail "encountered Nothing"
  Just a -> pure a
