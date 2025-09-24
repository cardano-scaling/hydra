{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  SlotNo,
  Tx,
  TxIn,
  TxOut,
  UTxO,
  findRedeemerSpending,
  genTxIn,
  getTxBody,
  getTxId,
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
 )
import Hydra.Cardano.Api.Pretty (renderTx, renderTxWithUTxO)
import Hydra.Chain (OnChainTx (..), PostTxError (..), maxMainnetLovelace, maximumNumberOfParties)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (..),
  ClosedState (..),
  HasKnownUTxO (getKnownUTxO),
  HydraContext (..),
  InitialState (..),
  OpenState (..),
  abort,
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
  genDecrementTx,
  genDepositTx,
  genFanoutTx,
  genHydraContext,
  genIncrementTx,
  genInitTx,
  genRecoverTx,
  genStInitial,
  getKnownUTxO,
  initialize,
  maxGenParties,
  observeClose,
  observeCollect,
  observeCommit,
  pickChainContext,
  unsafeAbort,
  unsafeClose,
  unsafeCollect,
  unsafeCommit,
  unsafeFanout,
  unsafeIncrement,
  unsafeObserveInitAndCommits,
 )
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.Contract.Dummy (dummyMintingScript)
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx,
  genValidityBoundsFromContestationPeriod,
  maxTxSize,
  propTransactionEvaluates,
  propTransactionFailsEvaluation,
 )
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Plutus (initialValidatorScript)
import Hydra.Tx.ContestationPeriod (toNominalDiffTime)
import Hydra.Tx.Deposit (DepositObservation (..), observeDepositTx)
import Hydra.Tx.Observe (
  AbortObservation (..),
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  IncrementObservation (..),
  NotAnInitReason (..),
  observeCommitTx,
  observeDecrementTx,
  observeHeadTx,
  observeIncrementTx,
  observeInitTx,
 )
import Hydra.Tx.Recover (RecoverObservation (..), observeRecoverTx)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (InitialSnapshot, initialUTxO))
import Hydra.Tx.Snapshot qualified as Snapshot
import Hydra.Tx.Utils (dummyValidatorScript, splitUTxO)
import PlutusLedgerApi.V3 qualified as Plutus
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, SpecWith, describe, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId)
import Test.Hydra.Tx.Gen (genOutput, genTxOut, genTxOutAdaOnly, genTxOutByron, genUTxO1, genUTxOSized)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  applyMutation,
  modifyInlineDatum,
  replaceHeadId,
  replacePolicyIdWith,
 )
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
  forAllShrink,
  getPositive,
  label,
  sublistOf,
  tabulate,
  (.&&.),
  (.||.),
  (===),
  (==>),
 )
import Test.QuickCheck.Monadic (assert, assertWith, monadicIO, monadicST, monitor, pick)
import Prelude qualified

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecs (Proxy @Plutus.PubKeyHash)

  describe "observeTx" $ do
    -- TODO: DRY with TxSpec
    prop "All valid transitions for all possible states can be observed." prop_observeAnyTx

  describe "splitUTxO" $ do
    prop "it splits at least one utxo off" prop_splitUTxO

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
        let utxo = UTxO.singleton seedInput seedTxOut
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
    -- stateful layer anyways.
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

  describe "deposit" $ do
    propBelowSizeLimit maxTxSize forAllDeposit
    propIsValid forAllDeposit

    prop "observes deposit" $
      forAllDeposit $ \utxo tx ->
        case observeDepositTx testNetworkId tx of
          Just DepositObservation{} -> property True
          Nothing ->
            False & counterexample ("observeDepositTx ignored transaction: " <> renderTxWithUTxO utxo tx)

  describe "recover" $ do
    propBelowSizeLimit maxTxSize forAllRecover
    propIsValid forAllRecover

    prop "observes recover" $
      forAllRecover $ \utxo tx ->
        case observeRecoverTx testNetworkId utxo tx of
          Just RecoverObservation{} -> property True
          Nothing ->
            False & counterexample ("observeRecoverTx ignored transaction: " <> renderTxWithUTxO utxo tx)

  describe "increment" $ do
    propBelowSizeLimit maxTxSize forAllIncrement
    propIsValid forAllIncrement
    it "increment observation observes correct utxo" prop_incrementObservesCorrectUTxO

  describe "decrement" $ do
    propBelowSizeLimit maxTxSize forAllDecrement
    propIsValid forAllDecrement

  prop "observes distributed outputs" $
    forAllDecrement' $ \toDistribute utxo tx ->
      case observeDecrementTx utxo tx of
        Just DecrementObservation{distributedUTxO} ->
          UTxO.txOutputs distributedUTxO === UTxO.txOutputs toDistribute
        Nothing ->
          False & counterexample ("observeDecrementTx ignored transaction: " <> renderTxWithUTxO utxo tx)

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
          ChangeMintingPolicy alwaysSucceedsV3
            : fmap changeMintingPolicy (zip changedOutputsValue [0 ..])
      , "new minting policy: " <> show (hashScript $ PlutusScript alwaysSucceedsV3)
      , NotAHeadPolicy
      )

  -- We do replace the minting policy of all tokens and datum of a head output to
  -- simulate a faked init transaction.
  alwaysSucceedsV3 = dummyMintingScript
  originalPolicyId = HeadTokens.headPolicyId seedInput
  fakePolicyId = scriptPolicyId $ PlutusScript alwaysSucceedsV3
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
      UTxO.find (isScriptTxOut initialValidatorScript) resolvedInputs

  resolvedInputs =
    UTxO.fromList $
      mapMaybe (\txIn -> (txIn,) <$> UTxO.resolveTxIn txIn utxo) (txIns' tx)

  initialRedeemer =
    fromMaybe (error "not found redeemer") $
      findRedeemerSpending @Initial.RedeemerType tx initialTxIn

  fakeScriptAddress = mkScriptAddress testNetworkId fakeScript

  fakeScript = dummyValidatorScript

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
    forAllShow genChainStateWithTx (("Transition: " <>) . showTransition) $ \(ctx, st, additionalUTxO, tx, transition) ->
      forAllShow genChainStateWithTx (("Some other transition: " <>) . showTransition) $ \(_, otherSt, additionalUTxO', _, _) -> do
        genericCoverTable [transition] $ do
          let expectedHeadId = chainStateHeadId st
              utxo = getKnownUTxO st <> getKnownUTxO otherSt <> additionalUTxO <> additionalUTxO'
          case observeHeadTx (networkId ctx) utxo tx of
            NoHeadTx ->
              False & counterexample ("observeHeadTx ignored transaction: " <> renderTxWithUTxO utxo tx)
            -- NOTE: we don't have the generated headId easily accessible in the initial state
            Init{} -> transition === Transition.Init
            Commit CommitObservation{headId} -> transition === Transition.Commit .&&. Just headId === expectedHeadId
            Abort AbortObservation{headId} -> transition === Transition.Abort .&&. Just headId === expectedHeadId
            CollectCom CollectComObservation{headId} -> transition === Transition.Collect .&&. Just headId === expectedHeadId
            Deposit DepositObservation{} -> property False
            Recover RecoverObservation{} -> property False
            Increment IncrementObservation{headId} -> transition === Transition.Increment .&&. Just headId === expectedHeadId
            Decrement DecrementObservation{headId} -> transition === Transition.Decrement .&&. Just headId === expectedHeadId
            Close CloseObservation{headId} -> transition === Transition.Close .&&. Just headId === expectedHeadId
            Contest ContestObservation{headId} -> transition === Transition.Contest .&&. Just headId === expectedHeadId
            Fanout FanoutObservation{headId} -> transition === Transition.Fanout .&&. Just headId === expectedHeadId
 where
  showTransition :: (a, b, c, d, Transition.ChainTransition) -> String
  showTransition (_, _, _, _, t) = show t

  chainStateHeadId = \case
    Idle{} -> Nothing
    Initial InitialState{headId} -> Just headId
    Open OpenState{headId} -> Just headId
    Closed ClosedState{headId} -> Just headId

-- | Given a UTxO with more than one entry, we can split it into two non-empty UTxO.
prop_splitUTxO :: UTxO -> Property
prop_splitUTxO utxo =
  (UTxO.size utxo > 1) ==>
    let (inHead, toDecommit) = splitUTxO utxo
     in conjoin
          [ not (UTxO.null inHead) & counterexample "inHead is empty"
          , not (UTxO.null toDecommit) & counterexample "toDecommit is empty"
          , inHead /= toDecommit & counterexample "inHead == toDecommit"
          ]

prop_canCloseFanoutEveryCollect :: Property
prop_canCloseFanoutEveryCollect = monadicST $ do
  let moreThanSupported = maximumNumberOfParties * 2
  ctx@HydraContext{ctxContestationPeriod} <- pickBlind $ genHydraContext moreThanSupported
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
      txClose = unsafeClose cctx closeUTxO headId (ctxHeadParameters ctx) 0 InitialSnapshot{headId, initialUTxO} closeLower closeUpper
  (deadline, stClosed) <- case observeClose stOpen txClose of
    Just (OnCloseTx{contestationDeadline}, st) -> pure (contestationDeadline, st)
    _ -> fail "not observed close"
  -- Fanout
  let fanoutUTxO = getKnownUTxO stClosed
  let txFanout = unsafeFanout cctx fanoutUTxO seedTxIn initialUTxO Nothing Nothing (slotNoFromUTCTime systemStart slotLength deadline)

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

prop_incrementObservesCorrectUTxO :: Property
prop_incrementObservesCorrectUTxO = monadicIO $ do
  (ctx, st@OpenState{headId}, _, txDeposit) <- pickBlind $ genDepositTx maxGenParties
  (_, _, _, txDeposit2) <- pickBlind $ genDepositTx maxGenParties
  case observeDepositTx (ctxNetworkId ctx) txDeposit of
    Nothing -> assertWith False "Deposit not observed"
    Just DepositObservation{depositTxId = depositedTxId, deadline} -> do
      cctx <- pickBlind $ pickChainContext ctx
      let slotNo = slotNoFromUTCTime systemStart slotLength deadline
      let version = 0
      let openUTxO = getKnownUTxO st
      -- NOTE: Use second deposit utxo deliberately here to test that the
      -- increment observation picks the correct one.
      -- We rely here on a fact that eventually this property will generate
      -- UTxO which would be wrongly picked up by the increment observation.
      let utxo = getKnownUTxO st <> utxoFromTx txDeposit <> utxoFromTx txDeposit2
      snapshot <- pickBlind $ Snapshot.genConfirmedSnapshot headId version 1 openUTxO (Just utxo) Nothing (ctxHydraSigningKeys ctx)
      let txIncrement = unsafeIncrement cctx utxo headId (ctxHeadParameters ctx) snapshot depositedTxId slotNo
      case observeIncrementTx utxo txIncrement of
        Nothing -> assertWith False "Increment not observed"
        Just IncrementObservation{depositTxId} -> do
          let txDepositId = getTxId (getTxBody txDeposit)
          monitor (counterexample $ "Expected TxId:" <> show depositTxId <> " Actual TxId:" <> show txDepositId)
          assert (depositTxId == txDepositId)

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
  showKB :: (Show i, Integral i) => i -> String
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
            utxo = UTxO.singleton seedIn seedOut <> getKnownUTxO cctx
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
                (UTxO.null toCommit)
                "Empty commit"
              & classify
                (not (UTxO.null toCommit))
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
  forAllBlind genCollectComTx $ \(ctx, committedUTxO, stInitialized, _, tx) ->
    let utxo = getKnownUTxO stInitialized <> getKnownUTxO ctx
     in action utxo tx
          & counterexample ("Committed UTxO: " <> show committedUTxO)

forAllDeposit ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllDeposit action = do
  forAllShrink (genDepositTx maximumNumberOfParties) shrink $ \(_ctx, st, depositUTxO, tx) ->
    let utxo = getKnownUTxO st <> depositUTxO
     in action utxo tx

forAllRecover ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllRecover action = do
  forAllShrink genRecoverTx shrink $ uncurry action

forAllIncrement ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllIncrement action = do
  forAllIncrement' $ \utxo tx ->
    action utxo tx

forAllIncrement' ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllIncrement' action = do
  forAllShrink (genIncrementTx maximumNumberOfParties) shrink $ \(ctx, st, incrementUTxO, tx) ->
    let utxo = getKnownUTxO st <> getKnownUTxO ctx <> incrementUTxO
     in action utxo tx

forAllDecrement ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllDecrement action = do
  forAllDecrement' $ \_ utxo tx ->
    action utxo tx

forAllDecrement' ::
  Testable property =>
  (UTxO -> UTxO -> Tx -> property) ->
  Property
forAllDecrement' action = do
  forAllShrink (genDecrementTx maximumNumberOfParties) shrink $ \(ctx, distributed, st, utxo', tx) ->
    let utxo = getKnownUTxO st <> getKnownUTxO ctx <> utxo'
     in action distributed utxo tx

forAllClose ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllClose action = do
  -- FIXME: we should not hardcode number of parties but generate it within bounds
  forAll (genCloseTx maximumNumberOfParties) $ \(ctx, st, _, tx, sn) ->
    let utxo = getKnownUTxO st <> getKnownUTxO ctx
     in action utxo tx
          & label (Prelude.head . Prelude.words . show $ sn)

forAllContest ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllContest action =
  -- XXX: This is always generating a fresh closed state with no previous contests
  forAllBlind genContestTx $ \(hctx@HydraContext{ctxContestationPeriod}, closePointInTime, stClosed, _, tx) ->
    -- XXX: Pick an arbitrary context to contest. We will stumble over this when
    -- we make contests only possible once per party.
    forAllBlind (pickChainContext hctx) $ \ctx ->
      let utxo = getKnownUTxO stClosed <> getKnownUTxO ctx
       in action utxo tx
            & counterexample ("Contestation deadline: " <> show stClosed.contestationDeadline)
            & counterexample ("Contestation period: " <> show ctxContestationPeriod)
            & counterexample ("Close point: " <> show closePointInTime)
            & tabulate "Contestation period" (tabulateContestationPeriod ctxContestationPeriod)
            & tabulate "Close point (slot)" (tabulateNum $ fst closePointInTime)
 where
  tabulateNum :: SlotNo -> [String]
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

forAllFanout ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllFanout action =
  -- TODO: The utxo to fanout should be more arbitrary to have better test coverage
  forAll (genFanoutTx maximumNumberOfParties) $ \(ctx, stClosed, _, tx) ->
    let utxo = getKnownUTxO stClosed <> getKnownUTxO ctx
     in action utxo tx
          & label ("Fanout size: " <> prettyLength (countAssets $ txOuts' tx))
 where
  maxSupported :: Int
  maxSupported = 44

  countAssets :: [TxOut ctx] -> Int
  countAssets = getSum . foldMap (Sum . valueSize . txOutValue)

  prettyLength :: Int -> String
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
