{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (
  SlotNo,
  Tx,
  TxIn,
  TxOut,
  UTxO,
  getTxBody,
  getTxId,
  hashScript,
  scriptPolicyId,
  toPlutusCurrencySymbol,
  txOutValue,
  txOuts',
  utxoFromTx,
  valueSize,
  pattern PlutusScript,
 )
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Cardano.Api.Pretty (renderTx, renderTxWithUTxO)
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (..),
  ClosedState (..),
  HasKnownUTxO (getKnownUTxO),
  HydraContext (..),
  OpenState (..),
  ctxHeadParameters,
  ctxParticipants,
  getKnownUTxO,
  initialize,
  unsafeIncrement,
 )
import Hydra.Contract.Dummy (dummyMintingScript)
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Tx (txInToHeadSeed)
import Hydra.Tx.ContestationPeriod (toNominalDiffTime)
import Hydra.Tx.Deposit (DepositObservation (..), observeDepositTx)
import Hydra.Tx.Observe (
  CloseObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  IncrementObservation (..),
  NotAnInitReason (..),
  observeDecrementTx,
  observeHeadTx,
  observeIncrementTx,
  observeInitTx,
 )
import Hydra.Tx.Recover (RecoverObservation (..), observeRecoverTx)
import Hydra.Tx.Utils (splitUTxO)
import PlutusLedgerApi.V3 qualified as Plutus
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Chain.Direct.State (
  ChainTransition,
  genChainStateWithTx,
  genCloseTx,
  genContestTx,
  genDecrementTx,
  genDepositTx,
  genFanoutTx,
  genHydraContext,
  genIncrementTx,
  genRecoverTx,
  maxGenParties,
  pickChainContext,
 )
import Test.Hydra.Chain.Direct.State qualified as Transition
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, maxTxSize)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId)
import Test.Hydra.Tx.Gen (genConfirmedSnapshot, genOutputFor, genTxOutAdaOnly, propTransactionEvaluates)
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
  forAll,
  forAllBlind,
  forAllShow,
  forAllShrink,
  label,
  tabulate,
  (.&&.),
  (===),
  (==>),
 )
import Test.QuickCheck.Monadic (assert, assertWith, monadicIO, monitor)
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
            Deposit DepositObservation{} -> property False
            Recover RecoverObservation{} -> property False
            Increment IncrementObservation{headId} -> transition === Transition.Increment .&&. Just headId === expectedHeadId
            Decrement DecrementObservation{headId} -> transition === Transition.Decrement .&&. Just headId === expectedHeadId
            Close CloseObservation{headId} -> transition === Transition.Close .&&. Just headId === expectedHeadId
            Contest ContestObservation{headId} -> transition === Transition.Contest .&&. Just headId === expectedHeadId
            Fanout FanoutObservation{headId} -> transition === Transition.Fanout .&&. Just headId === expectedHeadId
 where
  showTransition :: (a, b, c, d, ChainTransition) -> String
  showTransition (_, _, _, _, t) = show t

  chainStateHeadId = \case
    Idle{} -> Nothing
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

prop_incrementObservesCorrectUTxO :: Property
prop_incrementObservesCorrectUTxO = monadicIO $ do
  (ctx, st@OpenState{headId, seedTxIn}, _, txDeposit) <- pickBlind $ genDepositTx maxGenParties
  (_, _, _, txDeposit2) <- pickBlind $ genDepositTx maxGenParties
  let networkId = ctxNetworkId ctx
  case observeDepositTx networkId txDeposit of
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
      snapshot <- pickBlind $ genConfirmedSnapshot headId version 1 openUTxO (Just utxo) Nothing (ctxHydraSigningKeys ctx)
      let txIncrement =
            unsafeIncrement
              cctx
              utxo
              (txInToHeadSeed seedTxIn, headId)
              (ctxHeadParameters ctx)
              snapshot
              depositedTxId
              slotNo
      case observeIncrementTx networkId utxo txIncrement of
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
      forAll ((,) <$> genTxIn <*> genOutputFor (ownVerificationKey cctx)) $ \(seedIn, seedOut) -> do
        let tx = initialize cctx seedIn (ctxParticipants ctx) (ctxHeadParameters ctx)
            utxo = UTxO.singleton seedIn seedOut <> getKnownUTxO cctx
         in action utxo tx
              & classify
                (null (ctxVerificationKeys ctx))
                "1 party"
              & classify
                (not (null (ctxVerificationKeys ctx)))
                "2+ parties"

forAllDeposit ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllDeposit action = do
  forAllShrink (genDepositTx maximumNumberOfParties) shrink $ \(_ctx, st, utxoToDeposit, tx) ->
    let utxo = getKnownUTxO st <> utxoToDeposit
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
    forAllBlind (pickChainContext ctx) $ \cctx ->
      let utxo = getKnownUTxO st <> getKnownUTxO cctx <> incrementUTxO
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

-- * Helpers

mfail :: MonadFail m => Maybe a -> m a
mfail = \case
  Nothing -> fail "encountered Nothing"
  Just a -> pure a
