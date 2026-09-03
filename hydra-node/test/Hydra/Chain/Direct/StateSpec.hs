{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude hiding (HydraTestnet (..))

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import GHC.IsList qualified as IsList
import Hydra.Cardano.Api (
  ExecutionUnits (..),
  SlotNo,
  Tx,
  TxIn,
  TxIx (..),
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
  pattern PlutusScript,
  pattern TxIn,
 )
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Cardano.Api.Pretty (renderTx, renderTxWithUTxO)
import Hydra.Chain (PostTxError (..), maximumNumberOfParties)
import Hydra.Chain.Direct.Handlers (incrementTxBalancingMargin, rejectOversizedDeposit, serializedValueSize)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (..),
  ClosedState (..),
  HasKnownUTxO (getKnownUTxO),
  HydraContext (..),
  IncrementTxError (..),
  OpenState (..),
  PartialFanoutError (..),
  RecoverTxError (..),
  finalPartialFanout,
  getKnownUTxO,
  increment,
  initialChainState,
  initialize,
  partialFanout,
  recover,
 )
import Hydra.Contract.Dummy (dummyMintingScript)
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.HeadLogic qualified as HL
import Hydra.Ledger.Cardano.Evaluate (renderEvaluationReport)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Tx (ConfirmedSnapshot (..), txInToHeadSeed)
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
  PartialFanoutObservation (..),
  observeDecrementTx,
  observeHeadTx,
  observeIncrementTx,
  observeInitTx,
  observePartialFanoutTx,
 )
import Hydra.Tx.Recover (RecoverObservation (..), observeRecoverTx)
import PlutusLedgerApi.V3 qualified as Plutus
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Chain.Direct.State (
  ChainTransition,
  ctxHeadParameters,
  ctxParticipants,
  findFittingPartialChunk,
  genChainStateWithTx,
  genCloseTx,
  genClosedStateForFanout,
  genClosedStateWithAppliedDecommit,
  genClosedStateWithDuplicateTxOuts,
  genClosedStateWithPendingCommit,
  genClosedStateWithUnconfirmedCommit,
  genContestTx,
  genDecrementTx,
  genDepositTx,
  genDepositTxWith,
  genFanoutTx,
  genFinalPartialFanoutTx,
  genHydraContext,
  genIncrementTx,
  genIncrementTxWith,
  genPartialFanoutTx,
  genPartialFanoutTxWithComplexUTxO,
  genRecoverTx,
  maxGenParties,
  pickChainContext,
  unsafeIncrement,
  unsafePartialFanout,
 )
import Test.Hydra.Chain.Direct.State qualified as Transition
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, evaluateTx', maxCpu, maxMem, maxTxSize, pparamsWithMainnetValueLimit)
import Test.Hydra.Tx.Fixture (defaultPParams, slotLength, systemStart, testNetworkId)
import Test.Hydra.Tx.Gen (genConfirmedSnapshot, genOutputFor, genTxOutAdaOnly, genUTxOAdaOnlyOfSize, genUTxOWithUniquePolicyTokensOfSize, propTransactionEvaluates)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  applyMutation,
  modifyInlineDatum,
  replaceHeadId,
  replacePolicyIdWith,
 )
import Test.Hydra.Tx.Utils (splitUTxO)
import Test.QuickCheck (
  Property,
  Testable (property),
  checkCoverage,
  choose,
  classify,
  conjoin,
  counterexample,
  forAll,
  forAllBlind,
  forAllShow,
  forAllShrink,
  ioProperty,
  label,
  oneof,
  tabulate,
  (.&&.),
  (===),
  (==>),
 )
import Test.QuickCheck.Monadic (assert, assertWith, monadicIO, monitor)
import Test.Util (utxoNoThunks)
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

        let tx = initialize cctx defaultPParams seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)
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

    -- The observed deposit UTxO is a 'UTxO' ingress point (decoded from the plutus
    -- datum, not through the forcing JSON/CBOR instances) and flows into 'localUTxO'
    -- and snapshots, where 'forceNewEntries' trusts carried-over entries - so it must
    -- come out of observation fully evaluated. Guards the explicit 'forceUTxO' at the
    -- observation (without it the property holds only incidentally, through the
    -- round-trip guard's re-serialization).
    prop "observed deposit UTxO is fully evaluated" $
      forAllDeposit $ \utxo tx ->
        case observeDepositTx testNetworkId tx of
          Just DepositObservation{deposited} ->
            ioProperty $
              utxoNoThunks deposited >>= \case
                Nothing -> pure $ property True
                Just ti -> pure $ False & counterexample ("Thunk in observed deposit UTxO: " <> show ti)
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

    it "recover requires the deposit to be its transaction's first output" prop_recoverRequiresFirstDepositOutput

  describe "increment" $ do
    propBelowSizeLimit maxTxSize forAllIncrement
    propIsValid forAllIncrement
    it "increment observation observes correct utxo" prop_incrementObservesCorrectUTxO
    it "increment requires the deposit to be its transaction's first output" prop_incrementRequiresFirstDepositOutput

    -- Ties 'rejectOversizedDeposit' (which sizes a dry-run increment with a
    -- fabricated snapshot and dummy signatures) to reality: whenever the check
    -- accepts a deposit, the real increment transaction — built with real
    -- per-party signatures and a real accumulator — must stay within layer 1
    -- limits. This is the regression test for drift between the dry-run
    -- fabrication (and its balancing margin) and actual increment transactions.
    prop "deposits accepted by rejectOversizedDeposit yield increment txs within layer 1 limits" $
      forAllBlind (genIncrementTxWith (genDepositTxWith genMixedDeposit maximumNumberOfParties)) $
        \(ctx, st@OpenState{headId}, txDeposit, _spendableUTxO, txIncrement) ->
          forAllBlind (pickChainContext ctx) $ \cctx ->
            case rejectOversizedDeposit pparamsWithMainnetValueLimit cctx (getKnownUTxO st) headId InitialSnapshot{headId} txDeposit 100 of
              Left DepositTooLarge{} -> label "rejected" $ property True
              Left e -> counterexample ("unexpected error: " <> show e) (property False)
              Right () ->
                label "accepted" $
                  let txSize = fromIntegral $ LBS.length (serialize txIncrement)
                      -- 5000 bytes is the maxValSize of mainnet, matching
                      -- 'pparamsWithMainnetValueLimit'.
                      valueFits v = serializedValueSize pparamsWithMainnetValueLimit v <= 5000
                   in (property (txSize + incrementTxBalancingMargin <= maxTxSize) & counterexample ("Tx size too large: " <> show txSize))
                        .&&. (property (all (valueFits . txOutValue) (txOuts' txIncrement)) & counterexample "Output value size beyond mainnet maxValSize")

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

  describe "partialFanout" $ do
    propBelowSizeLimit maxTxSize forAllPartialFanout
    propIsValid forAllPartialFanout
    prop "validates within 90% of maxTxExecutionUnits for complex UTxO" $
      forAll (genPartialFanoutTxWithComplexUTxO maximumNumberOfParties) $ \(ctx, _, spendableUTxO, tx) ->
        let utxo = spendableUTxO <> getKnownUTxO ctx
            safeUnits =
              ExecutionUnits
                { executionMemory = maxMem * 9 `div` 10
                , executionSteps = maxCpu * 9 `div` 10
                }
         in case evaluateTx' safeUnits tx utxo of
              Right report ->
                all isRight (Map.elems report)
                  & counterexample ("Redeemer report:\n  " <> toString (renderEvaluationReport report))
              Left err ->
                property False
                  & counterexample ("Evaluation failed within 90% budget: " <> show err)
    prop "returns StaleChainState when UTxO does not match on-chain accumulator" $
      forAll (genClosedStateForFanout maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, _u0) ->
          partialFanout ctx spendableUTxO seedTxIn 1 mempty mempty deadlineSlotNo
            === Left StaleChainState
    prop "decommit paid out before close: batch tx can be built" $
      forAll (genClosedStateWithAppliedDecommit maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0, decommitUTxO) ->
          partialFanout ctx spendableUTxO seedTxIn 1 (u0 <> decommitUTxO) u0 deadlineSlotNo
            `shouldSatisfy` isRight
    prop "decommit paid out before close: batch tx evaluates on-chain" $
      forAll (genClosedStateWithAppliedDecommit maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0, decommitUTxO) ->
          let evalUTxO = spendableUTxO <> getKnownUTxO ctx
           in case partialFanout ctx spendableUTxO seedTxIn 1 (u0 <> decommitUTxO) u0 deadlineSlotNo of
                Left err -> counterexample ("partialFanout build failed: " <> show err) False
                Right tx -> propTransactionEvaluates (tx, evalUTxO)
    prop "pending deposit not confirmed on-chain: batch tx can be built" $
      forAll (genClosedStateWithUnconfirmedCommit maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0, commitUTxO) ->
          partialFanout ctx spendableUTxO seedTxIn 1 (u0 <> commitUTxO) u0 deadlineSlotNo
            `shouldSatisfy` isRight

  describe "finalPartialFanout" $ do
    propBelowSizeLimit maxTxSize forAllFinalPartialFanout
    propIsValid forAllFinalPartialFanout
    prop "returns StaleChainState when UTxO does not match on-chain accumulator" $
      forAll (genClosedStateForFanout maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0) ->
          let fanoutProgressUTxO = utxoFromTx $ unsafePartialFanout ctx spendableUTxO seedTxIn 1 u0 deadlineSlotNo
           in case finalPartialFanout ctx fanoutProgressUTxO seedTxIn mempty mempty deadlineSlotNo of
                Left StaleChainState -> property True
                other -> counterexample ("expected Left StaleChainState, got: " <> either show (const "Right <Tx>") other) False
    prop "deposit confirmed on-chain before close: final batch distributes it" $
      forAll (genClosedStateWithPendingCommit maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0, commitUTxO) ->
          let fullUTxO = u0 <> commitUTxO
              evalUTxO = spendableUTxO <> getKnownUTxO ctx
              (_, partialTx) = findFittingPartialChunk evalUTxO ctx spendableUTxO seedTxIn fullUTxO deadlineSlotNo
              fanoutProgressUTxO = utxoFromTx partialTx
           in finalPartialFanout ctx fanoutProgressUTxO seedTxIn commitUTxO mempty deadlineSlotNo
                `shouldSatisfy` isRight
    prop "decommit paid out before close: final batch succeeds after initial batch" $
      forAll (genClosedStateWithAppliedDecommit maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0, decommitUTxO) ->
          case partialFanout ctx spendableUTxO seedTxIn 1 (u0 <> decommitUTxO) u0 deadlineSlotNo of
            Left err -> counterexample ("partialFanout failed: " <> show err) False
            Right partialTx ->
              let fanoutProgressUTxO = utxoFromTx partialTx
                  remaining = UTxO.fromList (drop 1 (UTxO.toList u0))
               in case finalPartialFanout ctx fanoutProgressUTxO seedTxIn remaining decommitUTxO deadlineSlotNo of
                    Left err -> counterexample ("finalPartialFanout failed: " <> show err) False
                    Right _ -> property True
    prop "pending deposit not confirmed on-chain: final batch succeeds after initial batch" $
      forAll (genClosedStateWithUnconfirmedCommit maximumNumberOfParties) $
        \(ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0, commitUTxO) ->
          case partialFanout ctx spendableUTxO seedTxIn 1 (u0 <> commitUTxO) u0 deadlineSlotNo of
            Left err -> counterexample ("partialFanout failed: " <> show err) False
            Right partialTx ->
              let fanoutProgressUTxO = utxoFromTx partialTx
                  remaining = UTxO.fromList (drop 1 (UTxO.toList u0))
               in case finalPartialFanout ctx fanoutProgressUTxO seedTxIn remaining commitUTxO deadlineSlotNo of
                    Left err -> counterexample ("finalPartialFanout failed: " <> show err) False
                    Right _ -> property True
    prop "succeeds when snapshot UTxO has duplicate TxOut values" $
      forAll (genClosedStateWithDuplicateTxOuts maximumNumberOfParties) $
        \(_hctx, ctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0WithDups, chunkSize, _confirmed) ->
          let partialTx = unsafePartialFanout ctx spendableUTxO seedTxIn chunkSize u0WithDups deadlineSlotNo
              fanoutProgressUTxO = utxoFromTx partialTx
              remaining = UTxO.fromList (drop chunkSize (UTxO.toList u0WithDups))
           in finalPartialFanout ctx fanoutProgressUTxO seedTxIn remaining mempty deadlineSlotNo
                `shouldSatisfy` isRight
    prop "HeadLogic computes non-empty remaining UTxO when snapshot contains duplicate TxOut values" $
      forAllBlind (genClosedStateWithDuplicateTxOuts maximumNumberOfParties) $
        \(hctx, cctx, stClosed, spendableUTxO, deadlineSlotNo, u0WithDups, chunkSize, confirmed) ->
          let partialTx = unsafePartialFanout cctx spendableUTxO stClosed.seedTxIn chunkSize u0WithDups deadlineSlotNo
              evalUTxO = spendableUTxO <> getKnownUTxO cctx
           in case observePartialFanoutTx evalUTxO partialTx of
                Nothing -> counterexample "observePartialFanoutTx returned Nothing" False
                Just PartialFanoutObservation{distributedOutputs} ->
                  let hlClosedState =
                        HL.ClosedState
                          { parameters = ctxHeadParameters hctx
                          , confirmedSnapshot = confirmed
                          , contestationDeadline = stClosed.contestationDeadline
                          , readyToFanoutSent = False
                          , chainState = initialChainState
                          , headId = stClosed.headId
                          , headSeed = txInToHeadSeed stClosed.seedTxIn
                          , version = 0
                          }
                      outcome = HL.onClosedChainPartialFanoutTx hlClosedState initialChainState distributedOutputs
                      expectedRemaining = UTxO.fromList . drop chunkSize . UTxO.toList $ u0WithDups
                      -- A node observing a partial fanout it didn't initiate is a
                      -- passive observer: it records the remaining set (by content)
                      -- but does not post the next fanout.
                      remainingUTxOs =
                        [ remainingOutputs
                        | HL.HeadPartialFannedOut{remainingOutputs} <-
                            case outcome of
                              HL.Continue{stateChanges} -> stateChanges
                              HL.Wait{stateChanges} -> stateChanges
                              _ -> []
                        ]
                   in counterexample
                        ("Expected HeadPartialFannedOut{remainingOutputs = " <> show expectedRemaining <> "}")
                        (remainingUTxOs === [expectedRemaining])

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
            Deposit DepositObservation{headId} -> transition === Transition.Deposit .&&. Just headId === expectedHeadId
            Recover RecoverObservation{headId} -> transition === Transition.Recover .&&. Just headId === expectedHeadId
            Increment IncrementObservation{headId} -> transition === Transition.Increment .&&. Just headId === expectedHeadId
            Decrement DecrementObservation{headId} -> transition === Transition.Decrement .&&. Just headId === expectedHeadId
            Close CloseObservation{headId} -> transition === Transition.Close .&&. Just headId === expectedHeadId
            Contest ContestObservation{headId} -> transition === Transition.Contest .&&. Just headId === expectedHeadId
            Fanout FanoutObservation{headId} -> transition === Transition.Fanout .&&. Just headId === expectedHeadId
            FinalPartialFanout FanoutObservation{headId} -> transition === Transition.FinalPartialFanout .&&. Just headId === expectedHeadId
            PartialFanout PartialFanoutObservation{headId} -> transition === Transition.PartialFanout .&&. Just headId === expectedHeadId
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
      snapshot <- pickBlind $ genConfirmedSnapshot headId version 1 openUTxO (Just utxo) (Just depositedTxId) Nothing (ctxHydraSigningKeys ctx)
      let txIncrement =
            unsafeIncrement
              cctx
              utxo
              (txInToHeadSeed seedTxIn, headId)
              (ctxHeadParameters ctx)
              snapshot
              slotNo
      case observeIncrementTx networkId utxo txIncrement of
        Nothing -> assertWith False "Increment not observed"
        Just IncrementObservation{depositTxId} -> do
          let txDepositId = getTxId (getTxBody txDeposit)
          monitor (counterexample $ "Expected TxId:" <> show depositTxId <> " Actual TxId:" <> show txDepositId)
          assert (depositTxId == txDepositId)

-- | 'Hydra.Contract.Head.checkIncrement' requires the claimed deposit to be its
-- transaction's first output, so 'increment' resolves exactly that output rather
-- than any output of the deposit transaction. Matching by transaction id alone
-- would build a transaction that cannot validate.
prop_incrementRequiresFirstDepositOutput :: Property
prop_incrementRequiresFirstDepositOutput = monadicIO $ do
  (ctx, st@OpenState{headId, seedTxIn}, _, txDeposit) <- pickBlind $ genDepositTx maxGenParties
  let networkId = ctxNetworkId ctx
  case observeDepositTx networkId txDeposit of
    Nothing -> assertWith False "Deposit not observed"
    Just DepositObservation{depositTxId, deposited, deadline} -> do
      cctx <- pickBlind $ pickChainContext ctx
      let openUTxO = getKnownUTxO st
          slotNo = slotNoFromUTCTime systemStart slotLength deadline
      case UTxO.findWithKey (\txin _ -> txin == TxIn depositTxId (TxIx 0)) (utxoFromTx txDeposit) of
        Nothing -> assertWith False "Deposit is not the first output of its transaction"
        Just (_, depositOut) -> do
          -- Same deposit output, same transaction id, moved off index 0.
          let utxo = openUTxO <> UTxO.singleton (TxIn depositTxId (TxIx 1)) depositOut
          snapshot <-
            pickBlind $
              genConfirmedSnapshot headId 0 1 openUTxO (Just deposited) (Just depositTxId) Nothing (ctxHydraSigningKeys ctx)
          case increment cctx utxo (txInToHeadSeed seedTxIn, headId) (ctxHeadParameters ctx) snapshot slotNo of
            Left CannotFindDepositOutputInIncrement{} -> pure ()
            Left err -> assertWith False $ "Expected CannotFindDepositOutputInIncrement, got: " <> show err
            Right _ -> assertWith False "Expected increment to fail, but it built a transaction"

-- | 'Hydra.Tx.Recover.recoverTx' spends @TxIn depositTxId (TxIx 0)@, so 'recover'
-- resolves exactly that output. Matching by transaction id alone would read one
-- output's datum and then build a transaction spending a different one.
prop_recoverRequiresFirstDepositOutput :: Property
prop_recoverRequiresFirstDepositOutput = monadicIO $ do
  (ctx, OpenState{headId}, _, txDeposit) <- pickBlind $ genDepositTx maxGenParties
  let networkId = ctxNetworkId ctx
  case observeDepositTx networkId txDeposit of
    Nothing -> assertWith False "Deposit not observed"
    Just DepositObservation{depositTxId, deadline} -> do
      cctx <- pickBlind $ pickChainContext ctx
      let slotNo = slotNoFromUTCTime systemStart slotLength deadline
      case UTxO.findWithKey (\txin _ -> txin == TxIn depositTxId (TxIx 0)) (utxoFromTx txDeposit) of
        Nothing -> assertWith False "Deposit is not the first output of its transaction"
        Just (_, depositOut) -> do
          -- Same deposit output, same transaction id, moved off index 0.
          let utxo = UTxO.singleton (TxIn depositTxId (TxIx 1)) depositOut
          case recover cctx headId depositTxId utxo slotNo of
            Left CannotFindDepositOutputToRecover{} -> pure ()
            Left err -> assertWith False $ "Expected CannotFindDepositOutputToRecover, got: " <> show err
            Right _ -> assertWith False "Expected recover to fail, but it built a transaction"

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
        let tx = initialize cctx defaultPParams seedIn (ctxParticipants ctx) (ctxHeadParameters ctx)
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

-- | Deposits ranging from trivially fitting (a few ada-only outputs) to
-- clearly oversized (>100 distinct-policy tokens whose merged value exceeds
-- mainnet's 5000 byte maxValSize), so 'rejectOversizedDeposit' exercises both
-- verdicts.
genMixedDeposit :: Gen UTxO
genMixedDeposit =
  oneof
    [ genUTxOAdaOnlyOfSize =<< choose (1, 10)
    , genUTxOWithUniquePolicyTokensOfSize =<< choose (1, 140)
    ]

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
  forAll (genCloseTx maximumNumberOfParties) $ \(ctx, _, utxo', tx, sn) ->
    let utxo = utxo' <> getKnownUTxO ctx
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
  forAll (genFanoutTx maximumNumberOfParties) $ \(ctx, _stClosed, spendableUTxO, tx) ->
    let utxo = spendableUTxO <> getKnownUTxO ctx
     in action utxo tx
          & label ("Fanout size: " <> prettyLength (countAssets $ txOuts' tx))
 where
  maxSupported :: Int
  maxSupported = 44

  countAssets :: [TxOut ctx] -> Int
  countAssets = getSum . foldMap (Sum . length . IsList.toList . txOutValue)

  prettyLength :: Int -> String
  prettyLength len
    | len > maxSupported = "> " <> show maxSupported <> " ???"
    | len >= 40 = "40-" <> show maxSupported
    | len >= 10 = "10-40"
    | len >= 1 = "1-10"
    | otherwise = "0"

forAllPartialFanout ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property

-- | Use spendableUTxO (not 'getKnownUTxO stClosed'): the generator adds the
-- full UTxO value to the head output so 'partialFanoutTx' can subtract
-- distributed values without going negative. The evaluation UTxO must match.
forAllPartialFanout action =
  forAll (genPartialFanoutTx maximumNumberOfParties) $ \(ctx, _, spendableUTxO, tx) ->
    let utxo = spendableUTxO <> getKnownUTxO ctx
     in action utxo tx

-- | The spendable UTxO for the final partial fanout is the FanoutProgress head
-- output produced by the preceding partial fanout step, so we use the 3rd
-- element from the generator rather than 'getKnownUTxO stClosed'.
forAllFinalPartialFanout ::
  Testable property =>
  (UTxO -> Tx -> property) ->
  Property
forAllFinalPartialFanout action =
  forAll (genFinalPartialFanoutTx maximumNumberOfParties) $ \(ctx, _, fanoutProgressUTxO, tx) ->
    let utxo = fanoutProgressUTxO <> getKnownUTxO ctx
     in action utxo tx
