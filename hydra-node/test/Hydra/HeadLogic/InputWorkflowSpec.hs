{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.InputWorkflowSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.Chain (ChainEvent (..))
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx (..), SimpleTxOut (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.HeadLogic (
  CoordinatedHeadState (decommitTx, seenSnapshot),
  Effect (ClientEffect, NetworkEffect),
  FanoutMode (AutoDrain, AwaitingSelection, DistributingSelection),
  FinalizedSnapshot (FinalizedSnapshot),
  HeadState (FanoutProgress),
  Input (ChainInput, ClientInput, NetworkInput),
  LogicError (SideLoadSnapshotFailed),
  OpenState (coordinatedHeadState, headId, headSeed, parameters),
  Outcome (Continue, Error, Wait),
  PartialFanoutState (mode, remainingOutputs),
  StateChanged (
    CommitFinalized,
    DecommitFinalized,
    DecommitInvalid,
    HeadPartialFannedOut,
    SnapshotConfirmed,
    decommitInvalidReason,
    mode
  ),
  WaitReason (WaitOnNodeInSync),
  maybeRepostDecrementTx,
  mkSeenSnapshot,
  onOpenChainDecrementTx,
  onOpenChainIncrementTx,
  onOpenClientDecommit,
  onPartialFanoutChainPartialFanoutTx,
  update,
 )
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Prelude
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature, aggregate, aggregateInOrder, sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.Hydra.HeadLogic.InputFixtures qualified as Inputs
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Prelude (
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
  shouldSatisfy,
 )
import Test.Hydra.Tx.Fixture (alice, aliceSk, bobSk, carolSk)

spec :: Spec
spec = describe "bounded Agda input decisions" $ do
  it "checks size before sync admission, and admits chain inputs while catching up" $
    forM_ [(kind, safe, synced) | kind <- [0, 1, 2], safe <- [False, True], synced <- [False, True]] $ \(kind, safe, synced) -> do
      let node = if synced then initial else NodeCatchingUp initial.headState initial.deposits initial.chainPointTime
          input = case kind of
            0 -> ClientInput $ if safe then Close else SideLoadSnapshot $ ConfirmedSnapshot oversized (aggregate [])
            1 -> NetworkInput 1 $ ReceivedMessage alice $ ReqTx Scenario.transactionOne
            _ -> ChainInput $ Tick initialChainTime 0
          result = update env simpleLedger initialChainTime node input
          tag = case result of
            Error SideLoadSnapshotFailed{} -> 1
            Continue [] [ClientEffect ServerOutput.RejectedInputBecauseUnsynced{}] -> 2
            Wait WaitOnNodeInSync{} [] -> 3
            _ -> 0
      tag `shouldBe` Agda.inputAdmissionRef kind safe synced

  it "preserves client decommit failure priority" $
    forM_ [(existing, applies, outputs) | existing <- [False, True], applies <- [False, True], outputs <- [False, True]] $ \(existing, applies, outputs) -> do
      let tx = if outputs then Scenario.transactionOne else Scenario.transactionOne{txOutputs = mempty}
          st :: CoordinatedHeadState SimpleTx
          st = os.coordinatedHeadState{decommitTx = if existing then Just Scenario.transactionTwo else Nothing}
          ledger = Ledger $ \_ utxo _ -> if applies then Right utxo else Left (tx, ValidationError "controlled ledger failure")
          result = onOpenClientDecommit os.headId (testInterpreterConfig ledger) 0 st tx
          tag = case result of
            Continue [DecommitInvalid{decommitInvalidReason = DecommitAlreadyInFlight{}}] [] -> 0
            Continue [DecommitInvalid{decommitInvalidReason = DecommitTxInvalid{validationError = ValidationError "controlled ledger failure"}}] [] -> 1
            Continue [DecommitInvalid{decommitInvalidReason = DecommitTxInvalid{}}] [] -> 2
            Continue [] [NetworkEffect ReqDec{}] -> 3
            _ -> -1
      tag `shouldBe` Agda.clientDecommitRef existing applies outputs

  it "adopts exactly the matching signed settlement and always records finalization first" $
    forM_ [(increment, observed, newVersion, matches, valid) | increment <- [False, True], observed <- [0, 1, 2], newVersion <- [0, 1, 2], matches <- [False, True], valid <- [False, True]] $ \(increment, observed, newVersion, matches, valid) -> do
      let utxo = utxoRef 1
          commit = if increment && matches then Just $ utxoRef 42 else Nothing
          decommit = if not increment && matches then Just $ utxoRef 2 else Nothing
          (accumulator, appliedAccumulator) = Accumulator.buildFromSnapshotUTxOs @SimpleTx utxo commit decommit
          snapshot = Scenario.initialSnapshot{number = 1, utxoToCommit = commit, utxoToDecommit = decommit, depositTxId = if isJust commit then Just 42 else Nothing, accumulator, appliedAccumulator}
          st = os{coordinatedHeadState = os.coordinatedHeadState{seenSnapshot = mkSeenSnapshot snapshot mempty}}
          signatures = if valid then signaturesFor snapshot else aggregate []
          outcome =
            if increment
              then onOpenChainIncrementTx (testInterpreterConfig simpleLedger) env mempty st 1 newVersion 42 observed signatures
              else onOpenChainDecrementTx (testInterpreterConfig simpleLedger) env mempty st 1 newVersion (utxoRef 2) observed signatures
          shouldAdopt = Agda.adoptSettlementRef (toInteger observed) 1 0 (toInteger newVersion) matches valid
      case outcome of
        Continue events _ -> do
          [() | SnapshotConfirmed{} <- events] `shouldBe` [() | shouldAdopt]
          take 1 events `shouldSatisfy` \case [CommitFinalized{}] -> increment; [DecommitFinalized{}] -> not increment; _ -> False
        _ -> expectationFailure $ show outcome

  it "reposts retained settlement only strictly before its observation slot" $
    forM_ [(pending, rollbackSlot) | pending <- [False, True], rollbackSlot <- [2, 3, 4]] $ \(pending, rollbackSlot) -> do
      let snapshot :: Snapshot SimpleTx
          snapshot = Scenario.initialSnapshot{utxoToDecommit = Just $ utxoRef 2}
          confirmed = ConfirmedSnapshot snapshot (signaturesFor snapshot)
          finalized = Just $ FinalizedSnapshot confirmed 3
          result = maybeRepostDecrementTx (testInterpreterConfig simpleLedger) os.headSeed os.headId os.parameters (if pending then Just Scenario.transactionOne else Nothing) confirmed finalized (fromInteger rollbackSlot)
          posted = case result of Continue [] [_] -> True; _ -> False
      posted `shouldBe` Agda.repostFinalizedRef pending rollbackSlot 3

  it "continues fanout according to the remaining set and driver mode" $
    forM_ [(remainderEmpty, modeTag) | remainderEmpty <- [False, True], modeTag <- [0, 1, 2]] $ \(remainderEmpty, modeTag) -> do
      let modes = [AutoDrain, DistributingSelection $ utxoRef 2, AwaitingSelection]
          drivers = [st | ("fanout-driver-observation", NodeInSync{headState = FanoutProgress st}, _) <- Inputs.examples]
      length drivers `shouldBe` 1
      forM_ drivers $ \driver -> do
        let st :: PartialFanoutState SimpleTx
            st = driver{remainingOutputs = utxoRef 1 <> utxoRef 2 <> utxoRef 3, mode = fromMaybe AwaitingSelection $ listToMaybe $ drop (fromInteger modeTag) modes}
            observed = if remainderEmpty then st.remainingOutputs else utxoRef 1
            result = onPartialFanoutChainPartialFanoutTx (testInterpreterConfig simpleLedger) st 2 observed
            tag = case result of
              Continue _ [] -> 0
              Continue [HeadPartialFannedOut{mode = DistributingSelection{}}] [_] -> 2
              Continue _ [_] -> 1
              _ -> -1
        tag `shouldBe` Agda.fanoutContinuationRef remainderEmpty modeTag
 where
  initial = Scenario.initialNode
  os = Scenario.openState initial
  env = Scenario.environment 0
  -- Bounds checking inspects cardinality without forcing these rebuilt caches.
  oversized :: Snapshot SimpleTx
  oversized = Scenario.initialSnapshot{utxo = large, accumulator = largeAccumulator, appliedAccumulator = largeAccumulator}
  large = Set.fromList $ SimpleTxOut <$> [0 .. fromIntegral Accumulator.maxAccumulatorSize]
  largeAccumulator = Accumulator.buildFromUTxO @SimpleTx large

signaturesFor :: Snapshot SimpleTx -> MultiSignature (Snapshot SimpleTx)
signaturesFor snapshot = aggregateInOrder (Map.fromList [(p, sign sk snapshot) | (p, sk) <- zip Scenario.scenarioParties [aliceSk, bobSk, carolSk]]) Scenario.scenarioParties
