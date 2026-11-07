{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Replay the honest decommit schedule with frozen or corrected rules.
-- Every emitted network message is delivered; no retry or fault policy is added.
module Test.Hydra.HeadLogic.DecommitDependency (demonstration, historicalDemonstration) where

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value, object, (.=))
import Data.List ((!!))
import Hydra.HeadLogic (
  CoordinatedHeadState (
    confirmedSnapshot,
    decommitTx,
    localUTxO,
    seenSnapshot
  ),
  Effect (NetworkEffect),
  Input (NetworkInput),
  LogicError (RequireFailed),
  OpenState (coordinatedHeadState),
  Outcome (Continue, Error, Wait, stateChanges),
  RequirementFailure (SnapshotDoesNotApply),
  SeenSnapshot (RequestedSnapshot),
  StateChanged,
  aggregateNodeState,
  aggregateState,
  onOpenNetworkAckSnProgram,
  onOpenNetworkReqSnProgram,
  onOpenNetworkReqTxProgram,
  ruleEnvironment,
  update,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Rules.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state, trace)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.CoordinatedAudit qualified as Audit
import Test.Hydra.HeadLogic.LegacyIncremental qualified as LegacyIncremental
import Test.Hydra.HeadLogic.LegacyPrograms qualified as Legacy
import Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)

data Implementation = Historical | Corrected
  deriving stock (Eq)

data Delivery = Delivery Int Int (Message SimpleTx)
data Run = Run [NodeState SimpleTx] [Delivery] [Value] [(Text, [TraceStep])]

demonstration :: Either Text (Value, [(Text, [TraceStep])])
demonstration = scenario Corrected

historicalDemonstration :: Either Text (Value, [(Text, [TraceStep])])
historicalDemonstration = scenario Historical

scenario :: Implementation -> Either Text (Value, [(Text, [TraceStep])])
scenario implementation = do
  withParent <- broadcast 0 (ReqTx Scenario.transactionOne) initial
  withChild <- broadcast 0 (ReqTx Scenario.transactionTwo) withParent
  reserved@(Run reservedNodes _ _ _) <- broadcast 0 (ReqDec decommit) withChild
  ensure "ReqDec removes the pending decommit inputs" $ all ((== mempty) . (.localUTxO) . coordinated) reservedNodes
  ensure "all nodes retain the decommit" $ all ((== Just decommit) . (.decommitTx) . coordinated) reservedNodes
  rebuilt@(Run rebuiltNodes _ _ _) <- drainSnapshots reserved
  ensure "the rebase reservation matches the implementation" $
    all
      ( \node ->
          (coordinated node).localUTxO == (if implementation == Historical then utxoRef 3 else mempty)
            && (coordinated node).decommitTx == Just decommit
      )
      rebuiltNodes
  ensure "the independent reservation audit distinguishes the fix" $
    all ((== (implementation == Corrected)) . Audit.reservationHeld . coordinated) rebuiltNodes
  confirmed@(Run confirmedNodes pending _ _) <- drainAcknowledgements rebuilt
  ensure "snapshot 1 confirms at every node" $ all ((== 1) . (.number) . getSnapshot . (.confirmedSnapshot) . coordinated) confirmedNodes
  let requestedDecommit = if implementation == Historical then Just decommit else Nothing
  ensure "snapshot 2 defers the decommit only in the corrected rule" $
    length pending == 3 && all (\(Delivery sender _ msg) -> sender == 1 && msg == ReqSn 0 2 [2] requestedDecommit Nothing) pending
  Run final remaining steps trace <- drainAll 40 confirmed
  ensure "every emitted network message has been delivered" $ null remaining
  case implementation of
    Historical -> do
      ensure "the frozen head stops at snapshot 1" $ all ((== 1) . (.number) . getSnapshot . (.confirmedSnapshot) . coordinated) final
      ensure "the frozen leader still waits for snapshot 2" $ (coordinated $ final !! 1).seenSnapshot == RequestedSnapshot 1 2
    Corrected -> do
      ensure "the corrected head confirms the decommit in snapshot 3" $ all (\node -> let snapshot = getSnapshot (coordinated node).confirmedSnapshot in snapshot.number == 3 && snapshot.utxo == mempty && snapshot.utxoToDecommit == Just (utxoRef 4)) final
  pure (object ["schemaVersion" .= (2 :: Int), "historical" .= (implementation == Historical), "steps" .= steps, "finalNodes" .= final], trace)
 where
  initial = Run (replicate 3 Scenario.initialNode) [] [] []
  decommit = SimpleTx 3 (utxoRef 3) (utxoRef 4)
  coordinated node = (Scenario.openState node).coordinatedHeadState
  ensure :: Text -> Bool -> Either Text ()
  ensure reason condition = unless condition $ Left reason
  broadcast sender msg run = foldlM (\current recipient -> deliver implementation (Delivery sender recipient msg) current) run [0, 1, 2]
  drainSnapshots run@(Run _ queued _ _) = case queued of
    Delivery _ _ ReqSn{} : _ -> drainOne run >>= drainSnapshots
    _ -> Right run
  drainAcknowledgements run@(Run _ queued _ _) = case queued of
    Delivery _ _ AckSn{} : _ -> drainOne run >>= drainAcknowledgements
    _ -> Right run
  drainAll :: Int -> Run -> Either Text Run
  drainAll _ run@(Run _ [] _ _) = Right run
  drainAll 0 _ = Left "decommit demonstration exceeds its delivery bound"
  drainAll n run = drainOne run >>= drainAll (n - 1)
  drainOne (Run nodes (d : ds) steps trace) = deliver implementation d (Run nodes ds steps trace)
  drainOne _ = Left "empty demonstration queue"

-- | Deliver emitted messages in order. The historical run uses frozen input
-- rules and aggregation; the corrected run uses production execution and replay.
deliver :: Implementation -> Delivery -> Run -> Either Text Run
deliver implementation (Delivery sender recipient msg) (Run nodes queued steps traces) = do
  let node = nodes !! recipient
      env = Scenario.environment recipient
      os = Scenario.openState node
      party = Scenario.scenarioParties !! sender
      time = node.chainPointTime
      input = NetworkInput 2 $ ReceivedMessage party msg
      production = update env simpleLedger initialChainTime node input
      config = Interpreter.ledgerConfig simpleLedger
      (execution, expected) = case msg of
        ReqTx tx ->
          ( Interpreter.Execution config $ (if implementation == Historical then Legacy.onOpenNetworkReqTxProgram else onOpenNetworkReqTxProgram) (ruleEnvironment env) time os 2 mempty tx
          , Interpreter.runProgram simpleLedger $ Legacy.onOpenNetworkReqTxProgram (ruleEnvironment env) time os 2 mempty tx
          )
        ReqDec tx ->
          ( Interpreter.Execution config $ Workflow.run $ ReqDec.machine (ruleEnvironment env) 2 time mempty os tx
          , LegacyIncremental.onOpenNetworkReqDec env simpleLedger 2 time mempty os tx
          )
        ReqSn sv sn ids dec dep ->
          ( Interpreter.Execution (Interpreter.snapshotConfig env simpleLedger) $ (if implementation == Historical then Legacy.onOpenNetworkReqSnProgram else onOpenNetworkReqSnProgram) (ruleEnvironment env) mempty time os 2 party sv sn ids dec dep
          , Interpreter.runProgramWith (Interpreter.snapshotConfig env simpleLedger) $ Legacy.onOpenNetworkReqSnProgram (ruleEnvironment env) mempty time os 2 party sv sn ids dec dep
          )
        AckSn signature sn ->
          ( Interpreter.Execution Interpreter.basicConfig $ (if implementation == Historical then Legacy.onOpenNetworkAckSnProgram else onOpenNetworkAckSnProgram) (ruleEnvironment env) time mempty os party signature sn
          , Interpreter.runProgramWith Interpreter.basicConfig $ Legacy.onOpenNetworkAckSnProgram (ruleEnvironment env) time mempty os party signature sn
          )
      (tracedOutcome, observations) = case execution of Interpreter.Execution interpreter program -> Interpreter.traceProgramWith interpreter program
      outcome = if implementation == Historical then expected else production
      retained = case outcome of
        Continue{stateChanges} -> stateChanges
        Wait{stateChanges} -> stateChanges
        Error{} -> []
      after = if implementation == Historical then foldl' legacyAggregateNodeState node retained else aggregateState node outcome
  unless (outcome == tracedOutcome) $ Left "execution and instruction trace disagree"
  unless (decodeFull' (serialize' after) == Right after) $ Left "checkpoint round trip changes state"
  restored <- first show $ decodeFull' (serialize' node)
  events <- first show $ decodeFull' @[StateChanged SimpleTx] (serialize' retained)
  let replay = if implementation == Historical then legacyAggregateNodeState else aggregateNodeState
  unless (foldl' replay restored events == after) $ Left "serialized event replay changes state"
  case (implementation, msg) of
    (Historical, ReqSn _ 2 _ _ _) -> unless (case outcome of Error (RequireFailed (SnapshotDoesNotApply 2 3 _)) -> True; _ -> False) $ Left "frozen snapshot 2 does not reject its dependent decommit"
    _ -> case outcome of Continue{} -> pure (); _ -> Left "unexpected terminal outcome in honest prefix"
  let emitted = case outcome of
        Continue _ effects -> [Delivery recipient target message | NetworkEffect message <- effects, target <- [0, 1, 2]]
        _ -> []
      entry = object ["sender" .= sender, "recipient" .= recipient, "input" .= input, "before" .= node, "outcome" .= outcome, "after" .= after, "reservationHeld" .= Audit.reservationHeld (Scenario.openState after).coordinatedHeadState, "decommitRequestsReady" .= all (Audit.decommitReady (Scenario.openState after).coordinatedHeadState) [tx | Delivery _ _ (ReqSn _ _ _ (Just tx) _) <- emitted]]
      title =
        "Party " <> show recipient <> ": " <> case msg of
          ReqTx tx -> "ReqTx " <> show tx.txSimpleId
          ReqDec{} -> "ReqDec 3 (spends output of tx2)"
          ReqSn _ sn _ _ _ -> "ReqSn " <> show sn
          AckSn _ sn -> "AckSn " <> show sn
  when (implementation == Corrected) $ do
    unless (Audit.reservationHeld (Scenario.openState after).coordinatedHeadState) $ Left "corrected run loses a reservation"
    unless (all (Audit.decommitReady (Scenario.openState after).coordinatedHeadState) [tx | Delivery _ _ (ReqSn _ _ _ (Just tx) _) <- emitted]) $ Left "corrected run requests an ineligible decommit"
  pure $ Run (take recipient nodes <> [after] <> drop (recipient + 1) nodes) (queued <> emitted) (steps <> [entry]) (traces <> [(title, observations)])
