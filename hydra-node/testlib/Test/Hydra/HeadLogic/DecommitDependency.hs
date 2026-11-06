{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | A fixed honest delivery order exposing an existing decommit dependency bug.
-- Every emitted network message is delivered; no retry or fault policy is added.
module Test.Hydra.HeadLogic.DecommitDependency (demonstration) where

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

data Delivery = Delivery Int Int (Message SimpleTx)
data Run = Run [NodeState SimpleTx] [Delivery] [Value] [(Text, [TraceStep])]

demonstration :: Either Text (Value, [(Text, [TraceStep])])
demonstration = do
  withParent <- broadcast 0 (ReqTx Scenario.transactionOne) initial
  withChild <- broadcast 0 (ReqTx Scenario.transactionTwo) withParent
  reserved@(Run reservedNodes _ _ _) <- broadcast 0 (ReqDec decommit) withChild
  ensure "ReqDec removes the pending decommit inputs" $ all ((== mempty) . (.localUTxO) . coordinated) reservedNodes
  ensure "all nodes retain the decommit" $ all ((== Just decommit) . (.decommitTx) . coordinated) reservedNodes
  -- Deliver the first snapshot before its acknowledgements. It confirms tx1
  -- only; tx2 and the decommit that spends tx2's output remain pending.
  rebuilt@(Run rebuiltNodes _ _ _) <- drainSnapshots reserved
  ensure "pruning restores the reserved output while retaining the decommit" $
    all (\node -> (coordinated node).localUTxO == utxoRef 3 && (coordinated node).decommitTx == Just decommit) rebuiltNodes
  ensure "owner audit detects the lost reservation" $ not (any (Audit.reservationHeld . coordinated) rebuiltNodes)
  confirmed@(Run confirmedNodes pending _ _) <- drainAcknowledgements rebuilt
  ensure "snapshot 1 confirms at every node" $ all ((== 1) . (.number) . getSnapshot . (.confirmedSnapshot) . coordinated) confirmedNodes
  ensure "the next leader requests tx2 and its dependent decommit together" $
    length pending == 3 && all (\(Delivery sender _ msg) -> sender == 1 && msg == ReqSn 0 2 [2] (Just decommit) Nothing) pending
  ensure "owner audit detects the unconfirmed decommit dependency" $ not (any ((`Audit.decommitReady` decommit) . coordinated) confirmedNodes)
  Run final remaining steps trace <- drainSnapshots confirmed
  ensure "every emitted network message has been delivered" $ null remaining
  ensure "no node confirms snapshot 2" $ all ((== 1) . (.number) . getSnapshot . (.confirmedSnapshot) . coordinated) final
  ensure "the leader still waits for snapshot 2" $ (coordinated $ final !! 1).seenSnapshot == RequestedSnapshot 1 2
  pure (object ["schemaVersion" .= (1 :: Int), "steps" .= steps, "finalNodes" .= final, "finding" .= ("decommit of an unconfirmed output strands snapshot 2" :: Text)], trace)
 where
  initial = Run (replicate 3 Scenario.initialNode) [] [] []
  decommit = SimpleTx 3 (utxoRef 3) (utxoRef 4)
  coordinated node = (Scenario.openState node).coordinatedHeadState
  ensure :: Text -> Bool -> Either Text ()
  ensure reason condition = unless condition $ Left reason
  broadcast sender msg run = foldlM (\current recipient -> deliver (Delivery sender recipient msg) current) run [0, 1, 2]
  drainSnapshots run@(Run _ queued _ _) = case queued of
    Delivery _ _ ReqSn{} : _ -> drainOne run >>= drainSnapshots
    _ -> Right run
  drainAcknowledgements run@(Run _ queued _ _) = case queued of
    Delivery _ _ AckSn{} : _ -> drainOne run >>= drainAcknowledgements
    _ -> Right run
  drainOne (Run nodes (d : ds) steps trace) = deliver d (Run nodes ds steps trace)
  drainOne _ = Left "empty demonstration queue"

-- | Compare production update, its exposed program, and the frozen rule on each
-- delivered input. Exact replay of effects determines the following deliveries.
deliver :: Delivery -> Run -> Either Text Run
deliver (Delivery sender recipient msg) (Run nodes queued steps traces) = do
  let node = nodes !! recipient
      env = Scenario.environment recipient
      os = Scenario.openState node
      party = Scenario.scenarioParties !! sender
      time = node.chainPointTime
      input = NetworkInput 2 $ ReceivedMessage party msg
      outcome = update env simpleLedger initialChainTime node input
      after = aggregateState node outcome
      config = Interpreter.ledgerConfig simpleLedger
      (execution, expected) = case msg of
        ReqTx tx ->
          ( Interpreter.Execution config $ onOpenNetworkReqTxProgram (ruleEnvironment env) time os 2 mempty tx
          , Interpreter.runProgram simpleLedger $ Legacy.onOpenNetworkReqTxProgram (ruleEnvironment env) time os 2 mempty tx
          )
        ReqDec tx ->
          ( Interpreter.Execution config $ Workflow.run $ ReqDec.machine (ruleEnvironment env) 2 time mempty os tx
          , LegacyIncremental.onOpenNetworkReqDec env simpleLedger 2 time mempty os tx
          )
        ReqSn sv sn ids dec dep ->
          ( Interpreter.Execution (Interpreter.snapshotConfig env simpleLedger) $ onOpenNetworkReqSnProgram (ruleEnvironment env) mempty time os 2 party sv sn ids dec dep
          , Interpreter.runProgramWith (Interpreter.snapshotConfig env simpleLedger) $ Legacy.onOpenNetworkReqSnProgram (ruleEnvironment env) mempty time os 2 party sv sn ids dec dep
          )
        AckSn signature sn ->
          ( Interpreter.Execution Interpreter.basicConfig $ onOpenNetworkAckSnProgram (ruleEnvironment env) time mempty os party signature sn
          , Interpreter.runProgramWith Interpreter.basicConfig $ Legacy.onOpenNetworkAckSnProgram (ruleEnvironment env) time mempty os party signature sn
          )
      (tracedOutcome, observations) = case execution of Interpreter.Execution interpreter program -> Interpreter.traceProgramWith interpreter program
  unless (outcome == expected && outcome == tracedOutcome) $ Left "production, program and frozen handler disagree"
  let retained = case expected of
        Continue{stateChanges} -> stateChanges
        Wait{stateChanges} -> stateChanges
        Error{} -> []
  unless (after == foldl' legacyAggregateNodeState node retained) $ Left "production and frozen event reducers disagree"
  case msg of
    ReqSn _ 2 _ _ _ -> unless (case outcome of Error (RequireFailed (SnapshotDoesNotApply 2 3 _)) -> True; _ -> False) $ Left "snapshot 2 does not reject its dependent decommit"
    _ -> case outcome of Continue{} -> pure (); _ -> Left "unexpected terminal outcome in honest prefix"
  let emitted = case outcome of
        Continue _ effects -> [Delivery recipient target message | NetworkEffect message <- effects, target <- [0, 1, 2]]
        _ -> []
      entry = object ["sender" .= sender, "recipient" .= recipient, "input" .= input, "before" .= node, "outcome" .= outcome, "after" .= after, "reservationHeld" .= Audit.reservationHeld (Scenario.openState after).coordinatedHeadState, "decommitRequestsReady" .= all (Audit.decommitReady os.coordinatedHeadState) [tx | Delivery _ _ (ReqSn _ _ _ (Just tx) _) <- emitted]]
      title =
        "Party " <> show recipient <> ": " <> case msg of
          ReqTx tx -> "ReqTx " <> show tx.txSimpleId
          ReqDec{} -> "ReqDec 3 (spends output of tx2)"
          ReqSn _ sn _ _ _ -> "ReqSn " <> show sn
          AckSn _ sn -> "AckSn " <> show sn
  pure $ Run (take recipient nodes <> [after] <> drop (recipient + 1) nodes) (queued <> emitted) (steps <> [entry]) (traces <> [(title, observations)])
