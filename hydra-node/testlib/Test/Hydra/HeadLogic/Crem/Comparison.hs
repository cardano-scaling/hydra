{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Concrete comparisons and development diagrams, separate from the rules.
module Test.Hydra.HeadLogic.Crem.Comparison (Example (..), Reference (..), examples, compareExample, comparePrograms, writeDiagrams) where

import Cardano.Binary (decodeFull', serialize')
import Crem.Render.Render (Mermaid (..), baseMachineAsGraph, renderStateDiagram)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hydra.HeadLogic (
  CoordinatedHeadState (allTxs, localUTxO, seenSnapshot),
  HeadState (Open),
  OpenState (coordinatedHeadState, headId),
  Outcome,
  SeenSnapshot (RequestedSnapshot, SeenSnapshot, snapshot),
  aggregateState,
  mkSeenSnapshot,
  onOpenNetworkReqSn,
  onOpenNetworkReqTx,
  ruleEnvironment,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Rules.AckSn qualified as AckSn
import Hydra.HeadLogic.Rules.Recover qualified as Recover
import Hydra.HeadLogic.Rules.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Rules.ReqSn qualified as ReqSn
import Hydra.HeadLogic.Rules.ReqTx qualified as ReqTx
import Hydra.HeadLogic.Trace (TraceStep (Decided))
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state, trace)
import Hydra.Tx.Crypto (sign)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.LegacyIncremental qualified as LegacyIncremental
import Test.Hydra.HeadLogic.LegacyPrograms qualified as LegacyPrograms
import Test.Hydra.HeadLogic.Mermaid qualified as Mermaid
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)

-- ReqDec and Recover had no instruction trace before this migration.
data Reference tx ledger signing
  = FrozenProgram (Program tx ledger signing ())
  | FrozenOutcome (Outcome tx)

data Example where
  Example :: Text -> NodeState SimpleTx -> Interpreter.InterpreterConfig SimpleTx ledger signing -> Reference SimpleTx ledger signing -> Program SimpleTx ledger signing () -> Text -> Example

examples :: [Example]
examples =
  [ req "req-tx-leader" 0 initial 1 Scenario.transactionOne
  , req "req-tx-follower" 1 initial 1 Scenario.transactionOne
  , req "req-tx-wait" 0 initial 1 Scenario.transactionTwo
  , req "req-tx-timeout" 0 initial 0 Scenario.transactionTwo
  , req "req-tx-duplicate" 0 applied 0 Scenario.transactionOne
  , req "req-tx-output-collision" 0 collision 1 Scenario.transactionOne
  , req "req-tx-in-flight" 0 inFlight 1 Scenario.transactionOne
  , reqSn "req-sn-sign" applied 0 [1] Nothing
  , reqSn "req-sn-version-wait" applied 1 [1] Nothing
  , reqSn "req-sn-missing-transaction" initial 0 [2] Nothing
  , reqSn "req-sn-decommit" knownDecommit 0 [] (Just Scenario.transactionOne)
  , decommit "req-dec-record" Scenario.transactionOne 1
  , decommit "req-dec-wait" Scenario.transactionTwo 1
  , decommit "req-dec-timeout" Scenario.transactionTwo 0
  , recover "recover-post" True Nothing
  , recover "recover-missing" False Nothing
  , recover "recover-blocked" True (Just 7)
  , ack "ack-early" initial (sign carolSk snapshot)
  , ack "ack-partial" requested (sign carolSk snapshot)
  , ack "ack-duplicate" duplicate (sign carolSk snapshot)
  , ack "ack-confirm" ready (sign carolSk snapshot)
  , ack "ack-invalid" ready (sign aliceSk snapshot)
  ]
 where
  initial = Scenario.initialNode
  os = Scenario.openState initial
  env = Scenario.environment 0
  applied = aggregateState initial $ onOpenNetworkReqTx env simpleLedger initial.chainPointTime os 1 mempty Scenario.transactionOne
  requested = aggregateState applied $ onOpenNetworkReqSn env simpleLedger mempty applied.chainPointTime (Scenario.openState applied) 1 alice 0 1 [1] Nothing Nothing
  snapshot = case (Scenario.openState requested).coordinatedHeadState.seenSnapshot of
    SeenSnapshot{snapshot = value} -> value
    _ -> error "comparison fixture did not request a snapshot"
  collision = setCoordinated initial $ os.coordinatedHeadState{localUTxO = utxoRef 1 <> utxoRef 2}
  -- Missing payload retention is an intentional correction, tested separately.
  knownDecommit = setCoordinated initial $ os.coordinatedHeadState{allTxs = Map.singleton 1 Scenario.transactionOne}
  inFlight = setCoordinated initial $ os.coordinatedHeadState{seenSnapshot = RequestedSnapshot 0 1}
  duplicate = setCoordinated requested $ (Scenario.openState requested).coordinatedHeadState{seenSnapshot = mkSeenSnapshot snapshot $ Map.singleton carol $ sign carolSk snapshot}
  ready = setCoordinated requested $ (Scenario.openState requested).coordinatedHeadState{seenSnapshot = mkSeenSnapshot snapshot $ Map.fromList [(alice, sign aliceSk snapshot), (bob, sign bobSk snapshot)]}
  setCoordinated :: NodeState SimpleTx -> CoordinatedHeadState SimpleTx -> NodeState SimpleTx
  setCoordinated node coordinated = node{headState = Open (Scenario.openState node){coordinatedHeadState = coordinated}}
  req name partyIndex node ttl tx =
    let environment = ruleEnvironment $ Scenario.environment partyIndex
        st = Scenario.openState node
        machine = ReqTx.machine environment node.chainPointTime st ttl mempty tx
     in Example
          name
          node
          (Interpreter.ledgerConfig simpleLedger)
          (FrozenProgram $ LegacyPrograms.onOpenNetworkReqTxProgram environment node.chainPointTime st ttl mempty tx)
          (Workflow.run machine)
          (getText $ renderStateDiagram $ baseMachineAsGraph machine)
  reqSn name node version ids dec =
    let environment = ruleEnvironment env
        st = Scenario.openState node
        machine = ReqSn.machine environment mempty node.chainPointTime st 1 alice version 1 ids dec Nothing
     in Example
          name
          node
          (Interpreter.snapshotConfig env simpleLedger)
          (FrozenProgram $ LegacyPrograms.onOpenNetworkReqSnProgram environment mempty node.chainPointTime st 1 alice version 1 ids dec Nothing)
          (Workflow.run machine)
          (getText $ renderStateDiagram $ baseMachineAsGraph machine)
  decommit name tx ttl =
    let machine = ReqDec.machine (ruleEnvironment env) ttl initial.chainPointTime mempty os tx
     in Example
          name
          initial
          (Interpreter.ledgerConfig simpleLedger)
          (FrozenOutcome $ LegacyIncremental.onOpenNetworkReqDec env simpleLedger ttl initial.chainPointTime mempty os tx)
          (Workflow.run machine)
          (getText $ renderStateDiagram $ baseMachineAsGraph machine)
  recover name registered finalized =
    let pending = if registered then Map.singleton 7 (Deposit os.headId (utxoRef 99) initialChainTime (addUTCTime 3600 initialChainTime) Active) else mempty
        machine = Recover.machine 0 pending finalized 7
     in Example
          name
          initial
          Interpreter.basicConfig
          (FrozenOutcome $ LegacyIncremental.onClientRecover 0 pending finalized 7)
          (Workflow.run machine)
          (getText $ renderStateDiagram $ baseMachineAsGraph machine)
  ack name node signature =
    let environment = ruleEnvironment env
        st = Scenario.openState node
        machine = AckSn.machine environment node.chainPointTime mempty st carol signature 1
     in Example
          name
          node
          Interpreter.basicConfig
          (FrozenProgram $ LegacyPrograms.onOpenNetworkAckSnProgram environment node.chainPointTime mempty st carol signature 1)
          (Workflow.run machine)
          (getText $ renderStateDiagram $ baseMachineAsGraph machine)

compareExample :: Example -> Either Text (Outcome SimpleTx, [TraceStep])
compareExample (Example _ before config reference program _) = case reference of
  FrozenProgram free -> comparePrograms config before free program
  FrozenOutcome expected -> do
    let actual@(outcome, _) = Interpreter.traceProgramWith config program
    unless (outcome == expected && Interpreter.runProgramWith config program == expected) $ Left "outcome differs from frozen handler"
    unless (decodeFull' (serialize' $ aggregateState before outcome) == Right (aggregateState before expected)) $ Left "serialized resulting state differs"
    pure actual

-- | Equal labels alone are insufficient: compare complete outcomes, public
-- primitive arguments/replies, staged payloads, and serialized resulting states.
comparePrograms :: Interpreter.InterpreterConfig SimpleTx ledger signing -> NodeState SimpleTx -> Program SimpleTx ledger signing () -> Program SimpleTx ledger signing () -> Either Text (Outcome SimpleTx, [TraceStep])
comparePrograms config before free crem = do
  let expected@(outcome, _) = Interpreter.traceProgramWith config free
      actual = Interpreter.traceProgramWith config crem
      after = aggregateState before outcome
  unless (fst actual == fst expected && filter retainedDecision (snd actual) == filter retainedDecision (snd expected)) $
    Left $
      "ordered outcome or retained decision trace differs\nexpected: " <> show expected <> "\nactual: " <> show actual
  unless (Interpreter.runProgramWith config crem == outcome) $ Left "tracing changes the outcome"
  unless (decodeFull' (serialize' $ aggregateState before $ fst actual) == Right after) $ Left "serialized resulting state differs"
  freePrepared <- Debug.prepare Debug.RecordReplies config free
  cremPrepared <- Debug.prepare (Debug.RecordedReplies freePrepared.queries) config crem
  unless (fmap view (filter retainedStep freePrepared.steps) == fmap view (filter retainedStep cremPrepared.steps)) $ Left "primitive arguments or staged payloads differ"
  unless (Interpreter.runProgramWith config cremPrepared.lowered == outcome) $ Left "recorded replay changes the outcome"
  pure actual
 where
  view Debug.Step{kind, description, decisionName, payload} = (kind, description, decisionName, payload)
  retainedStep :: Debug.Step -> Bool
  retainedStep step = maybe True (`notElem` changedDecisions) step.decisionName
  retainedDecision (Decided name _) = name `notElem` changedDecisions
  retainedDecision _ = True

-- Capacity adds these guards; useful-work selection now runs only for the
-- eligible leader. Outcomes, primitive tapes and every other decision still
-- compare in order. Capacity regressions test the changed decisions separately.
changedDecisions :: [Text]
changedDecisions =
  [ "snapshot request has useful work"
  , "proposal matches the current snapshot context"
  , "unsettled fanout fits accumulator capacity"
  , "settled fanout fits accumulator capacity"
  , "pending transaction outputs avoid the carried deposit"
  , "pending transaction fits both fanout alternatives"
  , "decommit fits local and confirmed capacity"
  ]

writeDiagrams :: FilePath -> IO ()
writeDiagrams directory = do
  createDirectoryIfMissing True directory
  sections <- forM examples $ \example@(Example name _ config reference _ topology) -> do
    (_, trace) <- either (fail . toString . ((name <> ": ") <>)) pure $ compareExample example
    let traces = case reference of
          FrozenProgram free -> [("Frozen free program: " <> name, snd $ Interpreter.traceProgramWith config free), ("Production crem with program actions: " <> name, trace)]
          FrozenOutcome{} -> [("Production crem with program actions: " <> name, trace)]
        diagram = Mermaid.renderTraces traces
        file = toString name <> ".mmd"
    Text.writeFile (directory </> file) diagram
    when (name `elem` topologyExamples) $
      Text.writeFile (directory </> toString name <> "-topology.mmd") (Text.stripEnd topology <> "\n")
    pure (name, file, diagram, topology)
  Text.writeFile (directory </> "README.md") $
    "# Programs inside crem\n\nGenerated by `cabal run hydra-node:head-logic-lab -- compare-crem "
      <> toText directory
      <> "`.\n\n"
      <> "ReqTx, ReqSn and AckSn traces show each implementation's own execution. Generation checks ordered outcomes, primitive queries and replies, staged payloads, serialized states, and unchanged decisions. The named capacity guards and the moved useful-work decision are checked separately in CapacitySpec. ReqDec and Recover had no previous instruction trace; their outcomes and serialized states are checked against the frozen handlers.\n\n"
      <> "Start with [waiting for a dependency](req-tx-wait.mmd) or [rejecting a final invalid signature](ack-invalid.mmd). Read down the component lanes; the terminal note states which changes survive.\n\n"
      <> "The [ReqTx](req-tx-leader-topology.mmd), [ReqSn](req-sn-sign-topology.mmd), [AckSn](ack-confirm-topology.mmd), [ReqDec](req-dec-record-topology.mmd) and [Recover](recover-post-topology.mmd) graphs come from crem's topology. They constrain phase changes, including edges whose actions can terminate early by waiting or rejecting. They do not enumerate decisions inside each action; the execution traces show those decisions. Self-transitions are implicit.\n\n"
      <> foldMap (\(name, file, _, _) -> "- [" <> name <> "](" <> toText file <> ")\n") sections
      <> foldMap
        ( \(name, _, diagram, topology) ->
            if name == "ack-invalid"
              then "\n## Invalid final acknowledgement\n\n```mermaid\n" <> diagram <> "```\n"
              else
                if name `elem` topologyExamples
                  then "\n## " <> name <> " topology\n\n```mermaid\n" <> topology <> "\n```\n"
                  else ""
        )
        sections
  putStrLn $ "Compared " <> show (length examples) <> " examples; wrote " <> directory
 where
  topologyExamples = ["req-tx-leader", "req-sn-sign", "ack-confirm", "req-dec-record", "recover-post"]
