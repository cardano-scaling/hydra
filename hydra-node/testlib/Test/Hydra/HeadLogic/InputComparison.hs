{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Full input-path comparisons. The baseline has its own routing and handlers;
-- pure selection helpers, primitive implementations and codecs remain shared.
module Test.Hydra.HeadLogic.InputComparison (compareInput, examples, writeDiagrams) where

import Cardano.Binary (decodeFull', serialize')
import Crem.Render.Render (Mermaid (..), baseMachineAsGraph, renderStateDiagram)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Text
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Chain (ChainEvent (..), OnChainTx (..), PostTxError (..))
import Hydra.HeadLogic (
  CoordinatedHeadState (confirmedSnapshot, seenSnapshot),
  HeadState (Idle, Open),
  IdleState (IdleState),
  Input (ChainInput, ClientInput, NetworkInput),
  OpenState (coordinatedHeadState, headId, headSeed),
  Outcome (Continue, Error, Wait),
  StateChanged,
  aggregateNodeState,
  aggregateState,
  mkFullFanoutTx,
  mkSeenSnapshot,
  ruleEnvironment,
  update,
  updateProgram,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Rules.Decommit qualified as Decommit
import Hydra.HeadLogic.Rules.Input qualified as Input
import Hydra.HeadLogic.Rules.Settlement qualified as Settlement
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Network qualified as Network
import Hydra.Network.Message (NetworkEvent (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state, trace)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature, aggregate, aggregateInOrder, sign)
import Hydra.Tx.HeadId (HeadId (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.LegacyHandlers qualified as Legacy
import Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState)
import Test.Hydra.HeadLogic.Mermaid qualified as Mermaid
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk)

compareInput :: Environment -> UTCTime -> NodeState SimpleTx -> Input SimpleTx -> Either Text (Outcome SimpleTx, [TraceStep])
compareInput env now before input = do
  let expected = Legacy.update env simpleLedger now before input
      program = updateProgram (ruleEnvironment env) now before input
      config = Interpreter.snapshotConfig env simpleLedger
      actual@(outcome, _) = Interpreter.traceProgramWith config program
      events = case expected of Continue es _ -> es; Wait _ es -> es; Error{} -> []
      expectedState = foldl' legacyAggregateNodeState before events
      after = aggregateState before outcome
  unless (outcome == expected) $ Left $ "input outcome differs\nexpected: " <> show expected <> "\nactual: " <> show outcome
  unless (update env simpleLedger now before input == expected) $ Left "tracing changes the outcome"
  unless (after == expectedState) $ Left "state differs from frozen event aggregation"
  unless (decodeFull' (serialize' after) == Right expectedState) $ Left "serialized state differs"
  decoded <- first show $ decodeFull' @[StateChanged SimpleTx] $ serialize' events
  unless (foldl' aggregateNodeState before decoded == expectedState) $ Left "serialized event replay differs"
  prepared <- Debug.prepare Debug.RecordReplies config program
  replayed <- Debug.prepare (Debug.RecordedReplies prepared.queries) config program
  unless (Interpreter.runProgramWith config replayed.lowered == expected) $ Left "primitive replay changes the outcome"
  pure actual

examples :: [(Text, NodeState SimpleTx, Input SimpleTx)]
examples =
  [ ("init-request", idle, ClientInput Init)
  , ("init-observed", idle, observe $ OnInitTx hid os.headSeed (mkHeadParameters env) env.participants)
  , ("decommit-request", initial, ClientInput $ Decommit Scenario.transactionOne)
  , ("decommit-invalid", initial, ClientInput $ Decommit Scenario.transactionTwo)
  , ("deposit-observed", initial, deposit)
  , ("deposit-activated", deposited, ChainInput $ Tick (addUTCTime 100 initialChainTime) 2)
  , ("deposit-recovered", deposited, observe $ OnRecoverTx hid 42 (utxoRef 42))
  , ("recover-previous-head", deposited{headState = Open os{headId = UnsafeHeadId "another-head"}}, observe $ OnRecoverTx hid 42 (utxoRef 42))
  , ("increment-adopt", seenCommit, observe $ OnIncrementTx hid 1 42 1 (signed commit))
  , ("increment-invalid-signatures", seenCommit, observe $ OnIncrementTx hid 1 42 1 (aggregate []))
  , ("decrement-adopt", seenDecommit, observe $ OnDecrementTx hid 1 (utxoRef 2) 1 (signed decommit))
  , ("close-request", initial, ClientInput Close)
  , ("close-adopt", seen, observe $ OnCloseTx hid 1 deadline (signed snapshot))
  , ("contest-adopt", closedSeen, observe $ OnContestTx hid 1 deadline (signed snapshot))
  , ("side-load-current", initial, ClientInput $ SideLoadSnapshot os.coordinatedHeadState.confirmedSnapshot)
  , ("side-load-invalid-signatures", initial, ClientInput $ SideLoadSnapshot $ ConfirmedSnapshot snapshot (aggregate []))
  , ("fanout-ready", closed, ChainInput $ Tick (addUTCTime 1 deadline) 3)
  , ("fanout-request", closed, ClientInput Fanout)
  , ("fanout-selection", closed, ClientInput $ PartialFanout $ utxoRef 1)
  , ("fanout-empty-selection", closed, ClientInput $ PartialFanout mempty)
  , ("fanout-passive-observer", closed, observe $ OnPartialFanoutTx hid $ utxoRef 1)
  , ("fanout-driver-observation", driving, observe $ OnPartialFanoutTx hid $ utxoRef 1)
  , ("fanout-finished", driving, observe $ OnFanoutTx hid $ utxoRef 1)
  , ("fanout-post-failure", driving, ChainInput $ PostTxError fullFanout FailedToConstructFanoutTx Nothing)
  , ("fanout-stale-post", driving, ChainInput $ PostTxError fullFanout StalePartialFanoutTx Nothing)
  , ("fanout-rollback", driving, ChainInput $ Rollback initialChainTime 0)
  , ("open-rollback", initial, ChainInput $ Rollback initialChainTime 0)
  , ("connectivity", initial, NetworkInput 1 $ ConnectivityEvent Network.NetworkConnected)
  , ("unsynced-client", catchingUp, ClientInput Close)
  , ("unsynced-connectivity", catchingUp, NetworkInput 1 $ ConnectivityEvent Network.NetworkConnected)
  , ("unsynced-chain", catchingUp, observe $ OnCloseTx hid 0 deadline (aggregate []))
  ]
 where
  env = Scenario.environment 0
  initial = Scenario.initialNode
  os = Scenario.openState initial
  hid = os.headId
  idle = initial{headState = Idle $ IdleState 0}
  deadline = addUTCTime 100 initialChainTime
  observe :: OnChainTx SimpleTx -> Input SimpleTx
  observe tx = ChainInput $ Observation tx 1
  advance st input = aggregateState st $ Legacy.update env simpleLedger initialChainTime st input
  deposit = observe $ OnDepositTx hid 42 (utxoRef 42) initialChainTime (addUTCTime 1000 initialChainTime)
  deposited = advance initial deposit
  snapshot, commit, decommit :: Snapshot SimpleTx
  snapshot = Scenario.initialSnapshot{number = 1}
  commit = snapshot{utxoToCommit = Just $ utxoRef 42, depositTxId = Just 42, appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 1 <> utxoRef 42)}
  decommit = snapshot{utxoToDecommit = Just $ utxoRef 2, accumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 1 <> utxoRef 2)}
  withSeen sn = initial{headState = Open os{coordinatedHeadState = os.coordinatedHeadState{seenSnapshot = mkSeenSnapshot sn mempty}}}
  seen = withSeen snapshot
  seenCommit = withSeen commit
  seenDecommit = withSeen decommit
  signed :: Snapshot SimpleTx -> MultiSignature (Snapshot SimpleTx)
  signed sn = aggregateInOrder (Map.fromList [(p, sign sk sn) | (p, sk) <- zip Scenario.scenarioParties [aliceSk, bobSk, carolSk]]) Scenario.scenarioParties
  closed = advance initial $ observe $ OnCloseTx hid 0 deadline (aggregate [])
  closedSeen = advance seen $ observe $ OnCloseTx hid 0 deadline (aggregate [])
  driving = advance closed $ ClientInput Fanout
  fullFanout = mkFullFanoutTx os.coordinatedHeadState.confirmedSnapshot 0 os.headSeed deadline
  catchingUp = NodeCatchingUp initial.headState initial.deposits initial.chainPointTime

writeDiagrams :: FilePath -> IO ()
writeDiagrams directory = do
  createDirectoryIfMissing True directory
  forM_ examples $ \(name, before, input) -> do
    (_, trace) <- either (fail . toString) pure $ compareInput env initialChainTime before input
    Text.writeFile (directory </> toString name <> ".mmd") $ Mermaid.renderTraces [(name, trace)]
  Text.writeFile (directory </> "input-topology.mmd") $
    getText $
      renderStateDiagram $
        baseMachineAsGraph $
          Input.machine (ruleEnvironment env) initialChainTime Scenario.initialNode (ClientInput Close)
  Text.writeFile (directory </> "decommit-topology.mmd") $
    getText $
      renderStateDiagram $
        baseMachineAsGraph $
          Decommit.machine os.headId 0 os.coordinatedHeadState Scenario.transactionOne
  Text.writeFile (directory </> "settlement-topology.mmd") $
    getText $
      renderStateDiagram $
        baseMachineAsGraph $
          Settlement.machine (ruleEnvironment env) mempty os 1 1 (Settlement.Increment 42) 1 (aggregate [])
 where
  os = Scenario.openState Scenario.initialNode
  env = Scenario.environment 0
