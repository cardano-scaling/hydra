{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Input fixtures and interpreter, event replay and checkpoint consistency.
module Test.Hydra.HeadLogic.InputFixtures (checkInput, examples) where

import Cardano.Binary (decodeFull', serialize')
import Data.Map.Strict qualified as Map
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Chain (ChainEvent (..), OnChainTx (..), PostTxError (..))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Network qualified as Network
import Hydra.Network.Message (NetworkEvent (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.HeadLogic (
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
import Hydra.Node.HeadLogic.Configuration qualified as Configuration
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state, trace)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature, aggregate, aggregateInOrder, sign)
import Hydra.Tx.HeadId (HeadId (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk)

checkInput :: Environment -> UTCTime -> NodeState SimpleTx -> Input SimpleTx -> Either Text (Outcome SimpleTx, [TraceStep])
checkInput env now before input = do
  let program = updateProgram (ruleEnvironment env) now before input
      config = Configuration.interpreterConfig env simpleLedger
      actual@(outcome, _) = Interpreter.traceProgram config program
      events = case outcome of Continue es _ -> es; Wait _ es -> es; Error{} -> []
      after = aggregateState before outcome
  unless (update env simpleLedger now before input == outcome) $ Left "tracing changes the outcome"
  unless (decodeFull' (serialize' after) == Right after) $ Left "serialized state differs"
  decoded <- first show $ decodeFull' @[StateChanged SimpleTx] $ serialize' events
  unless (foldl' aggregateNodeState before decoded == after) $ Left "serialized event replay differs"
  prepared <- Debug.prepare Debug.RecordReplies config program
  replayed <- Debug.prepare (Debug.RecordedReplies prepared.queries) config program
  unless (Interpreter.runProgram config replayed.lowered == outcome) $ Left "primitive replay changes the outcome"
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
  advance st input = aggregateState st $ update env simpleLedger initialChainTime st input
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
