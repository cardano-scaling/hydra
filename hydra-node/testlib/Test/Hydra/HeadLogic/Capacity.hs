{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | A fixed capacity counterexample and its corrected execution. The audit uses
-- SimpleTx sets directly, independently of the production selector and ledger.
module Test.Hydra.HeadLogic.Capacity (demonstration, requestsFit) where

import Hydra.Prelude hiding (state, trace)

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value, object, (.=))
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.HeadLogic (aggregateNodeState, aggregateState, ruleEnvironment, update)
import Hydra.HeadLogic.Input (Input (NetworkInput))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Outcome (Effect (NetworkEffect), Outcome (Continue, Error, Wait, stateChanges), StateChanged)
import Hydra.HeadLogic.State (CoordinatedHeadState (allTxs, confirmedSnapshot, localUTxO, seenSnapshot), OpenState (coordinatedHeadState), SeenSnapshot (RequestedSnapshot), seenSnapshotNumber, snapshotInFlight)
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.Ledger.Simple (SimpleTx (..), SimpleTxOut, simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (ChainPointTime (currentChainTime), NodeState (chainPointTime))
import Hydra.Tx.Accumulator (maxAccumulatorSize)
import Hydra.Tx.Snapshot (Snapshot (number, utxo), getSnapshot)
import Test.Hydra.HeadLogic.Explorer qualified as Explorer
import Test.Hydra.HeadLogic.LegacyPrograms qualified as Legacy
import Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario

-- | Inspect the actual emitted request against its sender's new confirmed base
-- and registry. These fixtures have no incremental actions or chain changes.
requestsFit :: NodeState SimpleTx -> Outcome SimpleTx -> Bool
requestsFit node outcome = all fits [message | NetworkEffect message <- effects]
 where
  effects = case outcome of Continue _ emitted -> emitted; _ -> []
  current = (Scenario.openState node).coordinatedHeadState
  fits (ReqSn _ _ ids Nothing Nothing) = case traverse (`Map.lookup` current.allTxs) ids >>= foldlM apply (getSnapshot current.confirmedSnapshot).utxo of
    Just after -> Set.size after <= maxAccumulatorSize
    Nothing -> False
  fits ReqSn{} = False
  fits _ = True
  apply :: Set SimpleTxOut -> SimpleTx -> Maybe (Set SimpleTxOut)
  apply available tx
    | tx.txInputs `Set.isSubsetOf` available = Just $ (available Set.\\ tx.txInputs) <> tx.txOutputs
    | otherwise = Nothing

-- | Historical mode uses frozen programs and checks their event reducer against
-- the current one. Corrected mode executes the node's complete input program.
-- The compact JSON is a demonstration report; generic replay uses Scenario v2.
demonstration :: Bool -> Either Text (Value, [(Text, [TraceStep])])
demonstration historical = go 40 Scenario.capacityWorld [] [] []
 where
  go :: Int -> Scenario.World -> [Value] -> [(Text, [TraceStep])] -> [Bool] -> Either Text (Value, [(Text, [TraceStep])])
  go _ world@Scenario.World{pending = []} entries traces audits = do
    let confirmed = map ((.number) . getSnapshot . (.confirmedSnapshot) . (.coordinatedHeadState) . Scenario.openState) world.nodes
    if historical
      then do
        ensure "historical requests unexpectedly all fit" (not $ and audits)
        ensure "historical round does not stall" (confirmed == [3, 3, 3] && maybe False ((== RequestedSnapshot 3 4) . (.seenSnapshot) . (.coordinatedHeadState) . Scenario.openState) (listToMaybe world.nodes))
        ensure "historical progress failure is missing" (Explorer.progressFailure world == Just "quiescent scenario has unconfirmed transactions")
      else do
        ensure "corrected request fails independent capacity audit" (and audits)
        ensure "corrected merge does not confirm everywhere" (confirmed == [5, 5, 5])
        ensure "corrected final state fails an invariant" (isNothing $ Explorer.invariantFailure world)
    pure (object ["schemaVersion" .= (1 :: Int), "scenario" .= ("capacity-boundary" :: Text), "historical" .= historical, "capacity" .= maxAccumulatorSize, "steps" .= reverse entries, "confirmed" .= confirmed], reverse traces)
  go 0 _ _ _ _ = Left "capacity demonstration exceeds 40 deliveries"
  go remaining world@Scenario.World{pending = delivery : rest} entries traces audits = do
    let node = world.nodes !! delivery.recipientIndex
        env = Scenario.environment delivery.recipientIndex
        input = NetworkInput delivery.remainingTTL $ ReceivedMessage (Scenario.scenarioParties !! delivery.senderIndex) delivery.message
        execution = if historical then legacyExecution world delivery else Explorer.programFor world delivery
        (outcome, observations) = case execution of Interpreter.Execution config program -> Interpreter.traceProgramWith config program
        retained = case outcome of Continue events _ -> events; Wait{stateChanges} -> stateChanges; Error{} -> []
        after = aggregateState node outcome
        audit = requestsFit after outcome
        next = Explorer.installOutcome world{Scenario.pending = rest} delivery outcome
    ensure "trace changes production execution" (historical || outcome == update env simpleLedger node.chainPointTime.currentChainTime node input)
    ensure "historical reducer differs" (not historical || foldl' legacyAggregateNodeState node retained == after)
    restored <- first show $ decodeFull' (serialize' node)
    replayed <- first show $ decodeFull' (serialize' retained)
    ensure "serialized events change state" (foldl' aggregateNodeState restored (replayed :: [StateChanged SimpleTx]) == after)
    ensure "checkpoint changes state" (decodeFull' (serialize' after) == Right after)
    ensure "snapshot safety invariant fails" (isNothing $ Explorer.safetyFailure next)
    ensure "corrected handler rejects or waits" (historical || case outcome of Continue{} -> True; _ -> False)
    let current = (Scenario.openState after).coordinatedHeadState
        terminal = case outcome of Error err -> show err; Wait{} -> "wait"; Continue{} -> "continue"
        title = Scenario.actionLabel (Scenario.Deliver delivery) <> " — " <> terminal
        entry = object ["delivery" .= delivery, "outcome" .= terminal, "requestFits" .= audit, "localOutputs" .= Set.size current.localUTxO, "confirmed" .= (getSnapshot current.confirmedSnapshot).number, "seen" .= seenSnapshotNumber current.seenSnapshot, "inFlight" .= snapshotInFlight current.seenSnapshot]
    go (remaining - 1) next (entry : entries) ((title, observations) : traces) (audit : audits)

  ensure :: Text -> Bool -> Either Text ()
  ensure reason condition = unless condition $ Left reason

legacyExecution :: Scenario.World -> Scenario.Delivery -> Interpreter.Execution SimpleTx
legacyExecution world delivery = case delivery.message of
  ReqTx tx ->
    Interpreter.Execution (Interpreter.ledgerConfig simpleLedger) $
      Legacy.onOpenNetworkReqTxProgram (ruleEnvironment env) node.chainPointTime open delivery.remainingTTL mempty tx
  ReqSn version number ids decommit deposit ->
    Interpreter.Execution (Interpreter.snapshotConfig env simpleLedger) $
      Legacy.onOpenNetworkReqSnProgram (ruleEnvironment env) mempty node.chainPointTime open delivery.remainingTTL sender version number ids decommit deposit
  AckSn signature number ->
    Interpreter.Execution Interpreter.basicConfig $
      Legacy.onOpenNetworkAckSnProgram (ruleEnvironment env) node.chainPointTime mempty open sender signature number
  ReqDec{} -> error "capacity fixture has no decommit"
 where
  node = world.nodes !! delivery.recipientIndex
  open = Scenario.openState node
  env = Scenario.environment delivery.recipientIndex
  sender = Scenario.scenarioParties !! delivery.senderIndex
