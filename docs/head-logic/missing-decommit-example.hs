{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | ReqSn overtakes ReqDec. Both rounds use the normal rules and real signatures.
module MissingDecommit (probe, writeTrace) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Text
import Hydra.HeadLogic qualified as Head
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (SeenSnapshot))
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Prelude
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregateInOrder, sign)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.Mermaid qualified as Mermaid
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)

probe :: IO ()
probe = do
  (final, _) <- either (fail . toString) pure scenario
  let current = (Scenario.openState final).coordinatedHeadState
  print ((getSnapshot current.confirmedSnapshot).number, current.decommitTx, (getSnapshot current.confirmedSnapshot).utxo)

writeTrace :: FilePath -> IO ()
writeTrace path = do
  (_, traces) <- either (fail . toString) pure scenario
  Text.writeFile path $ Mermaid.renderTraces traces

type Run = (NodeState SimpleTx, [(Text, [TraceStep])])

scenario :: Either Text Run
scenario = do
  let base :: Snapshot SimpleTx
      base = Scenario.initialSnapshot{utxo = utxoRef 1 <> utxoRef 2, accumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 1 <> utxoRef 2), appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 1 <> utxoRef 2)}
      signers = [("Alice", alice, aliceSk), ("Bob", bob, bobSk), ("Carol", carol, carolSk)]
      signed = aggregateInOrder (Map.fromList [(p, sign sk base) | (_, p, sk) <- signers]) Scenario.scenarioParties
      open = Scenario.openState Scenario.initialNode
      initial = Scenario.initialNode{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{localUTxO = base.utxo, confirmedSnapshot = ConfirmedSnapshot base signed}}}
      decommit = SimpleTx 10 (utxoRef 1) (utxoRef 10)
      step :: Text -> Run -> Party -> Message SimpleTx -> Either Text Run
      step title (node, traces) sender msg =
        let env = Scenario.environment 1
            input = Head.NetworkInput 2 $ ReceivedMessage sender msg
            (outcome, observations) = Interpreter.traceProgramWith (Interpreter.snapshotConfig env simpleLedger) $ Head.updateProgram (Head.ruleEnvironment env) initialChainTime node input
         in case outcome of
              Head.Continue{} -> Right (Head.aggregateState node outcome, traces <> [(title, observations)])
              _ -> Left $ show outcome
      confirm :: Run -> Either Text Run
      confirm current@(node, _) = case (Scenario.openState node).coordinatedHeadState.seenSnapshot of
        SeenSnapshot candidate _ _ -> foldlM (\next (name, p, sk) -> step ("AckSn " <> show candidate.number <> " from " <> name) next p $ AckSn (sign sk candidate) candidate.number) current signers
        _ -> Left "no candidate snapshot"
  requested <- step "Bob accepts snapshot 1 before ReqDec" (initial, []) alice $ ReqSn 0 1 [] (Just decommit) Nothing
  confirmed <- confirm requested
  afterTx@(node, _) <- step "Bob receives transaction 20 and requests snapshot 2" confirmed bob $ ReqTx $ SimpleTx 20 (utxoRef 2) (utxoRef 20)
  let current = (Scenario.openState node).coordinatedHeadState
      emitted = Head.update (Scenario.environment 1) simpleLedger initialChainTime (fst confirmed) $ Head.NetworkInput 2 $ ReceivedMessage bob $ ReqTx $ SimpleTx 20 (utxoRef 2) (utxoRef 20)
  request <- case emitted of
    Head.Continue _ effects -> case [req | Head.NetworkEffect req@ReqSn{} <- effects] of
      [req] -> Right req
      _ -> Left "expected one snapshot request"
    _ -> Left "transaction did not complete"
  unless (current.decommitTx == Just decommit) $ Left "confirmation lost the carried transaction"
  unless (request == ReqSn 0 2 [20] (Just decommit) Nothing) $ Left "next request lost the carried transaction"
  requestedAgain <- step "Bob accepts snapshot 2 carrying the retained decommit" afterTx bob request
  confirm requestedAgain
