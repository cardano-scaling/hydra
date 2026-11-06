{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | A request overtakes ReqDec; confirmation loses the carried transaction.
-- This characterises an unresolved liveness defect, not the desired behavior.
module MissingDecommit (probe) where

import Data.Map.Strict qualified as Map
import Hydra.HeadLogic qualified as Head
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (SeenSnapshot))
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Prelude
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregateInOrder, sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)

probe :: IO ()
probe = do
  let base :: Snapshot SimpleTx
      base = Scenario.initialSnapshot{utxo = utxoRef 1 <> utxoRef 2, accumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 1 <> utxoRef 2), appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 1 <> utxoRef 2)}
      signed = aggregateInOrder (Map.fromList [(p, sign sk base) | (p, sk) <- [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]]) Scenario.scenarioParties
      open = Scenario.openState Scenario.initialNode
      initial = Scenario.initialNode{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{localUTxO = base.utxo, confirmedSnapshot = ConfirmedSnapshot base signed}}}
      run node sender msg = Head.update (Scenario.environment 1) simpleLedger initialChainTime node $ Head.NetworkInput 2 $ ReceivedMessage sender msg
      deliver node sender msg = Head.aggregateState node (run node sender msg)
      afterRequest = deliver initial alice (ReqSn 0 1 [] (Just (SimpleTx 10 (utxoRef 1) (utxoRef 10))) Nothing)
  case (Scenario.openState afterRequest).coordinatedHeadState.seenSnapshot of
    SeenSnapshot candidate _ _ -> do
      let confirmed = foldl' (\node (p, sk) -> deliver node p (AckSn (sign sk candidate) 1)) afterRequest [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]
          chs = (Scenario.openState confirmed).coordinatedHeadState
          outcome = run confirmed bob (ReqTx (SimpleTx 20 (utxoRef 2) (utxoRef 20)))
          afterTx = Head.aggregateState confirmed outcome
      print ((getSnapshot chs.confirmedSnapshot).number, chs.decommitTx, (getSnapshot chs.confirmedSnapshot).utxoToDecommit)
      case outcome of
        Head.Continue _ effects -> forM_ effects $ \case
          Head.NetworkEffect request@ReqSn{} -> print request >> print (run afterTx bob request)
          _ -> pure ()
        _ -> print outcome
    _ -> print (run initial alice (ReqSn 0 1 [] (Just (SimpleTx 10 (utxoRef 1) (utxoRef 10))) Nothing))
