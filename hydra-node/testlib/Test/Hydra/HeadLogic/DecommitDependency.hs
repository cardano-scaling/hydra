{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Deliver every emitted message in the dependent-decommit schedule.
module Test.Hydra.HeadLogic.DecommitDependency (decommitDependencyRegression) where

import Data.List ((!!))
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.HeadLogic (
  CoordinatedHeadState (confirmedSnapshot, decommitTx, localUTxO),
  Effect (NetworkEffect),
  Input (NetworkInput),
  OpenState (coordinatedHeadState),
  Outcome (Continue),
  update,
 )
import Hydra.Node.State (NodeState, initialChainTime)
import Hydra.Prelude
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.CoordinatedAudit qualified as Audit
import Test.Hydra.HeadLogic.Recovery (recover)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)

data Delivery = Delivery Int Int (Message SimpleTx)
data Run = Run [NodeState SimpleTx] [Delivery]

decommitDependencyRegression :: Either Text ()
decommitDependencyRegression = do
  withParent <- broadcast 0 (ReqTx Scenario.transactionOne) initial
  withChild <- broadcast 0 (ReqTx Scenario.transactionTwo) withParent
  reserved@(Run reservedNodes _) <- broadcast 0 (ReqDec decommit) withChild
  ensure "ReqDec reserves the pending decommit inputs" $ all ((== mempty) . (.localUTxO) . coordinated) reservedNodes
  ensure "all nodes retain the decommit" $ all ((== Just decommit) . (.decommitTx) . coordinated) reservedNodes
  rebuilt@(Run rebuiltNodes _) <- drainSnapshots reserved
  ensure "rebase preserves the reservation" $ all (Audit.reservationHeld . coordinated) rebuiltNodes
  ensure "rebase retains the decommit" $ all ((== Just decommit) . (.decommitTx) . coordinated) rebuiltNodes
  confirmed@(Run confirmedNodes pending) <- drainAcknowledgements rebuilt
  ensure "snapshot 1 confirms at every node" $ all ((== 1) . (.number) . getSnapshot . (.confirmedSnapshot) . coordinated) confirmedNodes
  ensure "snapshot 2 confirms the dependency before requesting the decommit" $
    length pending == 3 && all (\(Delivery sender _ msg) -> sender == 1 && msg == ReqSn 0 2 [2] Nothing Nothing) pending
  Run final remaining <- drainAll 40 confirmed
  ensure "every emitted network message is delivered" $ null remaining
  ensure "snapshot 3 confirms the decommit at every node" $
    all (\node -> let snapshot = getSnapshot (coordinated node).confirmedSnapshot in snapshot.number == 3 && snapshot.utxo == mempty && snapshot.utxoToDecommit == Just (utxoRef 4)) final
 where
  initial = Run (replicate 3 Scenario.initialNode) []
  decommit = SimpleTx 3 (utxoRef 3) (utxoRef 4)
  coordinated node = (Scenario.openState node).coordinatedHeadState
  broadcast sender msg run = foldlM (\current recipient -> deliver (Delivery sender recipient msg) current) run [0, 1, 2]
  drainSnapshots run@(Run _ queued) = case queued of
    Delivery _ _ ReqSn{} : _ -> drainOne run >>= drainSnapshots
    _ -> Right run
  drainAcknowledgements run@(Run _ queued) = case queued of
    Delivery _ _ AckSn{} : _ -> drainOne run >>= drainAcknowledgements
    _ -> Right run
  drainAll :: Int -> Run -> Either Text Run
  drainAll _ run@(Run _ []) = Right run
  drainAll 0 _ = Left "decommit regression exceeds its delivery bound"
  drainAll n run = drainOne run >>= drainAll (n - 1)
  drainOne (Run nodes (d : ds)) = deliver d (Run nodes ds)
  drainOne _ = Left "empty delivery queue"

deliver :: Delivery -> Run -> Either Text Run
deliver (Delivery sender recipient msg) (Run nodes queued) = do
  let node = nodes !! recipient
      env = Scenario.environment recipient
      party = Scenario.scenarioParties !! sender
      input = NetworkInput 2 $ ReceivedMessage party msg
      outcome = update env simpleLedger initialChainTime node input
  after <- recover node outcome
  emitted <- case outcome of
    Continue _ effects -> pure [Delivery recipient target message | NetworkEffect message <- effects, target <- [0, 1, 2]]
    _ -> Left "unexpected terminal outcome in honest prefix"
  ensure "delivery preserves the reservation" $ Audit.reservationHeld (Scenario.openState after).coordinatedHeadState
  ensure "every requested decommit has confirmed inputs" $
    all (Audit.decommitReady (Scenario.openState after).coordinatedHeadState) [tx | Delivery _ _ (ReqSn _ _ _ (Just tx) _) <- emitted]
  pure $ Run (take recipient nodes <> [after] <> drop (recipient + 1) nodes) (queued <> emitted)

ensure :: Text -> Bool -> Either Text ()
ensure reason condition = unless condition $ Left reason
