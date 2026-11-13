{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Capacity regression. The audit uses
-- SimpleTx sets directly, independently of the production selector and ledger.
module Test.Hydra.HeadLogic.Capacity (capacityRegression, requestsFit) where

import Hydra.Prelude hiding (state, trace)

import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.HeadLogic.Input (Input (NetworkInput))
import Hydra.HeadLogic.Outcome (Effect (NetworkEffect), Outcome (Continue))
import Hydra.HeadLogic.State (CoordinatedHeadState (allTxs, confirmedSnapshot), OpenState (coordinatedHeadState))
import Hydra.Ledger.Simple (SimpleTx (..), SimpleTxOut, simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.HeadLogic (update)
import Hydra.Node.State (ChainPointTime (currentChainTime), NodeState (chainPointTime))
import Hydra.Tx.Accumulator (maxAccumulatorSize)
import Hydra.Tx.Snapshot (Snapshot (number, utxo), getSnapshot)
import Test.Hydra.HeadLogic.Explorer qualified as Explorer
import Test.Hydra.HeadLogic.Recovery (recover)
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

-- | Every emitted request fits, and the merge confirms at all three nodes.
capacityRegression :: Either Text ()
capacityRegression = go 40 Scenario.capacityWorld
 where
  go :: Int -> Scenario.World -> Either Text ()
  go _ world@Scenario.World{pending = []} = do
    let confirmed = map ((.number) . getSnapshot . (.confirmedSnapshot) . (.coordinatedHeadState) . Scenario.openState) world.nodes
    ensure "merge does not confirm everywhere" (confirmed == [5, 5, 5])
    ensure "final state fails an invariant" (isNothing $ Explorer.invariantFailure world)
  go 0 _ = Left "capacity regression exceeds 40 deliveries"
  go remaining world@Scenario.World{pending = delivery : rest} = do
    let node = world.nodes !! delivery.recipientIndex
        env = Scenario.environment delivery.recipientIndex
        input = NetworkInput delivery.remainingTTL $ ReceivedMessage (Scenario.scenarioParties !! delivery.senderIndex) delivery.message
        outcome = update env simpleLedger node.chainPointTime.currentChainTime node input
        next = Explorer.installOutcome world{Scenario.pending = rest} delivery outcome
    after <- recover node outcome
    ensure "request fails independent capacity audit" (requestsFit after outcome)
    ensure "snapshot safety invariant fails" (isNothing $ Explorer.safetyFailure next)
    ensure "handler rejects or waits" (case outcome of Continue{} -> True; _ -> False)
    go (remaining - 1) next

  ensure :: Text -> Bool -> Either Text ()
  ensure reason condition = unless condition $ Left reason
