{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.ReplaySpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude (Expectation, Spec, describe, it, shouldBe)

import Cardano.Binary (decodeFull', serialize')
import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Hydra.HeadLogic.Machine qualified as Machine
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (CoordinatedHeadState (..), FinalizedSnapshot (..), HeadState (..), OpenState (..), SeenSnapshot (..))
import Hydra.HeadLogicSpec (inOpenState, testSnapshot, withAccumulators)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..), TrackedDeposit (..), initialChainTime)
import Hydra.Tx.Crypto (aggregate, sign)
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotVersion)
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, testHeadId)

spec :: Spec
spec = describe "commit and deposit replay" $ do
  forM_ [False, True] $ \settleFirst ->
    it (if settleFirst then "restores settlement before snapshot confirmation" else "restores approval before settlement without making the deposit spendable early") $ do
      let signature = sign aliceSk snapshot
          signatures = aggregate [signature]
          confirmed = ConfirmedSnapshot snapshot signatures
          settled = CommitFinalized (SimpleChainState 3) testHeadId 1 depositId
          spend = SimpleTx 100 deposited (utxoRef 100)
          checkConfirmed node = do
            (coordinated node).confirmedSnapshot `shouldBe` confirmed
            (coordinated node).seenSnapshot `shouldBe` LastSeenSnapshot 1
          checkSettled node = do
            node.pendingDeposits `shouldBe` mempty
            Map.lookup depositId node.deposits `shouldBe` Just (TrackedDeposit active 1 (Just 3))
            checkLocal Nothing deposited 1 node
          checkFinalized node = do
            checkConfirmed node
            checkSettled node
            (coordinated node).finalizedCommit `shouldBe` Just (FinalizedSnapshot confirmed 3)
          ending
            | settleFirst =
                [
                  ( settled
                  , \node -> do
                      checkSettled node
                      case (coordinated node).seenSnapshot of
                        SeenSnapshot{snapshot = candidate} -> candidate `shouldBe` snapshot
                        seen -> fail $ "expected a snapshot collecting signatures, got " <> show seen
                  )
                , (SnapshotConfirmed testHeadId (Just snapshot) signatures, checkFinalized)
                ]
            | otherwise =
                [ (SnapshotConfirmed testHeadId Nothing signatures, \node -> checkConfirmed node >> checkLocal (Just depositId) mempty 0 node)
                ,
                  ( CommitApproved testHeadId deposited
                  , \node -> do
                      checkConfirmed node
                      checkLocal (Just depositId) mempty 0 node
                      node.pendingDeposits `shouldBe` Map.singleton depositId active
                  )
                , (settled, checkFinalized)
                ]
      checkReplay $
        [
          ( recorded
          , \node -> do
              Map.lookup depositId node.deposits `shouldBe` Just (TrackedDeposit inactive 1 Nothing)
              checkLocal Nothing mempty 0 node
          )
        ,
          ( activated
          , \node -> do
              node.pendingDeposits `shouldBe` Map.singleton depositId active
              checkLocal (Just depositId) mempty 0 node
          )
        , (SnapshotRequested snapshot mempty (Just depositId), checkLocal (Just depositId) mempty 0)
        , (PartySignedSnapshot 1 alice signature, checkLocal (Just depositId) mempty 0)
        ]
          <> ending
          <> [ (TransactionReceived spend, checkLocal Nothing deposited 1)
             , (TransactionAppliedToLocalUTxO testHeadId spend, checkLocal Nothing (utxoRef 100) 1)
             , -- Re-observing an increment must not resurrect spent outputs.
               (settled, checkLocal Nothing (utxoRef 100) 1)
             ]

  it "restores expiry, recovery and rollback in the registry and selected deposit" $ do
    let expired = active{status = Expired}
    checkReplay
      [ (recorded, \node -> node.pendingDeposits `shouldBe` Map.singleton depositId inactive)
      , (activated, checkLocal (Just depositId) mempty 0)
      , -- Expiry changes eligibility, not an already selected or signed claim.

        ( DepositExpired depositId initialChainTime expired
        , \node -> do
            node.pendingDeposits `shouldBe` Map.singleton depositId expired
            checkLocal (Just depositId) mempty 0 node
        )
      ,
        ( DepositRecovered (SimpleChainState 3) testHeadId depositId deposited
        , \node -> do
            node.pendingDeposits `shouldBe` mempty
            Map.lookup depositId node.deposits `shouldBe` Just (TrackedDeposit expired 1 (Just 3))
            checkLocal Nothing mempty 0 node
        )
      ,
        ( ChainRolledBack (SimpleChainState 2)
        , \node -> do
            node.pendingDeposits `shouldBe` Map.singleton depositId expired
            Map.lookup depositId node.deposits `shouldBe` Just (TrackedDeposit expired 1 Nothing)
            checkLocal Nothing mempty 0 node
        )
      ,
        ( ChainRolledBack (SimpleChainState 0)
        , \node -> do
            node.deposits `shouldBe` mempty
            checkLocal Nothing mempty 0 node
        )
      ]
 where
  depositId = 7
  deposited = utxoRef 7
  inactive = Deposit testHeadId deposited initialChainTime (addUTCTime 3600 initialChainTime) Inactive
  active = inactive{status = Active}
  recorded = DepositRecorded (SimpleChainState 1) testHeadId depositId deposited inactive.created inactive.deadline
  activated = DepositActivated depositId initialChainTime active
  snapshot = withAccumulators $ (testSnapshot 1 0 [] mempty){utxoToCommit = Just deposited, depositTxId = Just depositId}

-- Compare a running machine with recovery from a serialized checkpoint and
-- serialized event at every prefix, as well as the independently expected state.
checkReplay :: [(StateChanged SimpleTx, NodeState SimpleTx -> Expectation)] -> Expectation
checkReplay = void . foldM step (Machine.start $ inOpenState [alice])
 where
  step :: Machine.Runtime SimpleTx -> (StateChanged SimpleTx, NodeState SimpleTx -> Expectation) -> IO (Machine.Runtime SimpleTx)
  step runtime (event, check) = do
    restored <- either (fail . show) pure $ decodeFull' $ serialize' $ Machine.currentState runtime
    retained <- either (fail . show) pure $ decodeFull' $ serialize' event
    let next = Machine.step runtime event
        node = Machine.currentState next
    Machine.applyEvents (inOpenState [alice]) [Checkpoint restored, retained] `shouldBe` node
    check node
    pure next

coordinated :: NodeState SimpleTx -> CoordinatedHeadState SimpleTx
coordinated node = case node.headState of
  Open open -> open.coordinatedHeadState
  observed -> error $ "expected an open head, got " <> show observed

checkLocal :: Maybe Integer -> UTxOType SimpleTx -> SnapshotVersion -> NodeState SimpleTx -> Expectation
checkLocal selected utxo version node = do
  let current = coordinated node
  current.currentDepositTxId `shouldBe` selected
  current.localUTxO `shouldBe` utxo
  current.version `shouldBe` version
