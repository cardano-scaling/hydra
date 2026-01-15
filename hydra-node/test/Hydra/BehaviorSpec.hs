{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.BehaviorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (shouldBe, shouldNotBe, shouldReturn, shouldSatisfy)

import Control.Concurrent.Class.MonadSTM (
  modifyTVar,
  modifyTVar',
  readTQueue,
  readTVarIO,
  retry,
  stateTVar,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (cancel, forConcurrently)
import Control.Monad.IOSim (IOSim, runSimTrace, selectTraceEventsDynamic)
import Data.List ((!!))
import Data.List qualified as List
import Hydra.API.ClientInput
import Hydra.API.Server (Server (..), mkTimedServerOutputFromStateEvent)
import Hydra.API.ServerOutput (ClientMessage (..), DecommitInvalidReason (..), ServerOutput (..), TimedServerOutput (..))
import Hydra.Cardano.Api (SigningKey)
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  initHistory,
 )
import Hydra.Chain.ChainState (ChainSlot (ChainSlot), ChainStateType, IsChainState, chainStatePoint, chainStateSlot)
import Hydra.Chain.Direct.Handlers (LocalChainState, getLatest, newLocalChainState, pushNew, rollback)
import Hydra.Events (EventSink (..))
import Hydra.Events.Rotation (EventStore (..))
import Hydra.HeadLogic (CoordinatedHeadState (..), Effect (..), HeadState (..), InitialState (..), Input (..), OpenState (..))
import Hydra.HeadLogicSpec (testSnapshot)
import Hydra.Ledger (Ledger, nextChainSlot)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (Tracer)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  DraftHydraNode (..),
  HydraNode (..),
  HydraNodeLog (..),
  NodeStateHandler (..),
  connect,
  createNodeStateHandler,
  defaultTxTTL,
  mkNetworkInput,
  queryNodeState,
  runHydraNode,
  waitDelay,
 )
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Node.DepositPeriod qualified as DP
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.InputQueue (InputQueue (enqueue), createInputQueue)
import Hydra.Node.State (NodeState (..), initNodeState)
import Hydra.Node.UnsyncedPeriod (defaultUnsyncedPeriodFor)
import Hydra.NodeSpec (createMockEventStore)
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod)
import Hydra.Tx (HeadId)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Crypto (HydraKey, aggregate, sign)
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Party (Party (..), deriveParty, getParty)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, getSnapshot)
import Test.Hydra.Tx.Fixture (
  alice,
  aliceSk,
  bob,
  bobSk,
  deriveOnChainId,
  testHeadId,
  testHeadSeed,
 )
import Test.QuickCheck (chooseEnum, counterexample, forAll, getNegative, ioProperty)
import Test.Util (
  propRunInSim,
  shouldBe,
  shouldNotBe,
  shouldRunInSim,
  shouldSatisfy,
  traceInIOSim,
 )

spec :: Spec
spec = parallel $ do
  describe "Sanity tests of test suite" $ do
    it "does not delay for real" $
      -- If it works, it simulates a lot of time passing within 1 second
      failAfter 1 $
        shouldRunInSim $
          threadDelay 600

  describe "Single participant Head" $ do
    it "accepts Init command" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n ->
            send n Init

    it "accepts Commit after successful Init" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            simulateCommit chain testHeadId alice (utxoRef 1)
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)

    it "can close an open head" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            simulateCommit chain testHeadId alice (utxoRef 1)
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRef 1}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed

    it "does not fanout automatically" $ do
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            simulateCommit chain testHeadId alice (utxoRef 1)
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRef 1}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed
            waitUntil [n1] $ ReadyToFanout testHeadId
            nothingHappensFor n1 100000

    it "does finalize head after contestation period upon command" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            simulateCommit chain testHeadId alice (utxoRef 1)
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRef 1}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed
            waitUntil [n1] $ ReadyToFanout testHeadId
            send n1 Fanout
            waitUntil [n1] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRef 1}

  -- XXX: Restructure test suites as it makes more sense to speak about
  -- features rather than head structure
  describe "Two participant Head" $ do
    it "only opens the head after all nodes committed" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])

              simulateCommit chain testHeadId alice (utxoRef 1)
              waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
              let veryLong :: MonadTimer m => m a -> m (Maybe a)
                  veryLong = timeout 1000000
              veryLong (waitForNext n1) >>= (`shouldNotBe` Just HeadIsOpen{headId = testHeadId, utxo = utxoRef 1})

              simulateCommit chain testHeadId bob (utxoRef 2)
              waitUntil [n1] $ Committed testHeadId bob (utxoRef 2)
              waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1, 2]}

    it "can abort and re-open a head when one party has not committed" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
              simulateCommit chain testHeadId alice (utxoRefs [1, 2])
              waitUntil [n1, n2] $ Committed testHeadId alice (utxoRefs [1, 2])
              send n2 Abort
              waitUntil [n1, n2] $ HeadIsAborted{headId = testHeadId, utxo = utxoRefs [1, 2]}
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])

    it "cannot abort head when commits have been collected" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
              simulateCommit chain testHeadId alice (utxoRef 1)
              simulateCommit chain testHeadId bob (utxoRef 2)

              waitUntil [n1, n2] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1, 2]}

              send n1 Abort

              m <- waitForNextMessage n1
              m `shouldSatisfy` \case
                CommandFailed{} -> True
                _ -> False

    it "ignores head initialization of other head" $
      shouldRunInSim $
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
              -- We expect bob to ignore alice's head which he is not part of
              -- although bob's configuration would includes alice as a
              -- peerconfigured)
              waitUntilMatch [n2] $ \case
                IgnoredHeadInitializing{headId, parties} ->
                  guard $ headId == testHeadId && parties == fromList [alice]
                _ -> Nothing

    it "outputs committed utxo when client requests it" $
      shouldRunInSim $
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
              simulateCommit chain testHeadId alice (utxoRef 1)

              waitUntil [n2] $ Committed testHeadId alice (utxoRef 1)
              headUTxO <- getHeadUTxO . headState <$> queryState n1
              fromMaybe mempty headUTxO `shouldBe` utxoRefs [1]

    describe "in an open head" $ do
      it "sees the head closed by other nodes" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2

                send n1 Close
                waitForNext n2
                  >>= assertHeadIsClosedWith 0

      it "valid new transactions are seen by all parties" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2

                send n1 (NewTx (aValidTx 42))
                waitUntil [n1, n2] $ TxValid testHeadId 42

      it "valid new transactions get snapshotted" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2

                let tx = aValidTx 42
                send n1 (NewTx tx)
                waitUntil [n1, n2] $ TxValid testHeadId 42

                let snapshot = Snapshot testHeadId 0 1 [tx] (utxoRefs [1, 2, 42]) mempty mempty
                    sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
                waitUntil [n1] $ SnapshotConfirmed testHeadId snapshot sigs

                send n1 Close
                waitForNext n1 >>= assertHeadIsClosedWith 1

      it "snapshots are created as long as transactions to snapshot exist" $
        shouldRunInSim $
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2

                -- Load the "ingest queue" of the head enough to have still
                -- pending transactions after a first snapshot request by
                -- alice. Note that we are in a deterministic simulation here.
                send n1 (NewTx $ aValidTx 40)
                send n1 (NewTx $ aValidTx 41)
                send n1 (NewTx $ aValidTx 42)

                -- Expect alice to create a snapshot from the first requested
                -- transaction right away which is the current snapshot policy.
                waitUntilMatch [n1, n2] $ \case
                  SnapshotConfirmed{snapshot = Snapshot{number, confirmed}} ->
                    guard $ number == 1 && confirmed == [aValidTx 40]
                  _ -> Nothing

                -- Expect bob to also snapshot what did "not fit" into the first
                -- snapshot.
                waitUntilMatch [n1, n2] $ \case
                  SnapshotConfirmed{snapshot = Snapshot{number, confirmed}} ->
                    -- NOTE: We sort the confirmed to be clear that the order may
                    -- be freely picked by the leader.
                    guard $ number == 2 && sort confirmed == [aValidTx 41, aValidTx 42]
                  _ -> Nothing

                -- As there are no pending transactions and snapshots anymore
                -- we expect to continue normally on seeing just another tx.
                send n1 (NewTx $ aValidTx 44)
                waitUntilMatch [n1, n2] $ \case
                  SnapshotConfirmed{snapshot = Snapshot{number, confirmed}} ->
                    guard $ number == 3 && confirmed == [aValidTx 44]
                  _ -> Nothing

      it "depending transactions stay pending and are confirmed in order" $
        shouldRunInSim $
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 -> do
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2
                let firstTx = SimpleTx 1 (utxoRef 1) (utxoRef 3)
                let secondTx = SimpleTx 2 (utxoRef 3) (utxoRef 4)
                -- Expect secondTx to be valid, but not applicable and stay pending
                send n2 (NewTx secondTx)
                send n1 (NewTx firstTx)

                -- Expect a snapshot of the firstTx transaction
                waitUntil [n1, n2] $ TxValid testHeadId 1
                waitUntil [n1, n2] $ do
                  let snapshot = testSnapshot 1 0 [firstTx] (utxoRefs [2, 3])
                      sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
                  SnapshotConfirmed testHeadId snapshot sigs

                -- Expect a snapshot of the now unblocked secondTx
                waitUntil [n1, n2] $ TxValid testHeadId 2
                waitUntil [n1, n2] $ do
                  let snapshot = testSnapshot 2 0 [secondTx] (utxoRefs [2, 4])
                      sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
                  SnapshotConfirmed testHeadId snapshot sigs

      it "depending transactions expire if not applicable in time" $
        shouldRunInSim $
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 -> do
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2
                let firstTx = SimpleTx 1 (utxoRef 1) (utxoRef 3)
                let secondTx = SimpleTx 2 (utxoRef 3) (utxoRef 4)
                -- Expect secondTx to be valid, but not applicable and stay pending
                send n2 (NewTx secondTx)
                -- If we wait too long, secondTx will expire
                threadDelay $ fromIntegral defaultTxTTL * waitDelay + 1
                waitUntilMatch [n1, n2] $ \case
                  TxInvalid{transaction} -> guard $ transaction == secondTx
                  _ -> Nothing

                send n1 (NewTx firstTx)
                waitUntil [n1, n2] $ TxValid testHeadId 1

      it "sending two conflicting transactions should lead one being confirmed and one expired" $
        shouldRunInSim $
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 -> do
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2
                let tx' =
                      SimpleTx
                        { txSimpleId = 1
                        , txInputs = utxoRef 1
                        , txOutputs = utxoRef 10
                        }
                    tx'' =
                      SimpleTx
                        { txSimpleId = 2
                        , txInputs = utxoRef 1
                        , txOutputs = utxoRef 11
                        }
                send n1 (NewTx tx')
                send n2 (NewTx tx'')
                waitUntil [n1, n2] $ do
                  let snapshot = testSnapshot 1 0 [tx'] (utxoRefs [2, 10])
                      sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
                  SnapshotConfirmed testHeadId snapshot sigs
                waitUntilMatch [n1, n2] $ \case
                  TxInvalid{transaction} -> guard $ transaction == tx''
                  _ -> Nothing

      it "outputs utxo from confirmed snapshot when client requests it" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead2 chain n1 n2
                let newTx = (aValidTx 42){txInputs = utxoRefs [1]}
                send n1 (NewTx newTx)

                let snapshot = testSnapshot 1 0 [newTx] (utxoRefs [2, 42])
                    sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]

                waitUntil [n1, n2] $ SnapshotConfirmed testHeadId snapshot sigs

                headUTxO <- getHeadUTxO . headState <$> queryState n1
                fromMaybe mempty headUTxO `shouldBe` utxoRefs [2, 42]

      describe "Incremental commit" $ do
        it "deposits with empty utxo are ignored" $
          ioProperty $
            shouldRunInSim $
              withSimulatedChainAndNetwork $ \chain ->
                withHydraNode aliceSk [] chain $ \n1 -> do
                  openHead chain n1
                  deadline <- newDeadlineFarEnoughFromNow
                  txid <- simulateDeposit chain testHeadId mempty deadline
                  -- NOTE: Deposit is not picked up and eventually expires
                  asExpected <- waitUntilMatch [n1] $ \case
                    DepositExpired{depositTxId} -> True <$ guard (depositTxId == txid)
                    CommitApproved{} -> Just False
                    _ -> Nothing
                  pure $
                    asExpected
                      & counterexample "Deposit with empty utxo approved instead of expired"

        prop "deposits with deadline in the past are ignored" $ \seconds ->
          ioProperty $
            shouldRunInSim $
              withSimulatedChainAndNetwork $ \chain ->
                withHydraNode aliceSk [] chain $ \n1 -> do
                  openHead chain n1
                  deadlineInThePast <- addUTCTime (getNegative seconds) <$> getCurrentTime
                  txid <- simulateDeposit chain testHeadId (utxoRef 123) deadlineInThePast
                  asExpected <- waitUntilMatch [n1] $ \case
                    DepositExpired{depositTxId} -> True <$ guard (depositTxId == txid)
                    CommitApproved{} -> Just False
                    _ -> Nothing
                  pure $
                    asExpected
                      & counterexample "Deposit with deadline in the past approved instead of expired"

        it "deposits with deadline too soon are ignored" $ do
          let depositPeriod = DP.toNominalDiffTime defaultDepositPeriod
          -- NOTE: Any deadline between now and deposit period should
          -- eventually result in an expired deposit.
          forAll (chooseEnum (0, depositPeriod)) $ \deadlineDiff ->
            propRunInSim $
              withSimulatedChainAndNetwork $ \chain ->
                withHydraNode aliceSk [] chain $ \n1 -> do
                  openHead chain n1
                  deadlineTooEarly <- addUTCTime deadlineDiff <$> getCurrentTime
                  txid <- simulateDeposit chain testHeadId (utxoRef 123) deadlineTooEarly
                  asExpected <- waitUntilMatch [n1] $ \case
                    DepositExpired{depositTxId} -> True <$ guard (depositTxId == txid)
                    CommitApproved{} -> Just False
                    _ -> Nothing
                  pure $
                    asExpected
                      & counterexample "Deposit with deadline too soon approved instead of expired"
                      & counterexample ("Deadline: " <> show deadlineTooEarly)

        it "commit snapshot only approved when deadline not too soon" $ do
          shouldRunInSim $
            withSimulatedChainAndNetwork $ \chain -> do
              let dpShort = DepositPeriod 60
              let dpLong = DepositPeriod 3600
              withHydraNode' dpShort aliceSk [bob] chain $ \n1 ->
                withHydraNode' dpLong bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  -- NOTE: We use a deadline that is okay for alice, but too soon for bob.
                  deadline <- addUTCTime 600 <$> getCurrentTime
                  txid <- simulateDeposit chain testHeadId (utxoRef 123) deadline
                  waitUntilMatch [n1, n2] $ \case
                    CommitRecorded{utxoToCommit} ->
                      guard (123 `member` utxoToCommit)
                    _ -> Nothing
                  waitUntilMatch [n1, n2] $ \case
                    DepositExpired{depositTxId} -> guard $ depositTxId == txid
                    _ -> Nothing

        it "deposits are only processed after settled" $ do
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [] chain $ \n1 -> do
                openHead chain n1
                deadline <- newDeadlineFarEnoughFromNow
                depositTxId <- simulateDeposit chain testHeadId (utxoRef 123) deadline
                waitUntilMatch [n1] $ \case
                  CommitRecorded{pendingDeposit} -> guard (pendingDeposit == depositTxId)
                  _ -> Nothing
                -- No approval yet, as the deposit is not settled
                let waitForApproval = waitUntilMatch [n1] $ \case
                      CommitApproved{utxoToCommit} -> guard (utxoToCommit == utxoRef 123)
                      _ -> Nothing
                timeout (fromIntegral defaultDepositPeriod) waitForApproval >>= \case
                  Nothing -> pure ()
                  Just _ -> failure "Deposit was approved before deadline expired"
                -- Now it should get approved
                waitForApproval

        it "commit snapshot only approved when deposit settled" $
          shouldRunInSim $
            withSimulatedChainAndNetwork $ \chain -> do
              -- NOTE: Only a maximum difference of 600 seconds is handled by the HeadLogic. See
              -- https://hydra.family/head-protocol/unstable/docs/known-issues#deposit-periods
              let dpShort = DepositPeriod 60
              let dpLong = DepositPeriod 600
              withHydraNode' dpShort aliceSk [bob] chain $ \n1 ->
                withHydraNode' dpLong bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  deadline <- newDeadlineFarEnoughFromNow
                  txid <- simulateDeposit chain testHeadId (utxoRef 123) deadline
                  waitUntilMatch [n1, n2] $ \case
                    CommitRecorded{pendingDeposit} -> guard (pendingDeposit == txid)
                    _ -> Nothing
                  -- No approval yet, as the deposit is not settled
                  let waitForApproval = waitUntilMatch [n1, n2] $ \case
                        CommitApproved{utxoToCommit} -> guard (utxoToCommit == utxoRef 123)
                        _ -> Nothing
                  timeout (fromIntegral dpLong) waitForApproval >>= \case
                    Nothing -> pure ()
                    Just _ -> failure "Deposit was approved before all deposit periods passed"
                  -- Now it should get approved
                  waitForApproval

        it "requested commits get approved" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  deadline <- newDeadlineFarEnoughFromNow
                  depositTxId <- simulateDeposit chain testHeadId depositUTxO deadline
                  waitUntil [n1, n2] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = depositTxId, deadline}

                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                      utxoToCommit >>= guard . (11 `member`)
                    _ -> Nothing

                  waitUntil [n1] $ CommitApproved{headId = testHeadId, utxoToCommit = depositUTxO}
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId}

                  headUTxO <- getHeadUTxO . headState <$> queryState n1
                  fromMaybe mempty headUTxO `shouldSatisfy` member 11

        it "can process multiple commits" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let depositUTxO2 = utxoRefs [22]
                  deadline <- newDeadlineFarEnoughFromNow
                  deposit1 <- simulateDeposit chain testHeadId depositUTxO deadline
                  deposit2 <- simulateDeposit chain testHeadId depositUTxO2 deadline
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = deposit1, deadline}
                  waitUntil [n2] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO2, pendingDeposit = deposit2, deadline}
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                      utxoToCommit >>= guard . (11 `member`)
                    _ -> Nothing

                  waitUntil [n1] $ CommitApproved{headId = testHeadId, utxoToCommit = depositUTxO}
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId = deposit1}
                  let normalTx = SimpleTx 3 (utxoRef 2) (utxoRef 3)
                  send n2 (NewTx normalTx)
                  waitUntil [n1, n2] $ TxValid testHeadId 3
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                      utxoToCommit >>= guard . (22 `member`)
                    _ -> Nothing
                  waitUntil [n2] $ CommitApproved{headId = testHeadId, utxoToCommit = depositUTxO2}
                  waitUntil [n2] $ CommitFinalized{headId = testHeadId, depositTxId = deposit2}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 3, 11, 22]}

        it "can process transactions while commit pending" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  deadline <- newDeadlineFarEnoughFromNow
                  depositTxId <- simulateDeposit chain testHeadId depositUTxO deadline
                  waitUntilMatch [n1, n2] $ \case
                    CommitRecorded{utxoToCommit} ->
                      guard (11 `member` utxoToCommit)
                    _ -> Nothing
                  let normalTx = SimpleTx 2 (utxoRef 2) (utxoRef 3)
                  send n2 (NewTx normalTx)
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{confirmed}} -> guard $ normalTx `elem` confirmed
                    _ -> Nothing
                  waitUntil [n1, n2] $ CommitFinalized{headId = testHeadId, depositTxId}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 3, 11]}

        -- XXX: This could be a single node test
        it "can close with commit in flight" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  deadline <- newDeadlineFarEnoughFromNow
                  depositTxId <- simulateDeposit chain testHeadId depositUTxO deadline
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = depositTxId, deadline}
                  waitUntilMatch [n1] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                      utxoToCommit >>= guard . (11 `member`)
                    _ -> Nothing

                  send n1 Close
                  waitUntilMatch [n1, n2] $ \case
                    HeadIsClosed{snapshotNumber} -> guard $ snapshotNumber == 1
                    _ -> Nothing
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2, 11]}

        -- XXX: This could be a single node test
        it "fanout utxo is correct after a commit" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  deadline <- newDeadlineFarEnoughFromNow
                  depositTxId <- simulateDeposit chain testHeadId depositUTxO deadline
                  waitUntil [n2] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = depositTxId, deadline}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2]}

        -- XXX: This could be a single node test
        it "multiple commits and decommits in sequence" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  deadline <- newDeadlineFarEnoughFromNow
                  depositTxId <- simulateDeposit chain testHeadId depositUTxO deadline
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = depositTxId, deadline}
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                      utxoToCommit >>= guard . (11 `member`)
                    _ -> Nothing
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId}

                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  send n2 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}

                  waitUntilMatch [n1] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToDecommit}} ->
                      utxoToDecommit >>= guard . (42 `member`)
                    _ -> Nothing

                  waitUntil [n1, n2] $ DecommitApproved{headId = testHeadId, decommitTxId = txId decommitTx, utxoToDecommit = utxoRef 42}
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, distributedUTxO = utxoRef 42}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [2, 11]}

        it "commit and decommit same utxo" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  deadline <- newDeadlineFarEnoughFromNow
                  depositTxId <- simulateDeposit chain testHeadId depositUTxO deadline
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = depositTxId, deadline}
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                      utxoToCommit >>= guard . (11 `member`)
                    _ -> Nothing
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId}

                  headUTxO <- getHeadUTxO . headState <$> queryState n1
                  fromMaybe mempty headUTxO `shouldBe` utxoRefs [1, 2, 11]

                  let decommitTx = SimpleTx 1 (utxoRef 11) (utxoRef 88)
                  send n2 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [88]}
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToDecommit}} ->
                      utxoToDecommit >>= guard . (88 `member`)
                    _ -> Nothing

                  waitUntil [n1, n2] $ DecommitApproved{headId = testHeadId, decommitTxId = txId decommitTx, utxoToDecommit = utxoRefs [88]}
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, distributedUTxO = utxoRef 88}

                  headUTxO2 <- getHeadUTxO . headState <$> queryState n1
                  fromMaybe mempty headUTxO2 `shouldSatisfy` (not . member 11)

      describe "Decommit" $ do
        it "can request decommit" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2

                  let decommitTx = aValidTx 42
                  send n1 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}

        it "requested decommits get approved" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  send n2 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}

                  waitUntilMatch [n1] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{utxoToDecommit}} ->
                      utxoToDecommit >>= guard . (42 `member`)
                    _ -> Nothing

                  waitUntil [n1, n2] $ DecommitApproved{headId = testHeadId, decommitTxId = txId decommitTx, utxoToDecommit = utxoRefs [42]}
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, distributedUTxO = utxoRef 42}

                  headUTxO <- getHeadUTxO . headState <$> queryState n1
                  fromMaybe mempty headUTxO `shouldSatisfy` (not . member 42)

        it "can only process one decommit at once" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let decommitTx1 = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  send n1 (Decommit{decommitTx = decommitTx1})
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx = decommitTx1, utxoToDecommit = utxoRefs [42]}

                  let decommitTx2 = SimpleTx 2 (utxoRef 2) (utxoRef 22)
                  send n2 (Decommit{decommitTx = decommitTx2})
                  waitUntil [n2] $
                    DecommitInvalid
                      { headId = testHeadId
                      , decommitTx = decommitTx2
                      , decommitInvalidReason = DecommitAlreadyInFlight{otherDecommitTxId = txId decommitTx1}
                      }

                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, distributedUTxO = utxoRef 42}

                  send n2 (Decommit{decommitTx = decommitTx2})
                  waitUntil [n1, n2] $ DecommitApproved{headId = testHeadId, decommitTxId = txId decommitTx2, utxoToDecommit = utxoRefs [22]}
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, distributedUTxO = utxoRef 22}

        it "can process transactions while decommit pending" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2

                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  send n2 (Decommit{decommitTx})
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}
                  waitUntil [n1, n2] $
                    DecommitApproved{headId = testHeadId, decommitTxId = 1, utxoToDecommit = utxoRefs [42]}

                  let normalTx = SimpleTx 2 (utxoRef 2) (utxoRef 3)
                  send n2 (NewTx normalTx)
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{confirmed}} -> guard $ normalTx `elem` confirmed
                    _ -> Nothing

                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, distributedUTxO = utxoRef 42}

        it "can fanout with decommit in flight" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 2) (utxoRef 42)
                  send n2 (Decommit{decommitTx})
                  -- Close while the decommit is still in flight
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n1 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 42]}

        it "can fanout after a decommit" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 2) (utxoRef 42)
                  send n2 (Decommit{decommitTx})
                  waitUntil [n1, n2] $
                    DecommitApproved
                      { headId = testHeadId
                      , decommitTxId = txId decommitTx
                      , utxoToDecommit = utxoRefs [42]
                      }
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n1 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1]}

        it "can fanout with empty utxo" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead2 chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  send n2 (Decommit{decommitTx})
                  waitUntil [n1, n2] $
                    DecommitApproved
                      { headId = testHeadId
                      , decommitTxId = txId decommitTx
                      , utxoToDecommit = utxoRefs [42]
                      }
                  waitUntil [n1, n2] $
                    DecommitFinalized
                      { headId = testHeadId
                      , distributedUTxO = utxoRef 42
                      }
                  let decommitTx2 = SimpleTx 2 (utxoRef 2) (utxoRef 88)
                  send n1 (Decommit{decommitTx = decommitTx2})
                  waitUntil [n1, n2] $
                    DecommitFinalized
                      { headId = testHeadId
                      , distributedUTxO = utxoRef 88
                      }
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n1 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs []}

    it "can be finalized by all parties after contestation period" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead2 chain n1 n2
              send n1 Close
              forM_ [n1, n2] $ waitForNext >=> assertHeadIsClosed
              waitUntil [n1, n2] $ ReadyToFanout testHeadId
              send n1 Fanout
              send n2 Fanout
              waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2]}

    it "contest automatically when detecting closing with old snapshot" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead2 chain n1 n2

              -- Perform a transaction to produce the latest snapshot, number 1
              let tx = aValidTx 42
              send n2 (NewTx tx)
              waitUntilMatch [n1, n2] $ \case
                SnapshotConfirmed{snapshot = Snapshot{number}} -> guard $ number == 1
                _ -> Nothing

              -- Have n1 & n2 observe a close with not the latest snapshot
              let deadline = arbitrary `generateWith` 42
              -- XXX: This is a bit cumbersome and maybe even incorrect (chain
              -- states), the simulated chain should provide a way to inject an
              -- 'OnChainTx' without providing a chain state?
              injectChainEvent n1 Observation{observedTx = OnCloseTx testHeadId 0 deadline, newChainState = SimpleChainState{slot = ChainSlot 0}}
              injectChainEvent n2 Observation{observedTx = OnCloseTx testHeadId 0 deadline, newChainState = SimpleChainState{slot = ChainSlot 0}}

              waitUntilMatch [n1, n2] $ \case
                HeadIsClosed{snapshotNumber} -> guard $ snapshotNumber == 0
                _ -> Nothing

              -- Expect n1 to contest with latest snapshot, number 1
              waitUntilMatch [n1, n2] $ \case
                HeadIsContested{snapshotNumber} -> guard $ snapshotNumber == 1
                _ -> Nothing

  describe "Hydra Node Logging" $ do
    it "traces processing of events" $ do
      let result = runSimTrace $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [] chain $ \n1 -> do
                send n1 Init
                waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
                simulateCommit chain testHeadId alice (utxoRef 1)

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs
        `shouldContain` [BeginInput alice 1 (ClientInput Init)]
      logs
        `shouldContain` [EndInput alice 1]

    it "traces handling of effects" $ do
      let result = runSimTrace $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [] chain $ \n1 -> do
                send n1 Abort
                msg <- waitForNextMessage n1
                msg `shouldSatisfy` \case
                  CommandFailed{} -> True
                  _ -> False

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs
        `shouldSatisfy` any
          ( \case
              (BeginEffect _ _ _ (ClientEffect CommandFailed{})) -> True
              _ -> False
          )
      logs `shouldContain` [EndEffect alice 1 0]

  describe "rolling back & forward does not make the node crash" $ do
    it "does work for rollbacks past init" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            -- We expect the Init to be rolled back and forward again
            rollbackAndForward chain 1
            -- We expect the node to still work and let us commit
            simulateCommit chain testHeadId alice (utxoRef 1)
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)

    it "does work for rollbacks past open" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            simulateCommit chain testHeadId alice (utxoRef 1)
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1]}
            -- We expect one Commit AND the CollectCom to be rolled back and
            -- forward again
            rollbackAndForward chain 2
            -- We expect the node to still work and let us post L2 transactions
            send n1 (NewTx (aValidTx 42))
            waitUntil [n1] $ TxValid testHeadId 42

-- | Wait for some output at some node(s) to be produced /eventually/. See
-- 'waitUntilMatch' for how long it waits.
waitUntil ::
  (HasCallStack, MonadThrow m, MonadAsync m, MonadTimer m, MonadLabelledSTM m, IsChainState tx) =>
  [TestHydraClient tx m] ->
  ServerOutput tx ->
  m ()
waitUntil nodes expected =
  waitUntilMatch nodes $ guard . (expected ==)

-- | Wait for a server output to match some predicate /eventually/. If a client
-- message is received instead, this fails. This will not wait forever, but for
-- a long time (1 month) to get a nice error location. Should not be an issue
-- when used within `shouldRunInSim`, this was even 1000 years before - but we
-- since we are having the protocol produce 'Tick' events constantly this would
-- be fully simulated to the end.
waitUntilMatch ::
  (Show (ServerOutput tx), HasCallStack, MonadThrow m, MonadAsync m, MonadTimer m, MonadLabelledSTM m, Eq a, Show a, IsChainState tx) =>
  [TestHydraClient tx m] ->
  (ServerOutput tx -> Maybe a) ->
  m a
waitUntilMatch nodes predicate = do
  seenMsgs <- newLabelledTVarIO "wait-until-seen-msgs" []
  timeout oneMonth (forConcurrently (zip [Node 1 ..] nodes) $ go seenMsgs) >>= \case
    Just [] -> failure "waitUntilMatch no results"
    Just (x : xs)
      | all (== x) xs -> pure x
      | otherwise -> failure . toString $ unlines ["waitUntilMatch encountered inconsistent results:", show (x : xs)]
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "waitUntilMatch did not match a message on all nodes (" <> show (length nodes) <> ") within " <> show oneMonth <> ", seen messages:"
            , unlines (show <$> msgs)
            ]
 where
  go seenOutputs (nid, n) = do
    out <-
      raceLabelled ("wait-for-next-msg", waitForNextMessage n) ("wait-for-next", waitForNext n) >>= \case
        Left msg -> failure $ "waitUntilMatch received unexpected client message: " <> show msg
        Right out -> pure out
    atomically (modifyTVar' seenOutputs ((nid, out) :))
    case predicate out of
      Just x -> pure x
      Nothing -> go seenOutputs (nid, n)

  oneMonth = 3600 * 24 * 30

newtype NodeId = Node Natural
  deriving (Enum, Show)

-- XXX: The names of the following handles and functions are confusing.

-- | A thin client layer around 'HydraNode' to be interact with it through
-- 'send', 'waitForNext', access all outputs and inject events through the test
-- chain.
data TestHydraClient tx m = TestHydraClient
  { send :: ClientInput tx -> m ()
  , waitForNext :: m (ServerOutput tx)
  , waitForNextMessage :: m (ClientMessage tx)
  , injectChainEvent :: ChainEvent tx -> m ()
  , serverOutputs :: m [ServerOutput tx]
  , queryState :: m (NodeState tx)
  }

-- | A simulated chain that just echoes 'PostChainTx' as 'Observation's of
-- 'OnChainTx' onto all connected nodes. It can also 'rollbackAndForward' any
-- number of these "transactions".
data SimulatedChainNetwork tx m = SimulatedChainNetwork
  { connectNode :: DraftHydraNode tx m -> m (HydraNode tx m)
  , tickThread :: Async m ()
  , rollbackAndForward :: Natural -> m ()
  , simulateCommit :: HeadId -> Party -> UTxOType tx -> m ()
  , simulateDeposit :: HeadId -> UTxOType tx -> UTCTime -> m (TxIdType tx)
  , closeWithInitialSnapshot :: (Party, UTxOType tx) -> m ()
  }

dummySimulatedChainNetwork :: SimulatedChainNetwork tx m
dummySimulatedChainNetwork =
  SimulatedChainNetwork
    { connectNode = error "connectNode"
    , tickThread = error "tickThread"
    , rollbackAndForward = error "rollbackAndForward"
    , simulateCommit = error "simulateCommit"
    , simulateDeposit = error "simulateDeposit"
    , closeWithInitialSnapshot = error "closeWithInitialSnapshot"
    }

-- | With-pattern wrapper around 'simulatedChainAndNetwork' which does 'cancel'
-- the 'tickThread'. Also, this will fix tx to 'SimpleTx' so that it can pick an
-- initial chain state to play back to our test nodes.
-- NOTE: The simulated network has a block time of 20 (simulated) seconds.
withSimulatedChainAndNetwork ::
  (MonadTime m, MonadDelay m, MonadAsync m, MonadThrow m, MonadLabelledSTM m) =>
  (SimulatedChainNetwork SimpleTx m -> m a) ->
  m a
withSimulatedChainAndNetwork =
  bracket
    (simulatedChainAndNetwork SimpleChainState{slot = ChainSlot 0})
    (cancel . tickThread)

-- | Creates a simulated chain and network to which 'HydraNode's can be
-- connected to using 'connectNode'. NOTE: The 'tickThread' needs to be
-- 'cancel'ed after use. Use 'withSimulatedChainAndNetwork' instead where
-- possible.
simulatedChainAndNetwork ::
  forall m.
  (MonadTime m, MonadDelay m, MonadAsync m, MonadLabelledSTM m) =>
  ChainStateType SimpleTx ->
  m (SimulatedChainNetwork SimpleTx m)
simulatedChainAndNetwork initialChainState = do
  history <- newLabelledTVarIO "sim-chain-history" []
  nodes <- newLabelledTVarIO "sim-chain-nodes" []
  nextTxId <- newLabelledTVarIO "sim-chain-next-txid" 10000
  localChainState <- newLocalChainState (initHistory initialChainState)
  tickThread <- asyncLabelled "sim-chain-tick" $ simulateTicks nodes localChainState
  pure $
    SimulatedChainNetwork
      { connectNode = \draftNode -> do
          let mockChain =
                Chain
                  { postTx = \tx -> do
                      now <- getCurrentTime
                      -- Only observe "after one block"
                      void . asyncLabelled "sim-chain-post-tx" $ do
                        threadDelay blockTime
                        createAndYieldEvent nodes history localChainState $ toOnChainTx now tx
                  , draftCommitTx = \_ -> error "unexpected call to draftCommitTx"
                  , draftDepositTx = \_ -> error "unexpected call to draftDepositTx"
                  , submitTx = \_ -> error "unexpected call to submitTx"
                  , checkNonADAAssets = \_ -> error "unexpected call to checkNonADAAssets"
                  }
              mockNetwork = createMockNetwork draftNode nodes
              mockServer :: Server tx m
              mockServer = Server{sendMessage = const $ pure ()}
          node <- connect mockChain mockNetwork mockServer draftNode
          atomically $ modifyTVar nodes (node :)
          pure node
      , tickThread
      , rollbackAndForward = rollbackAndForward nodes history localChainState
      , simulateCommit = \headId party toCommit ->
          createAndYieldEvent nodes history localChainState $ OnCommitTx{headId, party, committed = toCommit}
      , simulateDeposit = \headId toDeposit deadline -> do
          created <- getCurrentTime
          depositTxId <- atomically $ stateTVar nextTxId (\i -> (i, i + 1))
          createAndYieldEvent nodes history localChainState $
            OnDepositTx{headId, deposited = toDeposit, created, deadline, depositTxId}
          pure depositTxId
      , closeWithInitialSnapshot = error "unexpected call to closeWithInitialSnapshot"
      }
 where
  -- seconds
  blockTime = 20

  simulateTicks nodes localChainState = forever $ do
    threadDelay blockTime
    now <- getCurrentTime
    event <- atomically $ do
      cs <- getLatest localChainState
      -- XXX: This chain state (its point) does not correspond to 'now'
      pure $ Tick now (chainStatePoint cs)
    readTVarIO nodes >>= mapM_ (`handleChainEvent` event)

  createAndYieldEvent nodes history localChainState tx = do
    chainEvent <- atomically $ do
      cs <- getLatest localChainState
      let cs' = advanceSlot cs
      pushNew localChainState cs'
      pure $
        Observation
          { observedTx = tx
          , newChainState = cs'
          }
    recordAndYieldEvent nodes history chainEvent

  advanceSlot SimpleChainState{slot} = SimpleChainState{slot = nextChainSlot slot}

  recordAndYieldEvent ::
    TVar m [HydraNode tx m] ->
    TVar m [ChainEvent tx] ->
    ChainEvent tx ->
    m ()
  recordAndYieldEvent nodes history chainEvent = do
    ns <- atomically $ do
      modifyTVar' history (chainEvent :)
      readTVar nodes
    forM_ ns $ \n ->
      handleChainEvent n chainEvent

  rollbackAndForward ::
    IsChainState tx =>
    TVar m [HydraNode tx m] ->
    TVar m [ChainEvent tx] ->
    LocalChainState m tx ->
    Natural ->
    m ()
  rollbackAndForward nodes history localChainState steps = do
    -- Split the history after given steps
    (toReplay, kept) <- atomically $ do
      (toReplay, kept) <- splitAt (fromIntegral steps) <$> readTVar history
      writeTVar history kept
      pure (reverse toReplay, kept)
    -- Determine the new (last kept one) chainstate
    let chainSlot =
          List.head $
            map
              ( \case
                  Observation{newChainState} -> chainStateSlot newChainState
                  _NoObservation -> error "unexpected non-observation ChainEvent"
              )
              kept
    rolledBackChainState <- atomically $ rollback localChainState chainSlot
    -- Yield rollback events
    ns <- readTVarIO nodes
    chainTime <- getCurrentTime
    forM_ ns $ \n -> handleChainEvent n Rollback{chainTime, rolledBackChainState}
    -- Re-play the observation events
    forM_ toReplay $ \ev ->
      recordAndYieldEvent nodes history ev

handleChainEvent :: HydraNode tx m -> ChainEvent tx -> m ()
handleChainEvent HydraNode{inputQueue} = enqueue inputQueue . ChainInput

createMockNetwork :: MonadSTM m => DraftHydraNode tx m -> TVar m [HydraNode tx m] -> Network m (Message tx)
createMockNetwork node nodes =
  Network{broadcast}
 where
  broadcast msg = do
    allNodes <- readTVarIO nodes
    mapM_ (`handleMessage` msg) allNodes

  handleMessage HydraNode{inputQueue} msg =
    enqueue inputQueue $ mkNetworkInput sender msg

  sender = getParty node

-- | Derive an 'OnChainTx' from 'PostChainTx' to simulate a "perfect" chain.
-- NOTE: This implementation announces hard-coded contestationDeadlines. Also,
-- all heads will have the same 'headId' and 'headSeed'.
toOnChainTx :: IsTx tx => UTCTime -> PostChainTx tx -> OnChainTx tx
toOnChainTx now = \case
  InitTx{participants, headParameters} ->
    OnInitTx{headId = testHeadId, headSeed = testHeadSeed, headParameters, participants}
  AbortTx{} ->
    OnAbortTx{headId = testHeadId}
  CollectComTx{headId} ->
    OnCollectComTx{headId}
  RecoverTx{headId, recoverTxId, recoverUTxO} ->
    OnRecoverTx{headId, recoveredTxId = recoverTxId, recoveredUTxO = recoverUTxO}
  IncrementTx{headId, incrementingSnapshot, depositTxId} ->
    OnIncrementTx
      { headId
      , newVersion = version + 1
      , depositTxId
      }
   where
    Snapshot{version} = getSnapshot incrementingSnapshot
  DecrementTx{headId, decrementingSnapshot} ->
    OnDecrementTx
      { headId
      , newVersion = version + 1
      , distributedUTxO = fromMaybe mempty utxoToDecommit
      }
   where
    Snapshot{version, utxoToDecommit} = getSnapshot decrementingSnapshot
  CloseTx{closingSnapshot} ->
    OnCloseTx
      { headId = testHeadId
      , snapshotNumber = number (getSnapshot closingSnapshot)
      , contestationDeadline = addUTCTime (CP.toNominalDiffTime defaultContestationPeriod) now
      }
  ContestTx{headId, contestingSnapshot} ->
    OnContestTx
      { headId
      , snapshotNumber = number (getSnapshot contestingSnapshot)
      , contestationDeadline = addUTCTime (CP.toNominalDiffTime defaultContestationPeriod) now
      }
  FanoutTx{utxo, utxoToCommit, utxoToDecommit} ->
    OnFanoutTx{headId = testHeadId, fanoutUTxO = utxo <> fromMaybe mempty utxoToCommit <> fromMaybe mempty utxoToDecommit}

newDeadlineFarEnoughFromNow :: MonadTime m => m UTCTime
newDeadlineFarEnoughFromNow =
  addUTCTime (3 * DP.toNominalDiffTime defaultDepositPeriod) <$> getCurrentTime

nothingHappensFor ::
  (MonadTimer m, MonadThrow m, IsChainState tx) =>
  TestHydraClient tx m ->
  NominalDiffTime ->
  m ()
nothingHappensFor node secs =
  timeout (realToFrac secs) (waitForNext node) >>= (`shouldBe` Nothing)

withHydraNode ::
  forall s a.
  SigningKey HydraKey ->
  [Party] ->
  SimulatedChainNetwork SimpleTx (IOSim s) ->
  (TestHydraClient SimpleTx (IOSim s) -> IOSim s a) ->
  IOSim s a
withHydraNode signingKey otherParties chain action = do
  withHydraNode' defaultDepositPeriod signingKey otherParties chain action

withHydraNode' ::
  DepositPeriod ->
  SigningKey HydraKey ->
  [Party] ->
  SimulatedChainNetwork SimpleTx (IOSim s) ->
  (TestHydraClient SimpleTx (IOSim s) -> IOSim s b) ->
  IOSim s b
withHydraNode' dp signingKey otherParties chain action = do
  outputs <- newLabelledTQueueIO "hydra-node-outputs"
  messages <- newLabelledTQueueIO "hydra-node-messages"
  outputHistory <- newLabelledTVarIO "hydra-node-output-history" mempty
  let initialChainState = SimpleChainState{slot = ChainSlot 0}
  node@HydraNode{nodeStateHandler = NodeStateHandler{queryNodeState}} <-
    createHydraNode
      traceInIOSim
      simpleLedger
      initialChainState
      signingKey
      otherParties
      outputs
      messages
      outputHistory
      chain
      defaultContestationPeriod
      dp
  withAsyncLabelled ("run-hydra-node", runHydraNode node) $ \_ -> do
    -- await for the node to be in sync with the chain
    atomically $ do
      st <- queryNodeState
      case st of
        NodeInSync{} -> pure ()
        _ -> retry
    action (createTestHydraClient outputs messages outputHistory node)

createTestHydraClient ::
  MonadSTM m =>
  TQueue m (ServerOutput tx) ->
  TQueue m (ClientMessage tx) ->
  TVar m [ServerOutput tx] ->
  HydraNode tx m ->
  TestHydraClient tx m
createTestHydraClient outputs messages outputHistory HydraNode{inputQueue, nodeStateHandler} =
  TestHydraClient
    { send = enqueue inputQueue . ClientInput
    , waitForNext = atomically (readTQueue outputs)
    , waitForNextMessage = atomically (readTQueue messages)
    , injectChainEvent = enqueue inputQueue . ChainInput
    , serverOutputs = reverse <$> readTVarIO outputHistory
    , queryState = atomically (queryNodeState nodeStateHandler)
    }

createHydraNode ::
  (IsChainState tx, MonadDelay m, MonadAsync m, MonadLabelledSTM m, MonadThrow m) =>
  Tracer m (HydraNodeLog tx) ->
  Ledger tx ->
  ChainStateType tx ->
  SigningKey HydraKey ->
  [Party] ->
  TQueue m (ServerOutput tx) ->
  TQueue m (ClientMessage tx) ->
  TVar m [ServerOutput tx] ->
  SimulatedChainNetwork tx m ->
  ContestationPeriod ->
  DepositPeriod ->
  m (HydraNode tx m)
createHydraNode tracer ledger chainState signingKey otherParties outputs messages outputHistory chain cp dp = do
  EventStore{eventSource, eventSink} <- createMockEventStore
  let apiSink =
        EventSink
          { putEvent = \event ->
              case mkTimedServerOutputFromStateEvent event of
                Nothing -> pure ()
                Just TimedServerOutput{output} -> atomically $ do
                  writeTQueue outputs output
                  modifyTVar' outputHistory (output :)
          }
  -- NOTE: Not using 'hydrate' as we don't want to run the event source conduit.
  let nodeState = initNodeState chainState
  let chainStateHistory = initHistory chainState
  nodeStateHandler <- createNodeStateHandler Nothing nodeState
  inputQueue <- createInputQueue
  node <-
    connectNode
      chain
      DraftHydraNode
        { tracer
        , env
        , ledger
        , nodeStateHandler
        , inputQueue
        , eventSource
        , eventSinks = [apiSink, eventSink]
        , chainStateHistory
        }
  pure $
    node
      { server =
          Server
            { sendMessage = atomically . writeTQueue messages
            }
      }
 where
  env =
    Environment
      { party
      , signingKey
      , otherParties
      , contestationPeriod = cp
      , depositPeriod = dp
      , unsyncedPeriod = defaultUnsyncedPeriodFor cp
      , participants
      , configuredPeers = ""
      }
  party = deriveParty signingKey

  -- NOTE: We use the hydra-keys as on-chain identities directly. This is fine
  -- as this is a simulated network.
  participants = deriveOnChainId <$> (party : otherParties)

openHead ::
  SimulatedChainNetwork SimpleTx (IOSim s) ->
  TestHydraClient SimpleTx (IOSim s) ->
  IOSim s ()
openHead chain n1 = do
  send n1 Init
  waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
  simulateCommit chain testHeadId alice (utxoRef 1)
  waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1]}

openHead2 ::
  SimulatedChainNetwork SimpleTx (IOSim s) ->
  TestHydraClient SimpleTx (IOSim s) ->
  TestHydraClient SimpleTx (IOSim s) ->
  IOSim s ()
openHead2 chain n1 n2 = do
  send n1 Init
  waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
  simulateCommit chain testHeadId alice (utxoRef 1)
  waitUntil [n1, n2] $ Committed testHeadId alice (utxoRef 1)
  simulateCommit chain testHeadId bob (utxoRef 2)
  waitUntil [n1, n2] $ Committed testHeadId bob (utxoRef 2)
  waitUntil [n1, n2] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1, 2]}

assertHeadIsClosed :: (HasCallStack, MonadThrow m) => ServerOutput tx -> m ()
assertHeadIsClosed = \case
  HeadIsClosed{} -> pure ()
  _ -> failure "expected HeadIsClosed"

assertHeadIsClosedWith :: (HasCallStack, MonadThrow m) => SnapshotNumber -> ServerOutput tx -> m ()
assertHeadIsClosedWith expectedSnapshotNumber = \case
  HeadIsClosed{snapshotNumber} -> do
    snapshotNumber `shouldBe` expectedSnapshotNumber
  _ -> failure "expected HeadIsClosed"

-- | Provide a quick and dirty to way to label stuff from a signing key
shortLabel :: SigningKey HydraKey -> String
shortLabel s =
  take 8 $ drop 1 $ List.words (show s) !! 2

-- | Get the head 'UTxO' from open 'HeadState'.
getHeadUTxO :: IsTx tx => HeadState tx -> Maybe (UTxOType tx)
getHeadUTxO = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{localUTxO}} -> Just localUTxO
  Initial InitialState{committed} -> Just $ fold committed
  _ -> Nothing
