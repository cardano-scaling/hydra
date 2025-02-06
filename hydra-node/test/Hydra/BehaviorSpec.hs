{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.BehaviorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (shouldBe, shouldNotBe, shouldReturn, shouldSatisfy)

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  modifyTVar,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  readTVarIO,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (Async, MonadAsync (async), cancel, forConcurrently_)
import Control.Monad.IOSim (IOSim, runSimTrace, selectTraceEventsDynamic)
import Data.List ((!!))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Hydra.API.ClientInput
import Hydra.API.Server (Server (..))
import Hydra.HeadLogic.Outcome (DecommitInvalidReason (..), StateChanged (..))
import Hydra.Cardano.Api (SigningKey)
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  initHistory,
 )
import Hydra.Chain.ChainState (ChainSlot (ChainSlot), ChainStateType, IsChainState, chainStateSlot)
import Hydra.Chain.Direct.Handlers (getLatest, newLocalChainState, pushNew, rollback)
import Hydra.HeadLogic (Effect (..), HeadState (..), IdleState (..), Input (..), defaultTTL)
import Hydra.HeadLogicSpec (testSnapshot)
import Hydra.Ledger (Ledger, nextChainSlot)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (Tracer)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message, NetworkEvent (..))
import Hydra.Node (DraftHydraNode (..), HydraNode (..), HydraNodeLog (..), connect, createNodeState, queryHeadState, runHydraNode, waitDelay)
import Hydra.Node.InputQueue (InputQueue (enqueue), createInputQueue)
import Hydra.NodeSpec (createMockSourceSink)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod), toNominalDiffTime)
import Hydra.Tx.Crypto (HydraKey, aggregate, sign)
import Hydra.Tx.DepositDeadline (DepositDeadline (UnsafeDepositDeadline))
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
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
import Test.Util (shouldBe, shouldNotBe, shouldRunInSim, traceInIOSim)

testHeadParameters :: HeadParameters
testHeadParameters =
  HeadParameters
    { contestationPeriod = testContestationPeriod
    , parties = [alice]
    }

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
            waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}
            simulateCommit chain (alice, utxoRef 1)
            waitUntil [n1] $ Committed{headId = testHeadId, party = alice, chainState = SimpleChainState 2, utxo = utxoRef 1}

    it "can close an open head" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}
            simulateCommit chain (alice, utxoRef 1)
            waitUntil [n1] $ Committed{headId = testHeadId, party = alice, chainState = SimpleChainState 2, utxo = utxoRef 1}
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, initialUTxO = utxoRef 1, chainState = SimpleChainState 3}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed

    it "does not fanout automatically" $ do
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}
            simulateCommit chain (alice, utxoRef 1)
            waitUntil [n1] $ Committed{headId = testHeadId, party = alice, utxo = utxoRef 1, chainState = SimpleChainState 2}
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, initialUTxO = utxoRef 1, chainState = SimpleChainState 3}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed
            waitUntil [n1] $ ReadyToFanout testHeadId
            nothingHappensFor n1 1000000

    it "does finalize head after contestation period upon command" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}
            simulateCommit chain (alice, utxoRef 1)
            waitUntil [n1] $ Committed{headId = testHeadId, party = alice, utxo = utxoRef 1, chainState = SimpleChainState 2}
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, initialUTxO = utxoRef 1, chainState = SimpleChainState 3}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed
            waitUntil [n1] $ ReadyToFanout testHeadId
            send n1 Fanout
            waitUntil [n1] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRef 1, chainState = SimpleChainState 5}

  describe "Two participant Head" $ do
    it "only opens the head after all nodes committed" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              let parameters = HeadParameters{contestationPeriod = testContestationPeriod, parties = [alice, bob]}
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice, bob], parameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}

              simulateCommit chain (alice, utxoRef 1)
              waitUntil [n1] $ Committed{headId = testHeadId, party = alice, utxo = utxoRef 1, chainState = SimpleChainState 2}
              let veryLong = timeout 1000000
              veryLong (waitForNext n1) >>= (`shouldNotBe` Just HeadIsOpen{headId = testHeadId, initialUTxO = utxoRef 1, chainState = SimpleChainState 3})

              simulateCommit chain (bob, utxoRef 2)
              waitUntil [n1] $ Committed{headId = testHeadId, party = bob, utxo = utxoRef 2, chainState = SimpleChainState 3}
              waitUntil [n1] $ HeadIsOpen{headId = testHeadId, initialUTxO = utxoRefs [1, 2], chainState = SimpleChainState 4}

    it "can abort and re-open a head when one party has not committed" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              let parameters = HeadParameters{contestationPeriod = testContestationPeriod, parties = [alice, bob]}
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice, bob], parameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}
              simulateCommit chain (alice, utxoRefs [1, 2])
              waitUntil [n1, n2] $ Committed{headId = testHeadId, party = alice, utxo = utxoRefs [1, 2], chainState = SimpleChainState 2}
              send n2 Abort
              waitUntil [n1, n2] $ HeadIsAborted{headId = testHeadId, utxo = utxoRefs [1, 2], chainState = SimpleChainState 3}
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice, bob], parameters, chainState = SimpleChainState 4, headSeed = testHeadSeed}

    it "cannot abort head when commits have been collected" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice, bob], parameters = HeadParameters{contestationPeriod = testContestationPeriod, parties = [alice, bob]}, chainState = SimpleChainState 1, headSeed = testHeadSeed}
              simulateCommit chain (alice, utxoRef 1)
              simulateCommit chain (bob, utxoRef 2)

              waitUntil [n1, n2] $ HeadIsOpen{headId = testHeadId, initialUTxO = utxoRefs [1, 2], chainState = SimpleChainState 4}

              send n1 Abort

              waitMatch n1 $ \case
                CommandFailed{} -> guard True
                _ -> Nothing

    it "ignores head initialization of other head" $
      shouldRunInSim $
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}
              -- We expect bob to ignore alice's head which he is not part of
              -- although bob's configuration would includes alice as a
              -- peerconfigured)
              waitMatch n2 $ \case
                IgnoredHeadInitializing{headId, parties} ->
                  guard $ headId == testHeadId && parties == fromList [alice]
                _ -> Nothing

    it "outputs committed utxo when client requests it" $
      shouldRunInSim $
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice, bob], parameters = HeadParameters{contestationPeriod = testContestationPeriod, parties = [alice, bob]}, chainState = SimpleChainState 1, headSeed = testHeadSeed}
              simulateCommit chain (alice, utxoRef 1)

              waitUntil [n2] $ Committed{headId = testHeadId, party = alice, utxo = utxoRef 1, chainState = SimpleChainState 2}
              send n2 GetUTxO

              waitUntil [n2] $ GetUTxOResponse testHeadId (utxoRefs [1])

    describe "in an open head" $ do
      it "sees the head closed by other nodes" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead chain n1 n2

                send n1 Close
                waitForNext n2
                  >>= assertHeadIsClosedWith 0

      it "valid new transactions are seen by all parties" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead chain n1 n2

                let tx = aValidTx 42
                send n1 (NewTx tx)
                waitUntil [n1, n2] $ TxValid{headId = testHeadId, tx, newLocalUTxO = utxoRefs [1, 2, 42]}

      it "valid new transactions get snapshotted" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead chain n1 n2

                let tx = aValidTx 42
                send n1 (NewTx tx)
                waitUntil [n1, n2] $ TxValid{headId = testHeadId, tx, newLocalUTxO = utxoRefs [1, 2, 42]}

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
                openHead chain n1 n2

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
                    number == 1 && confirmed == [aValidTx 40]
                  _ -> False

                -- Expect bob to also snapshot what did "not fit" into the first
                -- snapshot.
                waitUntilMatch [n1, n2] $ \case
                  SnapshotConfirmed{snapshot = Snapshot{number, confirmed}} ->
                    -- NOTE: We sort the confirmed to be clear that the order may
                    -- be freely picked by the leader.
                    number == 2 && sort confirmed == [aValidTx 41, aValidTx 42]
                  _ -> False

                -- As there are no pending transactions and snapshots anymore
                -- we expect to continue normally on seeing just another tx.
                send n1 (NewTx $ aValidTx 44)
                waitUntilMatch [n1, n2] $ \case
                  SnapshotConfirmed{snapshot = Snapshot{number, confirmed}} ->
                    number == 3 && confirmed == [aValidTx 44]
                  _ -> False

      it "depending transactions stay pending and are confirmed in order" $
        shouldRunInSim $
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 -> do
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead chain n1 n2
                let firstTx = SimpleTx 1 (utxoRef 1) (utxoRef 3)
                let secondTx = SimpleTx 2 (utxoRef 3) (utxoRef 4)
                -- Expect secondTx to be valid, but not applicable and stay pending
                send n2 (NewTx secondTx)
                send n1 (NewTx firstTx)

                -- Expect a snapshot of the firstTx transaction
                waitUntil [n1, n2] $ TxValid{headId = testHeadId, newLocalUTxO = utxoRefs [2, 3], tx = firstTx}
                waitUntil [n1, n2] $ do
                  let snapshot = testSnapshot 1 0 [firstTx] (utxoRefs [2, 3])
                      sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
                  SnapshotConfirmed testHeadId snapshot sigs

                -- Expect a snapshot of the now unblocked secondTx
                waitUntil [n1, n2] $ TxValid{headId = testHeadId, newLocalUTxO = utxoRefs [2, 4], tx = secondTx}
                waitUntil [n1, n2] $ do
                  let snapshot = testSnapshot 2 0 [secondTx] (utxoRefs [2, 4])
                      sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
                  SnapshotConfirmed testHeadId snapshot sigs

      it "depending transactions expire if not applicable in time" $
        shouldRunInSim $
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 -> do
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead chain n1 n2
                let firstTx = SimpleTx 1 (utxoRef 1) (utxoRef 3)
                -- NOTE: secondTx is not applicable as it depends on an non-egisting output 3
                let secondTx = SimpleTx 2 (utxoRef 3) (utxoRef 4)
                -- Expect secondTx to be valid, but not applicable and stay pending
                send n2 (NewTx secondTx)
                -- If we wait too long, secondTx will expire
                threadDelay $ fromIntegral defaultTTL * waitDelay + 1
                waitUntilMatch [n1, n2] $ \case
                  TxInvalid{transaction} -> transaction == secondTx
                  _ -> False

                send n1 (NewTx firstTx)
                waitUntil [n1, n2] $ TxValid{headId = testHeadId, newLocalUTxO = utxoRefs [2, 3], tx = firstTx}

      it "sending two conflicting transactions should lead one being confirmed and one expired" $
        shouldRunInSim $
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 -> do
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead chain n1 n2
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
                  TxInvalid{transaction} -> transaction == tx''
                  _ -> False

      it "outputs utxo from confirmed snapshot when client requests it" $
        shouldRunInSim $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead chain n1 n2
                let newTx = (aValidTx 42){txInputs = utxoRefs [1]}
                send n1 (NewTx newTx)

                let snapshot = testSnapshot 1 0 [newTx] (utxoRefs [2, 42])
                    sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]

                waitUntil [n1, n2] $ SnapshotConfirmed testHeadId snapshot sigs

                send n1 GetUTxO

                waitUntil [n1] $ GetUTxOResponse testHeadId (utxoRefs [2, 42])

      describe "Commit" $ do
        it "requested commits get approved" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let deadline = arbitrary `generateWith` 42
                  let snapshotVersion = 0
                  injectChainEvent n1 Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState{slot = ChainSlot 5}}
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}

                  waitUntilMatch [n1, n2] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                        maybe False (11 `member`) utxoToCommit
                      _ -> False

                  waitUntil [n1] $ CommitApproved{headId = testHeadId, utxoToCommit = depositUTxO}
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId = 1, newVersion = snapshotVersion + 1}

                  send n1 GetUTxO
                  waitUntilMatch [n1] $
                    \case
                      GetUTxOResponse{headId, utxo} -> headId == testHeadId && member 11 utxo
                      _ -> False
        it "can process multiple commits" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let depositUTxO2 = utxoRefs [22]
                  let deadline = arbitrary `generateWith` 42
                  let snapshotVersion = 0
                  injectChainEvent
                    n1
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState{slot = ChainSlot 5}}
                  injectChainEvent
                    n2
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO2 2 deadline, newChainState = SimpleChainState{slot = ChainSlot 6}}
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}
                  waitUntil [n2] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO2, pendingDeposit = Map.singleton 2 depositUTxO2, deadline, newLocalUTxO = utxoRefs [1, 2, 22]}
                  waitUntilMatch [n1, n2] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                        maybe False (11 `member`) utxoToCommit
                      _ -> False

                  waitUntil [n1] $ CommitApproved{headId = testHeadId, utxoToCommit = depositUTxO}
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId = 1, newVersion = snapshotVersion + 1}
                  let normalTx = SimpleTx 3 (utxoRef 2) (utxoRef 3)
                  send n2 (NewTx normalTx)
                  waitUntil [n1, n2] $ TxValid{headId = testHeadId, tx = normalTx, newLocalUTxO = utxoRefs [1, 3, 11]}
                  waitUntilMatch [n1, n2] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                        maybe False (22 `member`) utxoToCommit
                      _ -> False
                  waitUntil [n2] $ CommitApproved{headId = testHeadId, utxoToCommit = depositUTxO2}
                  waitUntil [n2] $ CommitFinalized{headId = testHeadId, depositTxId = 2, newVersion = snapshotVersion + 2}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 3, 11, 22], chainState = SimpleChainState 9}

        it "can process transactions while commit pending" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let deadline = arbitrary `generateWith` 42
                  let snapshotVersion = 0
                  injectChainEvent
                    n1
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState{slot = ChainSlot 5}}

                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}
                  let normalTx = SimpleTx 2 (utxoRef 2) (utxoRef 3)
                  send n2 (NewTx normalTx)
                  waitUntil [n1] $ CommitApproved{headId = testHeadId, utxoToCommit = depositUTxO}
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{confirmed}} -> normalTx `elem` confirmed
                    _ -> False
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId = 1, newVersion = snapshotVersion + 1}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 3, 11], chainState = SimpleChainState 8}

        it "can close with commit in flight" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let deadline = arbitrary `generateWith` 42
                  injectChainEvent
                    n1
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState{slot = ChainSlot 5}}

                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}
                  waitUntilMatch [n1] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                        maybe False (11 `member`) utxoToCommit
                      _ -> False

                  send n1 Close
                  waitUntilMatch [n1, n2] $
                    \case
                      HeadIsClosed{snapshotNumber} -> snapshotNumber == 1
                      _ -> False
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2, 11], chainState = SimpleChainState 8}

        it "fanout utxo is correct after a commit" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let deadline = arbitrary `generateWith` 42
                  injectChainEvent
                    n2
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState{slot = ChainSlot 0}}
                  waitUntil [n2] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2], chainState = SimpleChainState 7}

        it "can do new deposit once the first one has settled" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let deadline = arbitrary `generateWith` 42
                  let depositUTxO2 = utxoRefs [111]
                  let deadline2 = arbitrary `generateWith` 42
                  let snapshotVersion = 0
                  injectChainEvent
                    n1
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState 5}
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}
                  waitUntil [n1] $ CommitApproved{headId = testHeadId, utxoToCommit = utxoRefs [11]}
                  waitUntil [n1, n2] $ CommitFinalized{headId = testHeadId, depositTxId = 1, newVersion = snapshotVersion + 1}
                  injectChainEvent
                    n2
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO2 2 deadline2, newChainState = SimpleChainState 6}
                  waitUntil [n2] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO2, pendingDeposit = Map.singleton 2 depositUTxO2, deadline = deadline2, newLocalUTxO = utxoRefs [1, 2, 11, 111]}
                  waitUntilMatch [n1, n2] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                        maybe False (111 `member`) utxoToCommit
                      _ -> False

                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2, 11, 111], chainState = SimpleChainState 9}

        it "multiple commits and decommits in sequence" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let deadline = arbitrary `generateWith` 42
                  let snapsotVersion = 0
                  injectChainEvent
                    n1
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState{slot = ChainSlot 5}}
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}
                  waitUntilMatch [n1, n2] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                        maybe False (11 `member`) utxoToCommit
                      _ -> False
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId = 1, newVersion = snapsotVersion + 1}

                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  send n2 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}

                  waitUntilMatch [n1] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToDecommit}} ->
                        maybe False (42 `member`) utxoToDecommit
                      _ -> False

                  waitUntil [n1, n2] $ DecommitApproved testHeadId (txId decommitTx) (utxoRefs [42])
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, decommitTxId = txId decommitTx, newVersion = snapsotVersion + 2}
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n2 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [2, 11], chainState = SimpleChainState{slot = ChainSlot 10}}
        it "commit and decommit same utxo" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let depositUTxO = utxoRefs [11]
                  let deadline = arbitrary `generateWith` 42
                  let snapshotVersion = 0
                  injectChainEvent
                    n1
                    Observation{observedTx = OnDepositTx testHeadId depositUTxO 1 deadline, newChainState = SimpleChainState{slot = ChainSlot 5}}
                  waitUntil [n1] $ CommitRecorded{headId = testHeadId, utxoToCommit = depositUTxO, pendingDeposit = Map.singleton 1 depositUTxO, deadline, newLocalUTxO = utxoRefs [1, 2, 11]}
                  waitUntilMatch [n1, n2] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToCommit}} ->
                        maybe False (11 `member`) utxoToCommit
                      _ -> False
                  waitUntil [n1] $ CommitFinalized{headId = testHeadId, depositTxId = 1, newVersion = snapshotVersion + 1}

                  send n1 GetUTxO
                  waitUntilMatch [n1] $
                    \case
                      GetUTxOResponse{headId, utxo} -> headId == testHeadId && member 11 utxo
                      _ -> False

                  let decommitTx = SimpleTx 1 (utxoRef 11) (utxoRef 88)
                  send n2 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [88]}
                  waitUntilMatch [n1, n2] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToDecommit}} ->
                        maybe False (88 `member`) utxoToDecommit
                      _ -> False

                  waitUntil [n1, n2] $ DecommitApproved{headId = testHeadId, decommitTxId = txId decommitTx, utxoToDecommit = utxoRefs [88]}
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, decommitTxId = txId decommitTx, newVersion = snapshotVersion + 2}
                  send n1 GetUTxO
                  waitUntilMatch [n1] $
                    \case
                      GetUTxOResponse{headId, utxo} -> headId == testHeadId && not (member 11 utxo)
                      _ -> False

      describe "Decommit" $ do
        it "can request decommit" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2

                  let decommitTx = aValidTx 42
                  send n1 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}

        it "requested decommits get approved" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  let snapshotVersion = 0
                  send n2 (Decommit decommitTx)
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}

                  waitUntilMatch [n1] $
                    \case
                      SnapshotConfirmed{snapshot = Snapshot{utxoToDecommit}} ->
                        maybe False (42 `member`) utxoToDecommit
                      _ -> False

                  waitUntil [n1, n2] $ DecommitApproved testHeadId (txId decommitTx) (utxoRefs [42])
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, decommitTxId = txId decommitTx, newVersion = snapshotVersion + 1}

                  send n1 GetUTxO
                  waitUntilMatch [n1] $
                    \case
                      GetUTxOResponse{headId, utxo} -> headId == testHeadId && not (member 42 utxo)
                      _ -> False

        it "can only process one decommit at once" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let decommitTx1 = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  let snapshotVersion = 0
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

                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, decommitTxId = txId decommitTx1, newVersion = snapshotVersion + 1}

                  send n2 (Decommit{decommitTx = decommitTx2})
                  waitUntil [n1, n2] $ DecommitApproved{headId = testHeadId, decommitTxId = txId decommitTx2, utxoToDecommit = utxoRefs [22]}
                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, decommitTxId = txId decommitTx2, newVersion = snapshotVersion + 2}

        it "can process transactions while decommit pending" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 ->
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2

                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  let snapshotVersion = 0
                  send n2 (Decommit{decommitTx})
                  waitUntil [n1, n2] $
                    DecommitRequested{headId = testHeadId, decommitTx, utxoToDecommit = utxoRefs [42]}
                  waitUntil [n1, n2] $
                    DecommitApproved{headId = testHeadId, decommitTxId = 1, utxoToDecommit = utxoRefs [42]}

                  let normalTx = SimpleTx 2 (utxoRef 2) (utxoRef 3)
                  send n2 (NewTx normalTx)
                  waitUntilMatch [n1, n2] $ \case
                    SnapshotConfirmed{snapshot = Snapshot{confirmed}} -> normalTx `elem` confirmed
                    _ -> False

                  waitUntil [n1, n2] $ DecommitFinalized{headId = testHeadId, decommitTxId = 1, newVersion = snapshotVersion + 1}

        it "can close with decommit in flight" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 2) (utxoRef 42)
                  send n2 (Decommit{decommitTx})
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n1 Fanout

                  waitMatch n2 $ \case
                    HeadIsContested{headId, snapshotNumber} -> guard $ headId == testHeadId && snapshotNumber == 1
                    _ -> Nothing

                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1], chainState = SimpleChainState 11}

        it "fanout utxo is correct after a decommit" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
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
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [2], chainState = SimpleChainState 9}

        it "can fanout with empty utxo" $
          shouldRunInSim $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [bob] chain $ \n1 -> do
                withHydraNode bobSk [alice] chain $ \n2 -> do
                  openHead chain n1 n2
                  let decommitTx = SimpleTx 1 (utxoRef 1) (utxoRef 42)
                  let snapshotVersion = 0
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
                      , decommitTxId = txId decommitTx
                      , newVersion = snapshotVersion + 1
                      }
                  let decommitTx2 = SimpleTx 2 (utxoRef 2) (utxoRef 88)
                  send n1 (Decommit{decommitTx = decommitTx2})
                  waitUntil [n1, n2] $
                    DecommitFinalized
                      { headId = testHeadId
                      , decommitTxId = txId decommitTx2
                      , newVersion = snapshotVersion + 2
                      }
                  send n1 Close
                  waitUntil [n1, n2] $ ReadyToFanout{headId = testHeadId}
                  send n1 Fanout
                  waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [], chainState = SimpleChainState 11}

    it "can be finalized by all parties after contestation period" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead chain n1 n2
              send n1 Close
              forM_ [n1, n2] $ waitForNext >=> assertHeadIsClosed
              waitUntil [n1, n2] $ ReadyToFanout testHeadId
              send n1 Fanout
              send n2 Fanout
              waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2], chainState = SimpleChainState{slot = ChainSlot 7}}

    it "contest automatically when detecting closing with old snapshot" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead chain n1 n2

              -- Perform a transaction to produce the latest snapshot, number 1
              let tx = aValidTx 42
              send n2 (NewTx tx)
              waitUntilMatch [n1, n2] $ \case
                SnapshotConfirmed{snapshot = Snapshot{number}} -> number == 1
                _ -> False

              -- Have n1 & n2 observe a close with not the latest snapshot
              let deadline = arbitrary `generateWith` 42
              -- XXX: This is a bit cumbersome and maybe even incorrect (chain
              -- states), the simulated chain should provide a way to inject an
              -- 'OnChainTx' without providing a chain state?
              injectChainEvent n1 Observation{observedTx = OnCloseTx testHeadId 0 deadline, newChainState = SimpleChainState{slot = ChainSlot 0}}
              injectChainEvent n2 Observation{observedTx = OnCloseTx testHeadId 0 deadline, newChainState = SimpleChainState{slot = ChainSlot 0}}

              waitUntilMatch [n1, n2] $ \case
                HeadIsClosed{snapshotNumber} -> snapshotNumber == 0
                _ -> False

              -- Expect n1 to contest with latest snapshot, number 1
              waitUntilMatch [n1, n2] $ \case
                HeadIsContested{snapshotNumber} -> snapshotNumber == 1
                _ -> False

  describe "Hydra Node Logging" $ do
    it "traces processing of events" $ do
      let result = runSimTrace $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [] chain $ \n1 -> do
                send n1 Init
                waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState{slot = ChainSlot 1}, headSeed = testHeadSeed}
                simulateCommit chain (alice, utxoRef 1)

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs
        `shouldContain` [BeginInput alice 0 (ClientInput Init)]
      logs
        `shouldContain` [EndInput alice 0]

    it "traces handling of effects" $ do
      let result = runSimTrace $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [] chain $ \n1 -> do
                send n1 Init
                waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs `shouldContain` [BeginEffect alice 2 0 (ClientEffect $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed})]
      logs `shouldContain` [EndEffect alice 2 0]

  describe "rolling back & forward does not make the node crash" $ do
    it "does work for rollbacks past init" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState 1, headSeed = testHeadSeed}
            -- We expect the Init to be rolled back and forward again
            rollbackAndForward chain 0
            -- We expect the node to still work and let us commit
            simulateCommit chain (alice, utxoRef 1)
            waitUntil [n1] $ Committed{headId = testHeadId, party = alice, utxo = utxoRef 1, chainState = SimpleChainState 2}

    it "does work for rollbacks past open" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice], parameters = testHeadParameters, chainState = SimpleChainState{slot = ChainSlot 1}, headSeed = testHeadSeed}
            simulateCommit chain (alice, utxoRef 1)
            waitUntil [n1] $ Committed{headId = testHeadId, party = alice, utxo = utxoRef 1, chainState = SimpleChainState{slot = ChainSlot 2}}
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, initialUTxO = utxoRefs [1], chainState = SimpleChainState{slot = ChainSlot 3}}
            -- We expect one Commit AND the CollectCom to be rolled back and
            -- forward again
            rollbackAndForward chain 2
            -- We expect the node to still work and let us post L2 transactions
            let tx = aValidTx 42
            send n1 (NewTx tx)
            waitUntil [n1] $ TxValid{headId = testHeadId, tx, newLocalUTxO = utxoRefs [1, 42]}

-- | Wait for some output at some node(s) to be produced /eventually/. See
-- 'waitUntilMatch' for how long it waits.
waitUntil ::
  (HasCallStack, MonadThrow m, MonadAsync m, MonadTimer m, IsChainState tx) =>
  [TestHydraClient tx m] ->
  StateChanged tx ->
  m ()
waitUntil nodes expected =
  waitUntilMatch nodes (== expected)

-- | Wait for some output to match some predicate /eventually/. This will not
-- wait forever, but for a long time (1 month) to get a nice error location.
-- Should not be an issue when used within `shouldRunInSim`, this was even 1000
-- years before - but we since we are having the protocol produce 'Tick' events
-- constantly this would be fully simulated to the end.
waitUntilMatch ::
  (Show (StateChanged tx), HasCallStack, MonadThrow m, MonadAsync m, MonadTimer m) =>
  [TestHydraClient tx m] ->
  (StateChanged tx -> Bool) ->
  m ()
waitUntilMatch nodes predicate = do
  seenMsgs <- newTVarIO []
  timeout oneMonth (forConcurrently_ nodes $ match seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "waitUntilMatch did not match a message within " <> show oneMonth <> ", seen messages:"
            , unlines (show <$> msgs)
            ]
 where
  match seenMsgs n = do
    msg <- waitForNext n
    atomically (modifyTVar' seenMsgs (msg :))
    unless (predicate msg) $
      match seenMsgs n

  oneMonth = 100 -- 3600 * 24 * 30

-- | Wait for an output matching the predicate and extracting some value. This
-- will loop forever until a match has been found.
waitMatch ::
  MonadThrow m =>
  TestHydraClient tx m ->
  (StateChanged tx -> Maybe a) ->
  m a
waitMatch node predicate =
  go
 where
  go = do
    next <- waitForNext node
    maybe go pure (predicate next)

-- XXX: The names of the following handles and functions are confusing.

-- | A thin client layer around 'HydraNode' to be interact with it through
-- 'send', 'waitForNext', access all outputs and inject events through the test
-- chain.
data TestHydraClient tx m = TestHydraClient
  { send :: ClientInput tx -> m ()
  , waitForNext :: m (StateChanged tx)
  , injectChainEvent :: ChainEvent tx -> m ()
  , serverOutputs :: m [StateChanged tx]
  , queryState :: m (HeadState tx)
  }

-- | A simulated chain that just echoes 'PostChainTx' as 'Observation's of
-- 'OnChainTx' onto all connected nodes. It can also 'rollbackAndForward' any
-- number of these "transactions".
data SimulatedChainNetwork tx m = SimulatedChainNetwork
  { connectNode :: DraftHydraNode tx m -> m (HydraNode tx m)
  , tickThread :: Async m ()
  , rollbackAndForward :: Natural -> m ()
  , simulateCommit :: (Party, UTxOType tx) -> m ()
  , closeWithInitialSnapshot :: (Party, UTxOType tx) -> m ()
  }

dummySimulatedChainNetwork :: SimulatedChainNetwork tx m
dummySimulatedChainNetwork =
  SimulatedChainNetwork
    { connectNode = \_ -> error "connectNode"
    , tickThread = error "tickThread"
    , rollbackAndForward = \_ -> error "rollbackAndForward"
    , simulateCommit = \_ -> error "simulateCommit"
    , closeWithInitialSnapshot = \(_, _) -> error "closeWithInitialSnapshot"
    }

-- | With-pattern wrapper around 'simulatedChainAndNetwork' which does 'cancel'
-- the 'tickThread'. Also, this will fix tx to 'SimpleTx' so that it can pick an
-- initial chain state to play back to our test nodes.
-- NOTE: The simulated network has a block time of 20 (simulated) seconds.
withSimulatedChainAndNetwork ::
  (MonadTime m, MonadDelay m, MonadAsync m) =>
  (SimulatedChainNetwork SimpleTx m -> m ()) ->
  m ()
withSimulatedChainAndNetwork action = do
  chain <- simulatedChainAndNetwork SimpleChainState{slot = ChainSlot 0}
  action chain
  cancel $ tickThread chain

-- | Class to manipulate the chain state by advancing it's slot in
-- 'simulatedChainAndNetwork'.
class IsChainState a => IsChainStateTest a where
  advanceSlot :: ChainStateType a -> ChainStateType a

instance IsChainStateTest SimpleTx where
  advanceSlot SimpleChainState{slot} = SimpleChainState{slot = nextChainSlot slot}

-- | Creates a simulated chain and network to which 'HydraNode's can be
-- connected to using 'connectNode'. NOTE: The 'tickThread' needs to be
-- 'cancel'ed after use. Use 'withSimulatedChainAndNetwork' instead where
-- possible.
simulatedChainAndNetwork ::
  forall m tx.
  (MonadTime m, MonadDelay m, MonadAsync m, IsChainStateTest tx) =>
  ChainStateType tx ->
  m (SimulatedChainNetwork tx m)
simulatedChainAndNetwork initialChainState = do
  history <- newTVarIO []
  nodes <- newTVarIO []
  localChainState <- newLocalChainState (initHistory initialChainState)
  tickThread <- async $ simulateTicks nodes localChainState
  pure $
    SimulatedChainNetwork
      { connectNode = \draftNode -> do
          let mockChain =
                Chain
                  { postTx = \tx -> do
                      now <- getCurrentTime
                      -- Only observe "after one block"
                      void . async $ do
                        threadDelay blockTime
                        createAndYieldEvent nodes history localChainState $ toOnChainTx now tx
                  , draftCommitTx = \_ -> error "unexpected call to draftCommitTx"
                  , draftDepositTx = \_ -> error "unexpected call to draftIncrementalCommitTx"
                  , submitTx = \_ -> error "unexpected call to submitTx"
                  }
              mockNetwork = createMockNetwork draftNode nodes
              mockServer = Server{sendOutput = const $ pure ()}
          node <- connect mockChain mockNetwork mockServer draftNode
          atomically $ modifyTVar nodes (node :)
          pure node
      , tickThread
      , rollbackAndForward = rollbackAndForward nodes history localChainState
      , simulateCommit = \(party, committed) ->
          createAndYieldEvent nodes history localChainState $ OnCommitTx{headId = testHeadId, party, committed}
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
      let chainSlot = chainStateSlot cs
      pure $ Tick now chainSlot
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

  recordAndYieldEvent nodes history chainEvent = do
    ns <- atomically $ do
      modifyTVar' history (chainEvent :)
      readTVar nodes
    forM_ ns $ \n ->
      handleChainEvent n chainEvent

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
    forM_ ns $ \n -> handleChainEvent n Rollback{rolledBackChainState}
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
    enqueue inputQueue . NetworkInput defaultTTL $ ReceivedMessage{sender, msg}

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
  RecoverTx{headId, recoverTxId} ->
    OnRecoverTx{headId, recoveredTxId = recoverTxId}
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
      , distributedOutputs = maybe mempty outputsOfUTxO utxoToDecommit
      }
   where
    Snapshot{version, utxoToDecommit} = getSnapshot decrementingSnapshot
  CloseTx{closingSnapshot} ->
    OnCloseTx
      { headId = testHeadId
      , snapshotNumber = number (getSnapshot closingSnapshot)
      , contestationDeadline = addUTCTime (toNominalDiffTime testContestationPeriod) now
      }
  ContestTx{headId, contestingSnapshot} ->
    OnContestTx
      { headId
      , snapshotNumber = number (getSnapshot contestingSnapshot)
      , contestationDeadline = addUTCTime (toNominalDiffTime testContestationPeriod) now
      }
  FanoutTx{} ->
    OnFanoutTx{headId = testHeadId}

testContestationPeriod :: ContestationPeriod
testContestationPeriod = UnsafeContestationPeriod 10

testDepositDeadline :: DepositDeadline
testDepositDeadline = UnsafeDepositDeadline 10

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
  outputs <- atomically newTQueue
  outputHistory <- newTVarIO mempty
  let initialChainState = SimpleChainState{slot = ChainSlot 0}
  node <- createHydraNode traceInIOSim simpleLedger initialChainState signingKey otherParties outputs outputHistory chain testContestationPeriod testDepositDeadline
  withAsync (runHydraNode node) $ \_ ->
    action (createTestHydraClient outputs outputHistory node)

createTestHydraClient ::
  MonadSTM m =>
  TQueue m (StateChanged tx) ->
  TVar m [StateChanged tx] ->
  HydraNode tx m ->
  TestHydraClient tx m
createTestHydraClient outputs outputHistory HydraNode{inputQueue, nodeState} =
  TestHydraClient
    { send = enqueue inputQueue . ClientInput
    , waitForNext = atomically (readTQueue outputs)
    , injectChainEvent = enqueue inputQueue . ChainInput
    , serverOutputs = reverse <$> readTVarIO outputHistory
    , queryState = atomically (queryHeadState nodeState)
    }

createHydraNode ::
  (MonadDelay m, MonadAsync m, MonadLabelledSTM m, MonadThrow m) =>
  Tracer m (HydraNodeLog tx) ->
  Ledger tx ->
  ChainStateType tx ->
  SigningKey HydraKey ->
  [Party] ->
  TQueue m (StateChanged tx) ->
  TVar m [StateChanged tx] ->
  SimulatedChainNetwork tx m ->
  ContestationPeriod ->
  DepositDeadline ->
  m (HydraNode tx m)
createHydraNode tracer ledger chainState signingKey otherParties outputs outputHistory chain cp depositDeadline = do
  (eventSource, eventSink) <- createMockSourceSink
  -- NOTE: Not using 'hydrate' as we don't want to run the event source conduit.
  let headState = Idle IdleState{chainState}
  let chainStateHistory = initHistory chainState
  nodeState <- createNodeState Nothing headState
  inputQueue <- createInputQueue
  node <-
    connectNode
      chain
      DraftHydraNode
        { tracer
        , env
        , ledger
        , nodeState
        , inputQueue
        , eventSource
        , eventSinks = [eventSink]
        , chainStateHistory
        }
  pure $
    node
      { server =
          Server
            { sendOutput = \out -> atomically $ do
                writeTQueue outputs out
                modifyTVar' outputHistory (out :)
            }
      }
 where
  env =
    Environment
      { party
      , signingKey
      , otherParties
      , contestationPeriod = cp
      , participants
      , depositDeadline
      }
  party = deriveParty signingKey

  -- NOTE: We use the hydra-keys as on-chain identities directly. This is fine
  -- as this is a simulated network.
  participants = deriveOnChainId <$> (party : otherParties)

openHead ::
  SimulatedChainNetwork SimpleTx (IOSim s) ->
  TestHydraClient SimpleTx (IOSim s) ->
  TestHydraClient SimpleTx (IOSim s) ->
  IOSim s ()
openHead chain n1 n2 = do
  send n1 Init
  waitUntil [n1, n2] $ HeadIsInitializing{headId = testHeadId, parties = fromList [alice, bob], parameters = HeadParameters{contestationPeriod = testContestationPeriod, parties = [alice, bob]}, headSeed = testHeadSeed, chainState = SimpleChainState 1}
  simulateCommit chain (alice, utxoRef 1)
  waitUntil [n1, n2] $ Committed{headId = testHeadId, party = alice, utxo = utxoRef 1, chainState = SimpleChainState 2}
  simulateCommit chain (bob, utxoRef 2)
  waitUntil [n1, n2] $ Committed{headId = testHeadId, party = bob, utxo = utxoRef 2, chainState = SimpleChainState 3}
  waitUntil [n1, n2] $ HeadIsOpen{headId = testHeadId, chainState = SimpleChainState 4, initialUTxO = utxoRefs [1, 2]}

assertHeadIsClosed :: (HasCallStack, MonadThrow m) => StateChanged tx -> m ()
assertHeadIsClosed = \case
  HeadIsClosed{} -> pure ()
  _ -> failure "expected HeadIsClosed"

assertHeadIsClosedWith :: (HasCallStack, MonadThrow m) => SnapshotNumber -> StateChanged tx -> m ()
assertHeadIsClosedWith expectedSnapshotNumber = \case
  HeadIsClosed{snapshotNumber} -> do
    snapshotNumber `shouldBe` expectedSnapshotNumber
  _ -> failure "expected HeadIsClosed"

-- | Provide a quick and dirty to way to label stuff from a signing key
shortLabel :: SigningKey HydraKey -> String
shortLabel s =
  take 8 $ drop 1 $ List.words (show s) !! 2
