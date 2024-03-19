{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Unit tests of the the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Data.Map (notMember)
import Data.Set qualified as Set
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (genTxIn, mkVkAddress, txOutValue, unSlotNo, pattern TxValidityUpperBound)
import Hydra.Chain (
  ChainEvent (..),
  HeadParameters (..),
  IsChainState,
  OnChainTx (..),
  PostChainTx (CollectComTx, ContestTx),
 )
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.State ()
import Hydra.Crypto (generateSigningKey, sign)
import Hydra.Crypto qualified as Crypto
import Hydra.Environment (Environment (..))
import Hydra.HeadLogic (
  ClosedState (..),
  CoordinatedHeadState (..),
  Effect (..),
  HeadState (..),
  IdleState (..),
  InitialState (..),
  Input (..),
  LogicError (..),
  OpenState (..),
  Outcome (..),
  RequirementFailure (..),
  SeenSnapshot (NoSeenSnapshot, SeenSnapshot),
  TTL,
  WaitReason (..),
  aggregateState,
  cause,
  defaultTTL,
  update,
 )
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.Ledger (ChainSlot (..), IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano (cardanoLedger, genKeyPair, genOutput, mkRangedTx)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Network.Message (Message (..))
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (Party (..))
import Hydra.Prelude qualified as Prelude
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, deriveOnChainId, testHeadId, testHeadSeed)
import Test.QuickCheck (Property, counterexample, elements, forAll, oneof, shuffle, suchThat)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

spec :: Spec
spec =
  parallel $ do
    let threeParties = [alice, bob, carol]
        bobEnv =
          Environment
            { party = bob
            , signingKey = bobSk
            , otherParties = [alice, carol]
            , contestationPeriod = defaultContestationPeriod
            , participants = deriveOnChainId <$> threeParties
            }
        aliceEnv =
          Environment
            { party = alice
            , signingKey = aliceSk
            , otherParties = [bob, carol]
            , contestationPeriod = defaultContestationPeriod
            , participants = deriveOnChainId <$> threeParties
            }

    describe "Coordinated Head Protocol" $ do
      let ledger = simpleLedger

      let coordinatedHeadState =
            CoordinatedHeadState
              { localUTxO = mempty
              , allTxs = mempty
              , localTxs = mempty
              , confirmedSnapshot = InitialSnapshot testHeadId mempty
              , seenSnapshot = NoSeenSnapshot
              , decommitTx = Nothing
              }

      it "reports if a requested tx is expired" $ do
        let inputs = utxoRef 1
            tx = SimpleTx 2 inputs mempty
            ttl = 0
            reqTx = NetworkInput ttl $ ReceivedMessage{sender = alice, msg = ReqTx tx}
            s0 = inOpenState threeParties

        update bobEnv ledger s0 reqTx `hasEffectSatisfying` \case
          ClientEffect TxInvalid{transaction} -> transaction == tx
          _ -> False

      it "waits if a requested tx is not (yet) applicable" $ do
        let reqTx = receiveMessage $ ReqTx $ SimpleTx 2 inputs mempty
            inputs = utxoRef 1
            s0 = inOpenState threeParties

        update bobEnv ledger s0 reqTx
          `assertWait` WaitOnNotApplicableTx (ValidationError "cannot apply transaction")

      it "confirms snapshot given it receives AckSn from all parties" $ do
        -- TODO: perhaps use smart constructor for ReqSn to reduce the noise
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [] Nothing
            snapshot1 = Snapshot testHeadId 1 mempty [] mempty
            ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot1) 1
        snapshotInProgress <- runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
          step reqSn
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getState

        getConfirmedSnapshot snapshotInProgress `shouldBe` Just (testSnapshot 0 mempty [])

        snapshotConfirmed <-
          runHeadLogic bobEnv ledger snapshotInProgress $ do
            step (ackFrom bobSk bob)
            getState
        getConfirmedSnapshot snapshotConfirmed `shouldBe` Just snapshot1

      describe "Decommit" $ do
        it "observes DecommitRequested and ReqDec in an Open state" $
          let decommitTx = SimpleTx 1 mempty (utxoRef 1)
              reqDec = ReqDec{transaction = decommitTx, decommitRequester = alice}
              input = NetworkInput defaultTTL alice reqDec
              st = inOpenState threeParties
              outcome = update aliceEnv ledger st input
           in outcome
                `hasEffectSatisfying` \case
                  ClientEffect DecommitRequested{headId, utxoToDecommit} ->
                    headId == testHeadId && utxoToDecommit == utxoRef 1
                  NetworkEffect ReqDec{transaction, decommitRequester} ->
                    transaction == decommitTx && decommitRequester == alice
                  _ -> False

        it "ignores ReqDec when not in Open state" $ monadicIO $ do
          let reqDec = ReqDec{transaction = SimpleTx 1 mempty (utxoRef 1), decommitRequester = alice}
          let input = NetworkInput defaultTTL alice reqDec
          st <- pickBlind $ oneof $ pure <$> [inInitialState threeParties, inIdleState, inClosedState threeParties]
          pure $
            update aliceEnv ledger st input
              `shouldNotBe` cause (NetworkEffect reqDec)

        it "wait for second decommit when another one is in flight" $ do
          let decommitTx1 = SimpleTx 1 mempty (utxoRef 1)
              decommitTx2 = SimpleTx 2 mempty (utxoRef 2)
              reqDec1 = ReqDec{transaction = decommitTx1, decommitRequester = alice}
              reqDec2 = ReqDec{transaction = decommitTx2, decommitRequester = bob}
              reqDecEvent1 = NetworkInput defaultTTL alice reqDec1
              reqDecEvent2 = NetworkInput defaultTTL bob reqDec2
              s0 = inOpenState threeParties

          s1 <- runHeadLogic aliceEnv ledger s0 $ do
            step reqDecEvent1
            getState

          let outcome = update bobEnv ledger s1 reqDecEvent2

          outcome `shouldSatisfy` \case
            Wait (WaitOnNotApplicableDecommitTx{waitingOnDecommitTx = decommitTx''}) _ ->
              decommitTx2 == decommitTx''
            _ -> False

        it "updates decommitTx on valid ReqDec" $ do
          let decommitTx' = SimpleTx 1 mempty (utxoRef 1)
          let reqDec = ReqDec{transaction = decommitTx', decommitRequester = alice}
              reqDecEvent = NetworkInput defaultTTL alice reqDec
              s0 = inOpenState threeParties

          s0 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> isNothing decommitTx
            _ -> False

          s1 <- runHeadLogic aliceEnv ledger s0 $ do
            step reqDecEvent
            getState

          s1 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> decommitTx == Just decommitTx'
            _ -> False

          -- running the 'ReqDec' again should not alter the recorded state
          s2 <- runHeadLogic aliceEnv ledger s1 $ do
            step reqDecEvent
            getState

          s2 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> decommitTx == Just decommitTx'
            _ -> False

        it "emits ReqSn on valid RecDec" $ do
          let decommitTx' = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
          let s0 = inOpenState threeParties

          s0 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> isNothing decommitTx
            _ -> False

          let reqDecEvent = NetworkInput defaultTTL alice ReqDec{transaction = decommitTx', decommitRequester = alice}
          let reqSn = ReqSn{snapshotNumber = 1, transactionIds = [], decommitTx = Just decommitTx'}

          let s1 = update aliceEnv ledger s0 reqDecEvent
          s1 `hasEffect` NetworkEffect reqSn

      describe "Tracks Transaction Ids" $ do
        it "keeps transactions in allTxs given it receives a ReqTx" $ do
          let s0 = inOpenState threeParties
              t1 = SimpleTx 1 mempty (utxoRef 1)

          sa <- runHeadLogic bobEnv ledger s0 $ do
            step $ receiveMessage $ ReqTx t1
            getState

          sa `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{allTxs}}) -> txId t1 `member` allTxs
            _ -> False

        it "removes transactions in allTxs given it receives a ReqSn" $ do
          let s0 = inOpenState threeParties
              t1 = SimpleTx 1 mempty (utxoRef 1)
              reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [1] Nothing

          s1 <- runHeadLogic bobEnv ledger s0 $ do
            step $ receiveMessage $ ReqTx t1
            step reqSn
            getState

          s1 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{allTxs}}) -> txId t1 `notMember` allTxs
            _ -> False

        it "removes transactions from allTxs when included in a acked snapshot even when emitting a ReqSn" $ do
          let t1 = SimpleTx 1 mempty (utxoRef 1)
              pendingTransaction = SimpleTx 2 mempty (utxoRef 2)
              reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [1] Nothing
              snapshot1 = testSnapshot 1 (utxoRefs [1]) [1]
              ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot1) 1

          sa <- runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step $ receiveMessage $ ReqTx t1
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)

            step $ receiveMessage $ ReqTx pendingTransaction

            step (ackFrom bobSk bob)
            getState

          sa `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{allTxs}}) -> txId t1 `notMember` allTxs
            _ -> False

      it "rejects last AckSn if one signature was from a different snapshot" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [] Nothing
            snapshot = testSnapshot 1 mempty []
            snapshot' = testSnapshot 2 mempty []
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot) 1
            invalidAckFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot') 1
        waitingForLastAck <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)
            getState
        update bobEnv ledger waitingForLastAck (invalidAckFrom bobSk bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if one signature was from a different key" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [] Nothing
            snapshot = testSnapshot 1 mempty []
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot) 1
        waitingForLastAck <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)
            getState

        update bobEnv ledger waitingForLastAck (ackFrom (generateSigningKey "foo") bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if one signature was from a completely different message" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [] Nothing
            snapshot1 = testSnapshot 1 mempty []
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot1) 1
            invalidAckFrom sk vk =
              receiveMessageFrom vk $
                AckSn (coerce $ sign sk ("foo" :: ByteString)) 1
        waitingForLastAck <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (invalidAckFrom bobSk bob)
            getState

        update bobEnv ledger waitingForLastAck (ackFrom aliceSk alice)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if already received signature from this party" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [] Nothing
            snapshot1 = testSnapshot 1 mempty []
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot1) 1
        waitingForAck <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn
            step (ackFrom carolSk carol)
            getState

        update bobEnv ledger waitingForAck (ackFrom carolSk carol)
          `shouldSatisfy` \case
            Error (RequireFailed SnapshotAlreadySigned{receivedSignature}) -> receivedSignature == carol
            _ -> False

      it "waits if we receive a snapshot with transaction not applicable on previous snapshot" $ do
        let reqTx42 = NetworkInput defaultTTL alice $ ReqTx (SimpleTx 42 mempty (utxoRef 1))
            reqTx1 = NetworkInput defaultTTL alice $ ReqTx (SimpleTx 1 (utxoRef 1) (utxoRef 2))
            input = NetworkInput defaultTTL alice $ ReqSn 1 [1] Nothing
            s0 = inOpenState threeParties

        s2 <- runHeadLogic bobEnv ledger s0 $ do
          step reqTx42
          step reqTx1
          getState

        update bobEnv ledger s2 input
          `shouldBe` Error (RequireFailed (SnapshotDoesNotApply 1 1 (ValidationError "cannot apply transaction")))

      it "waits if we receive a snapshot with unseen transactions" $ do
        let s0 = inOpenState threeParties
            reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [1] Nothing
        update bobEnv ledger s0 reqSn
          `assertWait` WaitOnTxs [1]

      it "waits if we receive an AckSn for an unseen snapshot" $ do
        let snapshot = testSnapshot 1 mempty []
            input = receiveMessage $ AckSn (sign aliceSk snapshot) 1
        update bobEnv ledger (inOpenState threeParties) input
          `assertWait` WaitOnSeenSnapshot

      -- TODO: Write property tests for various future / old snapshot behavior.
      -- That way we could cover variations of snapshot numbers and state of
      -- snapshot collection.

      it "rejects if we receive a too far future snapshot" $ do
        let input = NetworkInput defaultTTL bob $ ReqSn 2 [] Nothing
            st = inOpenState threeParties
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "waits if we receive a future snapshot while collecting signatures" $ do
        let reqSn1 = NetworkInput defaultTTL alice $ ReqSn 1 [] Nothing
            reqSn2 = NetworkInput defaultTTL bob $ ReqSn 2 [] Nothing
        st <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn1
            getState

        update bobEnv ledger st reqSn2
          `assertWait` WaitOnSnapshotNumber 1

      it "acks signed snapshot from the constant leader" $ do
        let leader = alice
            snapshot = testSnapshot 1 mempty []
            input = NetworkInput defaultTTL leader $ ReqSn (number snapshot) [] Nothing
            sig = sign bobSk snapshot
            st = inOpenState threeParties
            ack = AckSn sig (number snapshot)
        update bobEnv ledger st input `hasEffect` NetworkEffect ack

      it "does not ack snapshots from non-leaders" $ do
        let input = NetworkInput defaultTTL notTheLeader $ ReqSn 1 [] Nothing
            notTheLeader = bob
            st = inOpenState threeParties
        update bobEnv ledger st input `shouldSatisfy` \case
          Error (RequireFailed ReqSnNotLeader{requestedSn = 1, leader}) -> leader == notTheLeader
          _ -> False

      it "rejects too-old snapshots" $ do
        let input = NetworkInput defaultTTL theLeader $ ReqSn 2 [] Nothing
            theLeader = alice
            snapshot = testSnapshot 2 mempty []
            st =
              inOpenState' threeParties $
                coordinatedHeadState{confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])}
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "rejects too-old snapshots when collecting signatures" $ do
        let input = NetworkInput defaultTTL theLeader $ ReqSn 2 [] Nothing
            theLeader = alice
            snapshot = testSnapshot 2 mempty []
            st =
              inOpenState' threeParties $
                coordinatedHeadState
                  { confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])
                  , seenSnapshot = SeenSnapshot (testSnapshot 3 mempty []) mempty
                  }
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 3)

      it "rejects too-new snapshots from the leader" $ do
        let input = NetworkInput defaultTTL theLeader $ ReqSn 3 [] Nothing
            theLeader = carol
            st = inOpenState threeParties
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 3 0)

      it "rejects overlapping snapshot requests from the leader" $ do
        let theLeader = alice
            nextSN = 1
            firstReqTx = NetworkInput defaultTTL alice $ ReqTx (aValidTx 42)
            firstReqSn = NetworkInput defaultTTL theLeader $ ReqSn nextSN [42] Nothing
            secondReqTx = NetworkInput defaultTTL alice $ ReqTx (aValidTx 51)
            secondReqSn = NetworkInput defaultTTL theLeader $ ReqSn nextSN [51] Nothing

        s3 <- runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
          step firstReqTx
          step firstReqSn
          step secondReqTx
          getState

        update bobEnv ledger s3 secondReqSn `shouldSatisfy` \case
          Error RequireFailed{} -> True
          _ -> False

      it "ignores in-flight ReqTx when closed" $ do
        let s0 = inClosedState threeParties
            input = receiveMessage $ ReqTx (aValidTx 42)
        update bobEnv ledger s0 input `shouldBe` Error (UnhandledInput input s0)

      it "everyone does collect on last commit after collect com" $ do
        let aliceCommit = OnCommitTx testHeadId alice (utxoRef 1)
            bobCommit = OnCommitTx testHeadId bob (utxoRef 2)
            carolCommit = OnCommitTx testHeadId carol (utxoRef 3)
        waitingForLastCommit <-
          runHeadLogic bobEnv ledger (inInitialState threeParties) $ do
            step (observeTxAtSlot 1 aliceCommit)
            step (observeTxAtSlot 2 bobCommit)
            getState

        -- Bob is not the last party, but still does post a collect
        update bobEnv ledger waitingForLastCommit (observeTxAtSlot 3 carolCommit)
          `hasEffectSatisfying` \case
            OnChainEffect{postChainTx = CollectComTx{}} -> True
            _ -> False

      it "cannot observe abort after collect com" $ do
        afterCollectCom <-
          runHeadLogic bobEnv ledger (inInitialState threeParties) $ do
            step (observeTx $ OnCollectComTx testHeadId)
            getState

        let unhandledInput = observeTx OnAbortTx{headId = testHeadId}
        update bobEnv ledger afterCollectCom unhandledInput
          `shouldBe` Error (UnhandledInput unhandledInput afterCollectCom)

      it "cannot observe collect com after abort" $ do
        afterAbort <-
          runHeadLogic bobEnv ledger (inInitialState threeParties) $ do
            step (observeTx OnAbortTx{headId = testHeadId})
            getState

        let unhandledInput = observeTx (OnCollectComTx testHeadId)
        update bobEnv ledger afterAbort unhandledInput
          `shouldBe` Error (UnhandledInput unhandledInput afterAbort)

      it "notifies user on head closing and when passing the contestation deadline" $ do
        let s0 = inOpenState threeParties
            snapshotNumber = 0
            contestationDeadline = arbitrary `generateWith` 42
            observeCloseTx =
              observeTx
                OnCloseTx
                  { headId = testHeadId
                  , snapshotNumber
                  , contestationDeadline
                  }
            clientEffect = ClientEffect HeadIsClosed{headId = testHeadId, snapshotNumber, contestationDeadline}
        runHeadLogic bobEnv ledger s0 $ do
          outcome1 <- step observeCloseTx
          lift $ do
            outcome1 `hasEffect` clientEffect
            outcome1
              `hasNoEffectSatisfying` \case
                ClientEffect (ReadyToFanout _) -> True
                _ -> False

          let oneSecondsPastDeadline = addUTCTime 1 contestationDeadline
              someChainSlot = arbitrary `generateWith` 42
              stepTimePastDeadline = ChainInput $ Tick oneSecondsPastDeadline someChainSlot
          outcome2 <- step stepTimePastDeadline
          lift $ outcome2 `hasEffect` ClientEffect (ReadyToFanout testHeadId)

      it "contests when detecting close with old snapshot" $ do
        let snapshot = testSnapshot 2 mempty []
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])
            s0 =
              inOpenState' threeParties $
                coordinatedHeadState{confirmedSnapshot = latestConfirmedSnapshot}
            deadline = arbitrary `generateWith` 42
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters s0)
        runHeadLogic bobEnv ledger s0 $ do
          o1 <- step $ observeTx (OnCloseTx testHeadId 0 deadline)
          lift $ o1 `hasEffect` chainEffect (ContestTx testHeadId params latestConfirmedSnapshot)
          s1 <- getState
          lift $
            s1 `shouldSatisfy` \case
              Closed ClosedState{} -> True
              _ -> False

      it "re-contests when detecting contest with old snapshot" $ do
        let snapshot2 = testSnapshot 2 mempty []
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot2 (Crypto.aggregate [])
            s0 = inClosedState' threeParties latestConfirmedSnapshot
            deadline = arbitrary `generateWith` 42
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters s0)
        update bobEnv ledger s0 (observeTx $ OnContestTx testHeadId 1 deadline)
          `hasEffect` chainEffect (ContestTx testHeadId params latestConfirmedSnapshot)

      it "ignores unrelated initTx" prop_ignoresUnrelatedOnInitTx

      prop "connectivity messages passthrough without affecting the current state" $
        \(ttl, connectivityMessage, headState) -> do
          let input = connectivityChanged ttl connectivityMessage
          let outcome = update bobEnv ledger headState input
          stateChanges outcome `shouldBe` []
          outcome `hasEffectSatisfying` \case ClientEffect{} -> True; _ -> False

      prop "ignores abortTx of another head" $ \otherHeadId -> do
        let abortOtherHead = observeTx $ OnAbortTx{headId = otherHeadId}
        update bobEnv ledger (inInitialState threeParties) abortOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores collectComTx of another head" $ \otherHeadId -> do
        let collectOtherHead = observeTx $ OnCollectComTx{headId = otherHeadId}
        update bobEnv ledger (inInitialState threeParties) collectOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores closeTx of another head" $ \otherHeadId snapshotNumber contestationDeadline -> do
        let openState = inOpenState threeParties
        let closeOtherHead = observeTx $ OnCloseTx{headId = otherHeadId, snapshotNumber, contestationDeadline}
        update bobEnv ledger openState closeOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores contestTx of another head" $ \otherHeadId snapshotNumber contestationDeadline -> do
        let contestOtherHead = observeTx $ OnContestTx{headId = otherHeadId, snapshotNumber, contestationDeadline}
        update bobEnv ledger (inClosedState threeParties) contestOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores fanoutTx of another head" $ \otherHeadId -> do
        let collectOtherHead = observeTx $ OnFanoutTx{headId = otherHeadId}
        update bobEnv ledger (inClosedState threeParties) collectOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

    describe "Coordinated Head Protocol using real Tx" $
      prop "any tx with expiring upper validity range gets pruned" $ \slotNo -> monadicIO $ do
        (utxo, expiringTransaction) <- pick $ do
          (vk, sk) <- genKeyPair
          txOut <- genOutput vk
          utxo <- (,txOut) <$> genTxIn
          mkRangedTx
            utxo
            (mkVkAddress Fixture.testNetworkId vk, txOutValue txOut)
            sk
            (Nothing, Just $ TxValidityUpperBound slotNo)
            & \case
              Left _ -> Prelude.error "cannot generate expired tx"
              Right tx -> pure (utxo, tx)
        let ledger = cardanoLedger Fixture.defaultGlobals Fixture.defaultLedgerEnv
            st0 =
              Open
                OpenState
                  { parameters = HeadParameters defaultContestationPeriod threeParties
                  , coordinatedHeadState =
                      CoordinatedHeadState
                        { localUTxO = UTxO.singleton utxo
                        , allTxs = mempty
                        , localTxs = [expiringTransaction]
                        , confirmedSnapshot = InitialSnapshot testHeadId $ UTxO.singleton utxo
                        , seenSnapshot = NoSeenSnapshot
                        , decommitTx = Nothing
                        }
                  , chainState = Prelude.error "should not be used"
                  , headId = testHeadId
                  , headSeed = testHeadSeed
                  , currentSlot = ChainSlot . fromIntegral . unSlotNo $ slotNo + 1
                  }

        st <-
          run $
            runHeadLogic bobEnv ledger st0 $ do
              step (NetworkInput defaultTTL alice $ ReqSn 1 [] Nothing)
              getState

        assert $ case st of
          Open
            OpenState
              { coordinatedHeadState =
                CoordinatedHeadState{localTxs}
              } -> null localTxs
          _ -> False

-- * Properties

prop_ignoresUnrelatedOnInitTx :: Property
prop_ignoresUnrelatedOnInitTx =
  forAll arbitrary $ \env ->
    forAll (genUnrelatedInit env) $ \unrelatedInit -> do
      let outcome = update env simpleLedger inIdleState (observeTx unrelatedInit)
      counterexample ("Outcome: " <> show outcome) $
        outcome
          `hasEffectSatisfying` \case
            ClientEffect IgnoredHeadInitializing{} -> True
            _ -> False
 where
  genUnrelatedInit env =
    oneof
      [ genOnInitWithDifferentContestationPeriod env
      , genOnInitWithoutParty env
      , genOnInitWithoutOnChainId env
      ]

  genOnInitWithDifferentContestationPeriod Environment{party, contestationPeriod, participants} = do
    headId <- arbitrary
    headSeed <- arbitrary
    cp <- arbitrary `suchThat` (/= contestationPeriod)
    parties <- shuffle =<< (arbitrary <&> (party :))
    pure
      OnInitTx
        { headId
        , headSeed
        , headParameters = HeadParameters{contestationPeriod = cp, parties}
        , participants
        }

  genOnInitWithoutParty Environment{party, otherParties, contestationPeriod, participants} = do
    headId <- arbitrary
    headSeed <- arbitrary
    allParties <- shuffle (party : otherParties)
    toRemove <- elements allParties
    let differentParties = List.delete toRemove allParties
    pure
      OnInitTx
        { headId
        , headSeed
        , headParameters = HeadParameters{contestationPeriod, parties = differentParties}
        , participants
        }

  genOnInitWithoutOnChainId Environment{party, otherParties, contestationPeriod, participants} = do
    headId <- arbitrary
    headSeed <- arbitrary
    differentParticipants <- case participants of
      [] -> (: []) <$> arbitrary
      ps -> do
        toRemove <- elements participants
        pure $ List.delete toRemove ps
    pure
      OnInitTx
        { headId
        , headSeed
        , headParameters = HeadParameters{contestationPeriod, parties = party : otherParties}
        , participants = differentParticipants
        }

-- * Utilities

-- | Create a network input about a received protocol message with 'defaultTTL'
-- and 'alice' as the sender.
receiveMessage :: Message tx -> Input tx
receiveMessage = receiveMessageFrom alice

-- | Create a network input about a received protocol message with 'defaultTTL'
-- from given sender.
receiveMessageFrom :: Party -> Message tx -> Input tx
receiveMessageFrom sender msg =
  NetworkInput defaultTTL $ ReceivedMessage{sender, msg}

-- | Create a chain effect with fixed chain state and slot.
chainEffect :: PostChainTx SimpleTx -> Effect SimpleTx
chainEffect postChainTx =
  OnChainEffect
    { postChainTx
    }

-- | Create an observation chain input with chain state at given slot.
observeTxAtSlot :: Natural -> OnChainTx SimpleTx -> Input SimpleTx
observeTxAtSlot slot observedTx =
  ChainInput
    { chainEvent =
        Observation
          { observedTx
          , newChainState = SimpleChainState{slot = ChainSlot slot}
          }
    }

-- | Create an observation chain input with fixed chain state and slot.
observeTx :: OnChainTx SimpleTx -> Input SimpleTx
observeTx = observeTxAtSlot 0

connectivityChanged :: TTL -> Connectivity -> Input SimpleTx
connectivityChanged ttl connectivityMessage =
  NetworkInput
    { ttl
    , networkEvent = ConnectivityEvent connectivityMessage
    }

inIdleState :: HeadState SimpleTx
inIdleState =
  Idle IdleState{chainState = SimpleChainState{slot = ChainSlot 0}}

-- XXX: This is always called with threeParties and simpleLedger
inInitialState :: [Party] -> HeadState SimpleTx
inInitialState parties =
  Initial
    InitialState
      { parameters
      , pendingCommits = Set.fromList parties
      , committed = mempty
      , chainState = SimpleChainState{slot = ChainSlot 0}
      , headId = testHeadId
      , headSeed = testHeadSeed
      }
 where
  parameters = HeadParameters defaultContestationPeriod parties

-- XXX: This is always called with threeParties and simpleLedger
inOpenState ::
  [Party] ->
  HeadState SimpleTx
inOpenState parties =
  inOpenState' parties $
    CoordinatedHeadState
      { localUTxO = u0
      , allTxs = mempty
      , localTxs = mempty
      , confirmedSnapshot
      , seenSnapshot = NoSeenSnapshot
      , decommitTx = Nothing
      }
 where
  u0 = mempty
  confirmedSnapshot = InitialSnapshot testHeadId u0

inOpenState' ::
  [Party] ->
  CoordinatedHeadState SimpleTx ->
  HeadState SimpleTx
inOpenState' parties coordinatedHeadState =
  Open
    OpenState
      { parameters
      , coordinatedHeadState
      , chainState = SimpleChainState{slot = chainSlot}
      , headId = testHeadId
      , headSeed = testHeadSeed
      , currentSlot = chainSlot
      }
 where
  parameters = HeadParameters defaultContestationPeriod parties

  chainSlot = ChainSlot 0

-- XXX: This is always called with 'threeParties'
inClosedState :: [Party] -> HeadState SimpleTx
inClosedState parties = inClosedState' parties snapshot0
 where
  snapshot0 = InitialSnapshot testHeadId u0
  u0 = mempty

inClosedState' :: [Party] -> ConfirmedSnapshot SimpleTx -> HeadState SimpleTx
inClosedState' parties confirmedSnapshot =
  Closed
    ClosedState
      { parameters
      , confirmedSnapshot
      , contestationDeadline
      , readyToFanoutSent = False
      , chainState = SimpleChainState{slot = ChainSlot 0}
      , headId = testHeadId
      , headSeed = testHeadSeed
      }
 where
  parameters = HeadParameters defaultContestationPeriod parties

  contestationDeadline = arbitrary `generateWith` 42

getConfirmedSnapshot ::
  Monoid (UTxOType tx) =>
  HeadState tx ->
  Maybe (Snapshot tx)
getConfirmedSnapshot = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}} ->
    Just (getSnapshot confirmedSnapshot)
  _ ->
    Nothing

data StepState tx = StepState
  { headState :: HeadState tx
  , env :: Environment
  , ledger :: Ledger tx
  }

runHeadLogic ::
  Monad m =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  StateT (StepState tx) m a ->
  m a
runHeadLogic env ledger headState = (`evalStateT` StepState{env, ledger, headState})

-- | Retrieves the latest 'HeadState' from within 'runHeadLogic'.
getState :: MonadState (StepState tx) m => m (HeadState tx)
getState = headState <$> get

-- | Calls 'update' and 'aggregate' to drive the 'runHeadLogic' monad forward.
step ::
  (MonadState (StepState tx) m, IsChainState tx) =>
  Input tx ->
  m (Outcome tx)
step input = do
  StepState{headState, env, ledger} <- get
  let outcome = update env ledger headState input
  let headState' = aggregateState headState outcome
  put StepState{env, ledger, headState = headState'}
  pure outcome

hasEffect :: (HasCallStack, IsChainState tx) => Outcome tx -> Effect tx -> IO ()
hasEffect outcome effect = hasEffectSatisfying outcome (== effect)

assertWait :: (HasCallStack, IsChainState tx) => Outcome tx -> WaitReason tx -> IO ()
assertWait outcome waitReason =
  case outcome of
    Wait{reason} -> reason `shouldBe` waitReason
    _ -> failure $ "Expected a wait, but got: " <> show outcome

hasEffectSatisfying :: (HasCallStack, IsChainState tx) => Outcome tx -> (Effect tx -> Bool) -> IO ()
hasEffectSatisfying outcome predicate =
  case outcome of
    Wait{} -> failure "Expected an effect, but got Wait outcome"
    Error{} -> failure "Expected an effect, but got Error outcome"
    Continue{effects} ->
      unless (any predicate effects) $
        failure $
          "Expected an effect satisfying the predicate, but got: " <> show effects

hasNoEffectSatisfying :: (HasCallStack, IsChainState tx) => Outcome tx -> (Effect tx -> Bool) -> IO ()
hasNoEffectSatisfying outcome predicate =
  case outcome of
    Wait{} -> failure "Expected an effect, but got Wait outcome"
    Error{} -> failure "Expected an effect, but got Error outcome"
    Continue{effects} ->
      when (any predicate effects) $
        failure $
          "Expected no effect satisfying the predicate, but got: " <> show effects

testSnapshot ::
  Monoid (UTxOType tx) =>
  SnapshotNumber ->
  UTxOType tx ->
  [TxIdType tx] ->
  Snapshot tx
testSnapshot number utxo confirmed =
  Snapshot
    { headId = testHeadId
    , number
    , utxo
    , confirmed
    , utxoToDecommit = mempty
    }
