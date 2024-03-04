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
import Hydra.Events (EventID)
import Hydra.HeadLogic (
  ClosedState (..),
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  HeadState (..),
  IdleState (..),
  InitialState (..),
  Input (..),
  LogicError (..),
  OpenState (..),
  Outcome (..),
  RequirementFailure (..),
  SeenSnapshot (NoSeenSnapshot, SeenSnapshot),
  WaitReason (..),
  aggregateState,
  defaultTTL,
  update,
 )
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.Ledger (ChainSlot (..), IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano (cardanoLedger, genKeyPair, genOutput, mkRangedTx)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Network.Message (Message (AckSn, ReqSn, ReqTx))
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

    describe "Coordinated Head Protocol" $ do
      let ledger = simpleLedger
      let initialEventID = 0

      let coordinatedHeadState =
            CoordinatedHeadState
              { localUTxO = mempty
              , allTxs = mempty
              , localTxs = mempty
              , confirmedSnapshot = InitialSnapshot testHeadId mempty
              , seenSnapshot = NoSeenSnapshot
              }

      it "reports if a requested tx is expired" $ do
        let inputs = utxoRef 1
            tx = SimpleTx 2 inputs mempty
            ttl = 0
            reqTx = NetworkInput ttl alice $ ReqTx tx
            s0 = inOpenState threeParties

        -- NOTE(Elaine): check back here if tests fail
        update bobEnv ledger 1 s0 reqTx `hasEffectSatisfying` \case
          ClientEffect TxInvalid{transaction} -> transaction == tx
          _ -> False

      it "waits if a requested tx is not (yet) applicable" $ do
        let reqTx = NetworkInput defaultTTL alice $ ReqTx $ SimpleTx 2 inputs mempty
            inputs = utxoRef 1
            s0 = inOpenState threeParties

        update bobEnv ledger 1 s0 reqTx
          `assertWait` WaitOnNotApplicableTx (ValidationError "cannot apply transaction")

      it "confirms snapshot given it receives AckSn from all parties" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 []
            snapshot1 = Snapshot testHeadId 1 mempty []
            ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot1) 1
        snapshotInProgress <- runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
          step reqSn
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getState

        getConfirmedSnapshot snapshotInProgress `shouldBe` Just (testSnapshot 0 mempty [])

        snapshotConfirmed <-
          runHeadLogic bobEnv ledger snapshotInProgress initialEventID $ do
            step (ackFrom bobSk bob)
            getState
        getConfirmedSnapshot snapshotConfirmed `shouldBe` Just snapshot1

      describe "Tracks Transaction Ids" $ do
        it "keeps transactions in allTxs given it receives a ReqTx" $ do
          let s0 = inOpenState threeParties
              t1 = SimpleTx 1 mempty (utxoRef 1)

          sa <- runHeadLogic bobEnv ledger s0 initialEventID $ do
            step $ NetworkInput defaultTTL alice $ ReqTx t1
            getState

          sa `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{allTxs}}) -> txId t1 `member` allTxs
            _ -> False

        it "removes transactions in allTxs given it receives a ReqSn" $ do
          let s0 = inOpenState threeParties
              t1 = SimpleTx 1 mempty (utxoRef 1)
              reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [1]

          s1 <- runHeadLogic bobEnv ledger s0 initialEventID $ do
            step $ NetworkInput defaultTTL alice $ ReqTx t1
            step reqSn
            getState

          s1 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{allTxs}}) -> txId t1 `notMember` allTxs
            _ -> False

        it "removes transactions from allTxs when included in a acked snapshot even when emitting a ReqSn" $ do
          let t1 = SimpleTx 1 mempty (utxoRef 1)
              pendingTransaction = SimpleTx 2 mempty (utxoRef 2)
              reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [1]
              snapshot1 = testSnapshot 1 (utxoRefs [1]) [1]
              ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot1) 1

          sa <- runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
            step $ NetworkInput defaultTTL alice $ ReqTx t1
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)

            step $ NetworkInput defaultTTL alice $ ReqTx pendingTransaction

            step (ackFrom bobSk bob)
            getState

          sa `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{allTxs}}) -> txId t1 `notMember` allTxs
            _ -> False

      it "rejects last AckSn if one signature was from a different snapshot" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 []
            snapshot = testSnapshot 1 mempty []
            snapshot' = testSnapshot 2 mempty []
            ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot) 1
            invalidAckFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot') 1
        (waitingForLastAck, waitingForLastAckEventID) <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)
            getStateAndEventID -- NOTE(Elaine): to me this suggests that every time that HeadState should be aware of the current eventID
        update bobEnv ledger waitingForLastAckEventID waitingForLastAck (invalidAckFrom bobSk bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if one signature was from a different key" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 []
            snapshot = testSnapshot 1 mempty []
            ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot) 1
        (waitingForLastAck, waitingForLastAckEventID) <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)
            getStateAndEventID

        update bobEnv ledger waitingForLastAckEventID waitingForLastAck (ackFrom (generateSigningKey "foo") bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if one signature was from a completely different message" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 []
            snapshot1 = testSnapshot 1 mempty []
            ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot1) 1
            invalidAckFrom sk vk =
              NetworkInput defaultTTL vk $
                AckSn (coerce $ sign sk ("foo" :: ByteString)) 1
        (waitingForLastAck, waitingForLastAckEventID) <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (invalidAckFrom bobSk bob)
            getStateAndEventID

        update bobEnv ledger waitingForLastAckEventID waitingForLastAck (ackFrom aliceSk alice)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if already received signature from this party" $ do
        let reqSn = NetworkInput defaultTTL alice $ ReqSn 1 []
            snapshot1 = testSnapshot 1 mempty []
            ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot1) 1
        (waitingForAck, waitingForAckEventID) <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
            step reqSn
            step (ackFrom carolSk carol)
            getStateAndEventID

        update bobEnv ledger waitingForAckEventID waitingForAck (ackFrom carolSk carol)
          `shouldSatisfy` \case
            Error (RequireFailed SnapshotAlreadySigned{receivedSignature}) -> receivedSignature == carol
            _ -> False

      it "waits if we receive a snapshot with transaction not applicable on previous snapshot" $ do
        let reqTx42 = NetworkInput defaultTTL alice $ ReqTx (SimpleTx 42 mempty (utxoRef 1))
            reqTx1 = NetworkInput defaultTTL alice $ ReqTx (SimpleTx 1 (utxoRef 1) (utxoRef 2))
            input = NetworkInput defaultTTL alice $ ReqSn 1 [1]
            (s0, s0EventID) = (inOpenState threeParties, initialEventID)

        (s2, s2EventID) <- runHeadLogic bobEnv ledger s0 s0EventID $ do
          step reqTx42
          step reqTx1
          getStateAndEventID

        update bobEnv ledger s2EventID s2 input
          `shouldBe` Error (RequireFailed (SnapshotDoesNotApply 1 1 (ValidationError "cannot apply transaction")))

      it "waits if we receive a snapshot with unseen transactions" $ do
        let s0 = inOpenState threeParties
            reqSn = NetworkInput defaultTTL alice $ ReqSn 1 [1]
        update bobEnv ledger initialEventID s0 reqSn
          `assertWait` WaitOnTxs [1]

      it "waits if we receive an AckSn for an unseen snapshot" $ do
        let snapshot = testSnapshot 1 mempty []
            input = NetworkInput defaultTTL alice $ AckSn (sign aliceSk snapshot) 1
        update bobEnv ledger initialEventID (inOpenState threeParties) input
          `assertWait` WaitOnSeenSnapshot

      -- TODO: Write property tests for various future / old snapshot behavior.
      -- That way we could cover variations of snapshot numbers and state of
      -- snapshot collection.

      it "rejects if we receive a too far future snapshot" $ do
        let input = NetworkInput defaultTTL bob $ ReqSn 2 []
            (st, stEventID) = (inOpenState threeParties, initialEventID)
        update bobEnv ledger stEventID st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "waits if we receive a future snapshot while collecting signatures" $ do
        let reqSn1 = NetworkInput defaultTTL alice $ ReqSn 1 []
            reqSn2 = NetworkInput defaultTTL bob $ ReqSn 2 []
        (st, stEventID) <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
            step reqSn1
            getStateAndEventID

        update bobEnv ledger stEventID st reqSn2
          `assertWait` WaitOnSnapshotNumber 1

      it "acks signed snapshot from the constant leader" $ do
        let leader = alice
            snapshot = testSnapshot 1 mempty []
            input = NetworkInput defaultTTL leader $ ReqSn (number snapshot) []
            sig = sign bobSk snapshot
            (st, stEventID) = (inOpenState threeParties, initialEventID)
            ack = AckSn sig (number snapshot)
        update bobEnv ledger stEventID st input `hasEffect` NetworkEffect ack

      it "does not ack snapshots from non-leaders" $ do
        let input = NetworkInput defaultTTL notTheLeader $ ReqSn 1 []
            notTheLeader = bob
            (st, stEventID) = (inOpenState threeParties, initialEventID)
        update bobEnv ledger stEventID st input `shouldSatisfy` \case
          Error (RequireFailed ReqSnNotLeader{requestedSn = 1, leader}) -> leader == notTheLeader
          _ -> False

      it "rejects too-old snapshots" $ do
        let input = NetworkInput defaultTTL theLeader $ ReqSn 2 []
            theLeader = alice
            snapshot = testSnapshot 2 mempty []
            stEventID = initialEventID
            st =
              inOpenState' threeParties $
                coordinatedHeadState{confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])}
        update bobEnv ledger stEventID st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "rejects too-old snapshots when collecting signatures" $ do
        let input = NetworkInput defaultTTL theLeader $ ReqSn 2 []
            theLeader = alice
            snapshot = testSnapshot 2 mempty []
            stEventID = initialEventID
            st =
              inOpenState' threeParties $
                coordinatedHeadState
                  { confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])
                  , seenSnapshot = SeenSnapshot (testSnapshot 3 mempty []) mempty
                  }
        update bobEnv ledger stEventID st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 3)

      it "rejects too-new snapshots from the leader" $ do
        let input = NetworkInput defaultTTL theLeader $ ReqSn 3 []
            theLeader = carol
            (st, stEventID) = (inOpenState threeParties, stEventID)
        update bobEnv ledger stEventID st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 3 0)

      it "rejects overlapping snapshot requests from the leader" $ do
        let theLeader = alice
            nextSN = 1
            firstReqTx = NetworkInput defaultTTL alice $ ReqTx (aValidTx 42)
            firstReqSn = NetworkInput defaultTTL theLeader $ ReqSn nextSN [42]
            secondReqTx = NetworkInput defaultTTL alice $ ReqTx (aValidTx 51)
            secondReqSn = NetworkInput defaultTTL theLeader $ ReqSn nextSN [51]

        (s3, s3EventID) <- runHeadLogic bobEnv ledger (inOpenState threeParties) initialEventID $ do
          step firstReqTx
          step firstReqSn
          step secondReqTx
          getStateAndEventID

        update bobEnv ledger s3EventID s3 secondReqSn `shouldSatisfy` \case
          Error RequireFailed{} -> True
          _ -> False

      it "ignores in-flight ReqTx when closed" $ do
        let (s0, s0EventID) = (inClosedState threeParties, initialEventID)
            input = NetworkInput defaultTTL alice $ ReqTx (aValidTx 42)
        update bobEnv ledger s0EventID s0 input `shouldBe` Error (UnhandledInput input s0)

      it "everyone does collect on last commit after collect com" $ do
        let aliceCommit = OnCommitTx testHeadId alice (utxoRef 1)
            bobCommit = OnCommitTx testHeadId bob (utxoRef 2)
            carolCommit = OnCommitTx testHeadId carol (utxoRef 3)
        (waitingForLastCommit, waitingForLastCommitEventID) <-
          runHeadLogic bobEnv ledger (inInitialState threeParties) initialEventID $ do
            step (observeTxAtSlot 1 aliceCommit)
            step (observeTxAtSlot 2 bobCommit)
            getStateAndEventID

        -- Bob is not the last party, but still does post a collect
        update bobEnv ledger waitingForLastCommitEventID waitingForLastCommit (observeTxAtSlot 3 carolCommit)
          `hasEffectSatisfying` \case
            OnChainEffect{postChainTx = CollectComTx{}} -> True
            _ -> False

      it "cannot observe abort after collect com" $ do
        (afterCollectCom, afterCollectComEventID) <-
          runHeadLogic bobEnv ledger (inInitialState threeParties) initialEventID $ do
            step (observeTx $ OnCollectComTx testHeadId)
            getStateAndEventID

        let unhandledInput = observeTx OnAbortTx{headId = testHeadId}
        update bobEnv ledger afterCollectComEventID afterCollectCom unhandledInput
          `shouldBe` Error (UnhandledInput unhandledInput afterCollectCom)

      it "cannot observe collect com after abort" $ do
        (afterAbort, afterAbortEventID) <-
          runHeadLogic bobEnv ledger (inInitialState threeParties) initialEventID $ do
            step (observeTx OnAbortTx{headId = testHeadId})
            getStateAndEventID

        let unhandledInput = observeTx (OnCollectComTx testHeadId)
        update bobEnv ledger afterAbortEventID afterAbort unhandledInput
          `shouldBe` Error (UnhandledInput unhandledInput afterAbort)

      it "notifies user on head closing and when passing the contestation deadline" $ do
        let (s0, s0EventID) = (inOpenState threeParties, initialEventID)
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
        runHeadLogic bobEnv ledger s0 s0EventID $ do
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
            s0EventID = initialEventID
            s0 =
              inOpenState' threeParties $
                coordinatedHeadState{confirmedSnapshot = latestConfirmedSnapshot}
            deadline = arbitrary `generateWith` 42
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters s0)
        runHeadLogic bobEnv ledger s0 s0EventID $ do
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
            (s0, s0EventID) = (inClosedState' threeParties latestConfirmedSnapshot, initialEventID)
            deadline = arbitrary `generateWith` 42
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters s0)
        update bobEnv ledger s0EventID s0 (observeTx $ OnContestTx testHeadId 1 deadline)
          `hasEffect` chainEffect (ContestTx testHeadId params latestConfirmedSnapshot)

      it "ignores unrelated initTx" prop_ignoresUnrelatedOnInitTx

      prop "ignores abortTx of another head" $ \otherHeadId -> do
        let abortOtherHead = observeTx $ OnAbortTx{headId = otherHeadId}
        update bobEnv ledger initialEventID (inInitialState threeParties) abortOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores collectComTx of another head" $ \otherHeadId -> do
        let collectOtherHead = observeTx $ OnCollectComTx{headId = otherHeadId}
        update bobEnv ledger initialEventID (inInitialState threeParties) collectOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores closeTx of another head" $ \otherHeadId snapshotNumber contestationDeadline -> do
        let openState = inOpenState threeParties
        let closeOtherHead = observeTx $ OnCloseTx{headId = otherHeadId, snapshotNumber, contestationDeadline}
        update bobEnv ledger initialEventID openState closeOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores contestTx of another head" $ \otherHeadId snapshotNumber contestationDeadline -> do
        let contestOtherHead = observeTx $ OnContestTx{headId = otherHeadId, snapshotNumber, contestationDeadline}
        update bobEnv ledger initialEventID (inClosedState threeParties) contestOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores fanoutTx of another head" $ \otherHeadId -> do
        let collectOtherHead = observeTx $ OnFanoutTx{headId = otherHeadId}
        update bobEnv ledger initialEventID (inClosedState threeParties) collectOtherHead
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
                        }
                  , chainState = Prelude.error "should not be used"
                  , headId = testHeadId
                  , headSeed = testHeadSeed
                  , currentSlot = ChainSlot . fromIntegral . unSlotNo $ slotNo + 1
                  }

        st <-
          run $
            runHeadLogic bobEnv ledger st0 0 $ do
              step (NetworkInput defaultTTL alice $ ReqSn 1 [])
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
      -- NOTE(Elaine): check back here if tests fail
      let outcome = update env simpleLedger 0 inIdleState (observeTx unrelatedInit)
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

getConfirmedSnapshot :: HeadState tx -> Maybe (Snapshot tx)
getConfirmedSnapshot = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}} ->
    Just (getSnapshot confirmedSnapshot)
  _ ->
    Nothing

data StepState tx = StepState
  { headState :: HeadState tx
  , env :: Environment
  , ledger :: Ledger tx
  , lastStateChangeId :: Word64
  -- TODO(Elaine): should this be folded into HeadState?
  -- the type of aggregate kinda suggests it
  }

runHeadLogic ::
  Monad m =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  EventID ->
  StateT (StepState tx) m a ->
  m a
runHeadLogic env ledger headState lastStateChangeId = (`evalStateT` StepState{env, ledger, headState, lastStateChangeId})

-- | Retrieves the latest 'HeadState' from within 'runHeadLogic'.
getState :: MonadState (StepState tx) m => m (HeadState tx)
getState = headState <$> get

getStateAndEventID :: MonadState (StepState tx) m => m (HeadState tx, EventID)
getStateAndEventID = do
  StepState{headState, lastStateChangeId} <- get
  pure (headState, lastStateChangeId)

-- | Calls 'update' and 'aggregate' to drive the 'runEvents' monad forward.
step ::
  (MonadState (StepState tx) m, IsChainState tx) =>
  Input tx ->
  m (Outcome tx)
step input = do
  StepState{headState, env, ledger, lastStateChangeId} <- get
  let nextStateChangeID = succ lastStateChangeId
  -- FIXME(Elaine): need to make sure every place that calls update is also updating eventID
  -- or change the monad transformer to track it somehow
  let outcome = update env ledger nextStateChangeID headState input
  let headState' = aggregateState headState outcome
  put StepState{env, ledger, headState = headState', lastStateChangeId = nextStateChangeID}
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
  SnapshotNumber ->
  UTxOType tx ->
  [TxIdType tx] ->
  Snapshot tx
testSnapshot = Snapshot testHeadId
