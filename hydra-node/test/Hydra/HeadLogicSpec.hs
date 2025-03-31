{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Unit tests of the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (bodyTxL, inputsTxBodyL)
import Control.Lens ((.~))
import Data.List qualified as List
import Data.Map (notMember)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (SideLoadSnapshot))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Cardano.Api (fromLedgerTx, genTxIn, mkVkAddress, toLedgerTx, txOutValue, unSlotNo, pattern TxValidityUpperBound)
import Hydra.Chain (
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (CollectComTx, ContestTx),
 )
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState)
import Hydra.Chain.Direct.State ()
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
  SideLoadRequirementFailure (..),
  StateChanged (..),
  TTL,
  WaitReason (..),
  aggregateState,
  cause,
  defaultTTL,
  update,
 )
import Hydra.HeadLogic.State (SeenSnapshot (..), getHeadParameters)
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano (cardanoLedger, mkRangedTx)
import Hydra.Ledger.Cardano.TimeSpec (genUTCTime)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Network (Connectivity)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Options (defaultContestationPeriod, defaultDepositDeadline)
import Hydra.Prelude qualified as Prelude
import Hydra.Tx.Crypto (aggregate, generateSigningKey, sign)
import Hydra.Tx.Crypto qualified as Crypto
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Party (Party (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)
import Test.Hydra.Node.Fixture qualified as Fixture
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, deriveOnChainId, testHeadId, testHeadSeed)
import Test.Hydra.Tx.Gen (genKeyPair, genOutput)
import Test.QuickCheck (Property, counterexample, elements, forAll, forAllShrink, oneof, shuffle, suchThat)
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
            , depositDeadline = defaultDepositDeadline
            , participants = deriveOnChainId <$> threeParties
            }
        aliceEnv =
          Environment
            { party = alice
            , signingKey = aliceSk
            , otherParties = [bob, carol]
            , contestationPeriod = defaultContestationPeriod
            , depositDeadline = defaultDepositDeadline
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
              , pendingDeposits = mempty
              , decommitTx = Nothing
              , version = 0
              }

      it "reports if a requested tx is expired" $ do
        let inputs = utxoRef 1
            tx = SimpleTx 2 inputs mempty
            ttl = 0
            reqTx = NetworkInput ttl $ ReceivedMessage{sender = alice, msg = ReqTx tx}
            s0 = inOpenState threeParties

        update bobEnv ledger s0 reqTx `hasStateChangedSatisfying` \case
          TxInvalid{transaction} -> transaction == tx
          _ -> False

      it "waits if a requested tx is not (yet) applicable" $ do
        let reqTx = receiveMessage $ ReqTx $ SimpleTx 2 inputs mempty
            inputs = utxoRef 1
            s0 = inOpenState threeParties

        update bobEnv ledger s0 reqTx
          `assertWait` WaitOnNotApplicableTx (ValidationError "cannot apply transaction")

      it "confirms snapshot given it receives AckSn from all parties" $ do
        let reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot1 = Snapshot testHeadId 0 1 [] mempty Nothing Nothing
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot1) 1
        snapshotInProgress <- runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
          step reqSn
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getState

        getConfirmedSnapshot snapshotInProgress `shouldBe` Just (testSnapshot 0 0 [] mempty)

        snapshotConfirmed <-
          runHeadLogic bobEnv ledger snapshotInProgress $ do
            step (ackFrom bobSk bob)
            getState
        getConfirmedSnapshot snapshotConfirmed `shouldBe` Just snapshot1

      describe "ReqSn" $ do
        it "prunes local txs in order" $ do
          -- Given a list of transactions each depending on the previous. If a
          -- prefix gets snapshotted, the suffix still stays in the local txs.
          let tx1 = SimpleTx 1 mempty (utxoRef 2) -- No inputs, requires no specific starting state
              tx2 = SimpleTx 2 (utxoRef 2) (utxoRef 3)
              tx3 = SimpleTx 3 (utxoRef 3) (utxoRef 4)
              s0 = inOpenState threeParties

          -- XXX: this is hiding unxpected 'Error' outcomes
          s <- runHeadLogic bobEnv ledger s0 $ do
            step $ receiveMessage $ ReqTx tx1
            step $ receiveMessage $ ReqTx tx2
            step $ receiveMessage $ ReqTx tx3
            step $ receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
            getState

          case s of
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{localTxs}}) -> do
              localTxs `shouldBe` [tx2, tx3]
            _ -> fail "expected Open state"

      describe "Decommit" $ do
        it "observes DecommitRecorded and ReqDec in an Open state" $ do
          let outputs = utxoRef 1
              transaction = SimpleTx 1 mempty outputs
              input = receiveMessage ReqDec{transaction}
              st = inOpenState threeParties
          update aliceEnv ledger st input `hasStateChangedSatisfying` \case
            DecommitRecorded{headId, utxoToDecommit} -> headId == testHeadId && utxoToDecommit == outputs
            _ -> False

        it "ignores ReqDec when not in Open state" $ monadicIO $ do
          let reqDec = ReqDec{transaction = SimpleTx 1 mempty (utxoRef 1)}
          let input = receiveMessage reqDec
          st <- pickBlind $ elements [inInitialState threeParties, inIdleState, inClosedState threeParties]
          pure $
            update aliceEnv ledger st input
              `shouldNotBe` cause (NetworkEffect reqDec)

        it "reports if a requested decommit tx is expired" $ do
          let inputs = utxoRef 1
              decommitTx = SimpleTx 1 mempty inputs
              ttl = 0
              reqDec = ReqDec{transaction = decommitTx}
              reqDecEvent = NetworkInput ttl $ ReceivedMessage{sender = alice, msg = reqDec}
              decommitTxInFlight = SimpleTx 2 mempty (utxoRef 2)
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { decommitTx = Just decommitTxInFlight
                    }
          update bobEnv ledger s0 reqDecEvent `hasStateChangedSatisfying` \case
            DecommitInvalid{decommitTx = invalidTx} -> invalidTx == decommitTx
            _ -> False

        it "wait for second decommit when another one is in flight" $
          do
            let decommitTx1 = SimpleTx 1 mempty (utxoRef 1)
                decommitTx2 = SimpleTx 2 mempty (utxoRef 2)
                s0 = inOpenState threeParties

            s1 <- runHeadLogic aliceEnv ledger s0 $ do
              step $ receiveMessageFrom alice ReqDec{transaction = decommitTx1}
              getState

            update bobEnv ledger s1 (receiveMessageFrom bob ReqDec{transaction = decommitTx2})
              `assertWait` WaitOnNotApplicableDecommitTx
                { notApplicableReason =
                    DecommitAlreadyInFlight
                      { otherDecommitTxId = txId decommitTx1
                      }
                }

        it "cannot commit while another decommit is pending" $ do
          let decommitTx = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
              s0 = inOpenState' threeParties $ coordinatedHeadState{decommitTx = Just decommitTx}
              observeDeposit =
                observeTx $
                  OnDepositTx{headId = testHeadId, deposited = utxoRefs [2], depositTxId = 1, deadline = arbitrary `generateWith` 42}
          update aliceEnv ledger s0 observeDeposit
            `assertWait` WaitOnUnresolvedDecommit{decommitTx}

        it "waits if a requested decommit tx is not (yet) applicable" $ do
          let decommitTx = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
              s0 = inOpenState threeParties
              reqDecEvent = receiveMessage ReqDec{transaction = decommitTx}

          update aliceEnv ledger s0 reqDecEvent
            `assertWait` WaitOnNotApplicableDecommitTx (DecommitTxInvalid mempty (ValidationError "cannot apply transaction"))

        it "updates decommitTx on valid ReqDec" $ do
          let decommitTx' = SimpleTx 1 mempty (utxoRef 1)
          let reqDec = ReqDec{transaction = decommitTx'}
              reqDecEvent = receiveMessage reqDec
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
          let localUTxO = utxoRefs [2]
          let decommitTx' = SimpleTx{txSimpleId = 1, txInputs = localUTxO, txOutputs = utxoRefs [4]}
          let s0 = inOpenState' threeParties $ coordinatedHeadState{localUTxO}

          s0 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> isNothing decommitTx
            _ -> False

          let reqDecEvent = receiveMessage ReqDec{transaction = decommitTx'}

          let s1 = update aliceEnv ledger s0 reqDecEvent

          let reqSn = ReqSn{snapshotVersion = 0, snapshotNumber = 1, transactionIds = [], decommitTx = Just decommitTx', incrementUTxO = Nothing}
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
              reqSn = receiveMessage $ ReqSn 0 1 [1] Nothing Nothing

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
              reqSn = receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
              snapshot1 = testSnapshot 1 0 [t1] (utxoRefs [1])
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
        let reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot = testSnapshot 1 0 [] mempty
            snapshot' = testSnapshot 2 0 [] mempty
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
        let reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot = testSnapshot 1 0 [] mempty
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
        let reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot1 = testSnapshot 1 0 [] mempty
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
        let reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot1 = testSnapshot 1 0 [] mempty
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

      it "rejects snapshot request with transaction not applicable to previous snapshot" $ do
        let reqTx42 = receiveMessage $ ReqTx (SimpleTx 42 mempty (utxoRef 1))
            reqTx1 = receiveMessage $ ReqTx (SimpleTx 1 (utxoRef 1) (utxoRef 2))
            input = receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
            s0 = inOpenState threeParties

        s2 <- runHeadLogic bobEnv ledger s0 $ do
          step reqTx42
          step reqTx1
          getState

        update bobEnv ledger s2 input
          `shouldBe` Error (RequireFailed (SnapshotDoesNotApply 1 1 (ValidationError "cannot apply transaction")))

      it "waits if we receive a snapshot with unseen transactions" $ do
        let s0 = inOpenState threeParties
            reqSn = receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
        update bobEnv ledger s0 reqSn
          `assertWait` WaitOnTxs [1]

      it "waits if we receive an AckSn for an unseen snapshot" $ do
        let snapshot = testSnapshot 1 0 [] mempty
            input = receiveMessage $ AckSn (sign aliceSk snapshot) 1
        update bobEnv ledger (inOpenState threeParties) input
          `assertWait` WaitOnSeenSnapshot

      -- TODO: Write property tests for various future / old snapshot behavior.
      -- That way we could cover variations of snapshot numbers and state of
      -- snapshot collection.

      it "rejects if we receive a too far future snapshot" $ do
        let input = receiveMessageFrom bob $ ReqSn 0 2 [] Nothing Nothing
            st = inOpenState threeParties
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "waits if we receive a future snapshot while collecting signatures" $ do
        let reqSn1 = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            reqSn2 = receiveMessageFrom bob $ ReqSn 0 2 [] Nothing Nothing
        st <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn1
            getState

        update bobEnv ledger st reqSn2
          `assertWait` WaitOnSnapshotNumber 1

      it "acks signed snapshot from the constant leader" $ do
        let leader = alice
            snapshot = testSnapshot 1 0 [] mempty
            input = receiveMessageFrom leader $ ReqSn 0 (number snapshot) [] Nothing Nothing
            sig = sign bobSk snapshot
            st = inOpenState threeParties
            ack = AckSn sig (number snapshot)
        update bobEnv ledger st input `hasEffect` NetworkEffect ack

      it "does not ack snapshots from non-leaders" $ do
        let input = receiveMessageFrom notTheLeader $ ReqSn 0 1 [] Nothing Nothing
            notTheLeader = bob
            st = inOpenState threeParties
        update bobEnv ledger st input `shouldSatisfy` \case
          Error (RequireFailed ReqSnNotLeader{requestedSn = 1, leader}) -> leader == notTheLeader
          _ -> False

      it "rejects too-old snapshots" $ do
        let input = receiveMessageFrom theLeader $ ReqSn 0 2 [] Nothing Nothing
            theLeader = alice
            snapshot = testSnapshot 2 0 [] mempty
            st =
              inOpenState' threeParties $
                coordinatedHeadState{confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])}
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "rejects too-old snapshots when collecting signatures" $ do
        let input = receiveMessageFrom theLeader $ ReqSn 0 2 [] Nothing Nothing
            theLeader = alice
            snapshot = testSnapshot 2 0 [] mempty
            st =
              inOpenState' threeParties $
                coordinatedHeadState
                  { confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])
                  , seenSnapshot = SeenSnapshot (testSnapshot 3 0 [] mempty) mempty
                  }
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 3)

      it "rejects too-new snapshots from the leader" $ do
        let input = receiveMessageFrom theLeader $ ReqSn 0 3 [] Nothing Nothing
            theLeader = carol
            st = inOpenState threeParties
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 3 0)

      it "rejects invalid snapshots version" $ do
        let validSnNumber = 0
            invalidSnVersion = 1
            input = receiveMessageFrom theLeader $ ReqSn invalidSnVersion validSnNumber [] Nothing Nothing
            theLeader = carol
            expectedSnVersion = 0
            st = inOpenState threeParties
        update bobEnv ledger st input `shouldBe` Error (RequireFailed $ ReqSvNumberInvalid invalidSnVersion expectedSnVersion)

      it "rejects overlapping snapshot requests from the leader" $ do
        let theLeader = alice
            nextSN = 1
            firstReqTx = receiveMessage $ ReqTx (aValidTx 42)
            firstReqSn = receiveMessageFrom theLeader $ ReqSn 0 nextSN [42] Nothing Nothing
            secondReqTx = receiveMessage $ ReqTx (aValidTx 51)
            secondReqSn = receiveMessageFrom theLeader $ ReqSn 0 nextSN [51] Nothing Nothing

        s3 <- runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
          step firstReqTx
          step firstReqSn
          step secondReqTx
          getState

        update bobEnv ledger s3 secondReqSn `shouldSatisfy` \case
          Error RequireFailed{} -> True
          _ -> False

      it "rejects same version snapshot requests with differring decommit txs" $ do
        let decommitTx1 = SimpleTx 1 (utxoRef 1) (utxoRef 3)
            decommitTx2 = SimpleTx 2 (utxoRef 2) (utxoRef 4)
            activeUTxO = utxoRefs [1, 2]
            snapshot =
              Snapshot
                { headId = testHeadId
                , version = 0
                , number = 1
                , confirmed = []
                , utxo = activeUTxO
                , utxoToCommit = Nothing
                , utxoToDecommit = Just $ utxoRefs [3]
                }
            s0 =
              inOpenState'
                threeParties
                coordinatedHeadState
                  { confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])
                  , seenSnapshot = LastSeenSnapshot 1
                  , localUTxO = activeUTxO
                  }
            reqSn0 = receiveMessageFrom alice $ ReqSn 0 1 [] (Just decommitTx1) Nothing
            reqSn1 = receiveMessageFrom bob $ ReqSn 0 2 [] (Just decommitTx2) Nothing

        outcome <- runHeadLogic bobEnv ledger s0 $ do
          step reqSn0
          step reqSn1
        outcome `shouldSatisfy` \case
          Error RequireFailed{requirementFailure} | requirementFailure == ReqSnDecommitNotSettled -> True
          _ -> False

      it "ignores in-flight ReqTx when closed" $ do
        let s0 = inClosedState threeParties
            input = receiveMessage $ ReqTx (aValidTx 42)
        update bobEnv ledger s0 input `shouldBe` Error (UnhandledInput input s0)

      it "ignores in-flight ReqDec when closed" $ do
        let s0 = inClosedState threeParties
            input = receiveMessage $ ReqDec{transaction = aValidTx 42}
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
        runHeadLogic bobEnv ledger s0 $ do
          outcome1 <- step observeCloseTx
          lift $ do
            outcome1 `hasStateChangedSatisfying` \case
              HeadClosed{} -> True
              _ -> False
            outcome1
              `hasNoStateChangedSatisfying` \case
                HeadIsReadyToFanout{} -> True
                _ -> False

          let oneSecondsPastDeadline = addUTCTime 1 contestationDeadline
              someChainSlot = arbitrary `generateWith` 42
              stepTimePastDeadline = ChainInput $ Tick oneSecondsPastDeadline someChainSlot
          outcome2 <- step stepTimePastDeadline
          lift $
            outcome2 `hasStateChangedSatisfying` \case
              HeadIsReadyToFanout{headId} -> testHeadId == headId
              _ -> False

      it "contests when detecting close with old snapshot" $ do
        let snapshotVersion = 0
            snapshot = testSnapshot 2 snapshotVersion [] mempty
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])
            s0 =
              inOpenState' threeParties $
                coordinatedHeadState{confirmedSnapshot = latestConfirmedSnapshot}
            deadline = arbitrary `generateWith` 42
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters s0)
        runHeadLogic bobEnv ledger s0 $ do
          o1 <- step $ observeTx (OnCloseTx testHeadId 0 deadline)
          lift $ o1 `hasEffect` chainEffect (ContestTx testHeadId params snapshotVersion latestConfirmedSnapshot)
          s1 <- getState
          lift $
            s1 `shouldSatisfy` \case
              Closed ClosedState{} -> True
              _ -> False

      it "re-contests when detecting contest with old snapshot" $ do
        let snapshotVersion = 0
            snapshot2 = testSnapshot 2 snapshotVersion [] mempty
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot2 (Crypto.aggregate [])
            s0 = inClosedState' threeParties latestConfirmedSnapshot
            deadline = arbitrary `generateWith` 42
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters s0)
        update bobEnv ledger s0 (observeTx $ OnContestTx testHeadId 1 deadline)
          `hasEffect` chainEffect (ContestTx testHeadId params snapshotVersion latestConfirmedSnapshot)

      it "ignores unrelated initTx" prop_ignoresUnrelatedOnInitTx

      prop "connectivity messages passthrough without affecting the current state" $
        \(ttl, connectivityMessage, headState) -> do
          let input = connectivityChanged ttl connectivityMessage
          let outcome = update bobEnv ledger headState input
          outcome `shouldSatisfy` \case
            Continue{stateChanges, effects} ->
              null effects
                && all
                  ( \case
                      -- NOTE: match only network related outcomes
                      PeerConnected{} -> True
                      PeerDisconnected{} -> True
                      NetworkVersionMismatch{} -> True
                      NetworkConnected{} -> True
                      NetworkDisconnected{} -> True
                      _ -> False
                  )
                  stateChanges
            _ -> False

      prop "ignores abortTx of another head" $ \otherHeadId -> do
        let abortOtherHead = observeTx $ OnAbortTx{headId = otherHeadId}
        update bobEnv ledger (inInitialState threeParties) abortOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores collectComTx of another head" $ \otherHeadId -> do
        let collectOtherHead = observeTx $ OnCollectComTx{headId = otherHeadId}
        update bobEnv ledger (inInitialState threeParties) collectOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores depositTx of another head" $ \otherHeadId -> do
        let depositOtherHead =
              observeTx $
                OnDepositTx
                  { headId = otherHeadId
                  , deposited = mempty
                  , depositTxId = 1
                  , deadline = genUTCTime `generateWith` 42
                  }
        update bobEnv ledger (inOpenState threeParties) depositOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores recoverTx of another head" $ \otherHeadId -> do
        let recoverOtherHead =
              observeTx $
                OnRecoverTx
                  { headId = otherHeadId
                  , recoveredTxId = 1
                  }
        update bobEnv ledger (inOpenState threeParties) recoverOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores decrementTx of another head" $ \otherHeadId -> do
        let decrementOtherHead = observeTx $ OnDecrementTx{headId = otherHeadId, newVersion = 1, distributedOutputs = mempty}
        update bobEnv ledger (inOpenState threeParties) decrementOtherHead
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

      prop "ignores fanoutTx of another head" $ \otherHeadId fanoutUTxO -> do
        let collectOtherHead = observeTx $ OnFanoutTx{headId = otherHeadId, fanoutUTxO}
        update bobEnv ledger (inClosedState threeParties) collectOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "fanout utxo always relies on observed utxo" $ \fanoutUTxO ->
        forAllShrink genClosedState shrink $ \closedState -> do
          let fanoutHead = observeTx $ OnFanoutTx{headId = testHeadId, fanoutUTxO}
          let outcome = update bobEnv ledger closedState fanoutHead
          counterexample ("Outcome: " <> show outcome) $
            outcome `hasStateChangedSatisfying` \case
              HeadFannedOut{utxo} -> utxo == fanoutUTxO
              _ -> False

      describe "SideLoad InitialSnapshot" $ do
        it "accept side load initial snapshot with idempotence" $ do
          let s0 = inOpenState threeParties
              initialSn = InitialSnapshot testHeadId mempty
              snapshot0 = getSnapshot initialSn
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0
          sideLoadedState <- runHeadLogic bobEnv ledger s0 $ do
            step $ ClientInput (SideLoadSnapshot initialSn)
            getState
          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot0

        it "reject side load wrong initial snapshot" $ do
          let s0 = inOpenState threeParties
              initialSn = InitialSnapshot testHeadId mempty
              snapshot0 = getSnapshot initialSn
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0
          let wrongInitialSnapshot = InitialSnapshot testHeadId (utxoRef 2)
          update bobEnv ledger s0 (ClientInput (SideLoadSnapshot wrongInitialSnapshot))
            `shouldBe` Error (SideLoadSnapshotFailed SideLoadInitialSnapshotMissmatch)
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0

        prop "ignores side load initial snapshot of another head" $ \otherHeadId -> do
          let s0 = inOpenState threeParties
              initialSn = InitialSnapshot testHeadId mempty
              snapshot0 = getSnapshot initialSn
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0
          let initialSnapshotOtherHead = InitialSnapshot otherHeadId mempty
          update bobEnv ledger s0 (ClientInput (SideLoadSnapshot initialSnapshotOtherHead))
            `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0

      describe "SideLoad ConfirmedSnapshot" $ do
        -- Given a list of transactions each depending on the previous.
        let tx1 = SimpleTx 1 mempty (utxoRef 2) -- No inputs, requires no specific starting state
            tx2 = SimpleTx 2 (utxoRef 2) (utxoRef 3)
            tx3 = SimpleTx 3 (utxoRef 3) (utxoRef 4)
            snapshot1 = Snapshot testHeadId 0 1 [tx1] (utxoRef 2) Nothing Nothing
            multisig1 = aggregate [sign aliceSk snapshot1, sign bobSk snapshot1, sign carolSk snapshot1]
        -- Given a starting state with:
        -- \* All txs were submitted and locally applied.
        -- \* Snapshot 1 containing tx1 got confirmed.
        -- \* tx2 and tx2 are pending confirmation.
        -- \* Snapshot 2 is inflight.
        let startingState =
              inOpenState'
                threeParties
                coordinatedHeadState
                  { localUTxO = utxoRef 4
                  , allTxs = Map.fromList [(txId tx2, tx2), (txId tx3, tx3)]
                  , localTxs = [tx2, tx3]
                  , confirmedSnapshot = ConfirmedSnapshot snapshot1 multisig1
                  , seenSnapshot = RequestedSnapshot{lastSeen = 1, requested = 2}
                  }

        it "accept new side load confirmed snapshot" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = Snapshot testHeadId 0 2 [tx2] (utxoRef 3) Nothing Nothing
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2, sign carolSk snapshot2]

          sideLoadedState <- runHeadLogic bobEnv ledger startingState $ do
            step $ ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2)
            getState

          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot2

        it "reject side load confirmed snapshot because old snapshot number" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = Snapshot testHeadId 0 2 [tx2] (utxoRef 3) Nothing Nothing
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2, sign carolSk snapshot2]

          sideLoadedState <- runHeadLogic bobEnv ledger startingState $ do
            step $ ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2)
            getState

          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot2

          update bobEnv ledger sideLoadedState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot1 multisig1))
            `shouldBe` Error (SideLoadSnapshotFailed{sideLoadRequirementFailure = SideLoadSnNumberInvalid 1 2})

        it "reject side load confirmed snapshot because missing signature" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = Snapshot testHeadId 0 2 [tx2] (utxoRef 3) Nothing Nothing
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          update bobEnv ledger startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
            `shouldSatisfy` \case
              Error (SideLoadSnapshotFailed SideLoadInvalidMultisignature{vkeys}) -> vkeys == [vkey alice, vkey bob, vkey carol]
              _ -> False

        it "reject side load confirmed snapshot because wrong snapshot version" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = Snapshot testHeadId 1 2 [tx2] (utxoRef 3) Nothing Nothing
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          update bobEnv ledger startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
            `shouldBe` Error (SideLoadSnapshotFailed{sideLoadRequirementFailure = SideLoadSvNumberInvalid 1 0})

        prop "reject side load confirmed snapshot because wrong snapshot utxoToDecommit" $ \utxoToDecommit -> do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1
          let snapshot2 = Snapshot testHeadId 0 2 [tx2] (utxoRef 3) Nothing (Just utxoToDecommit)
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          update bobEnv ledger startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
            `shouldBe` Error (SideLoadSnapshotFailed $ SideLoadUTxOToDecommitInvalid (Just utxoToDecommit) Nothing)

        prop "reject side load confirmed snapshot because wrong snapshot utxoToCommit" $ \utxoToCommit -> do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = Snapshot testHeadId 0 2 [tx2] (utxoRef 3) (Just utxoToCommit) Nothing
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          update bobEnv ledger startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
            `shouldBe` Error (SideLoadSnapshotFailed $ SideLoadUTxOToCommitInvalid (Just utxoToCommit) Nothing)

        it "accept side load confirmed snapshot with idempotence" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          sideLoadedState <- runHeadLogic bobEnv ledger startingState $ do
            step $ ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot1 multisig1)
            getState

          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot1

        prop "ignores side load confirmed snapshot of another head" $ \otherHeadId -> do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot1OtherHead = Snapshot otherHeadId 0 1 [tx1] (utxoRef 2) Nothing Nothing
              multisig1OtherHead = aggregate [sign aliceSk snapshot1OtherHead, sign bobSk snapshot1OtherHead, sign carolSk snapshot1OtherHead]
              confirmedSnapshotOtherHead = ConfirmedSnapshot snapshot1OtherHead multisig1OtherHead

          update bobEnv ledger startingState (ClientInput (SideLoadSnapshot confirmedSnapshotOtherHead))
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
                        , pendingDeposits = mempty
                        , decommitTx = Nothing
                        , version = 0
                        }
                  , chainState = Prelude.error "should not be used"
                  , headId = testHeadId
                  , headSeed = testHeadSeed
                  , currentSlot = ChainSlot . fromIntegral . unSlotNo $ slotNo + 1
                  }

        st <-
          run $
            runHeadLogic bobEnv ledger st0 $ do
              step (receiveMessage $ ReqSn 0 1 [] Nothing Nothing)
              getState

        assert $ case st of
          Open
            OpenState
              { coordinatedHeadState =
                CoordinatedHeadState{localTxs}
              } -> null localTxs
          _ -> False

    prop "empty inputs in decommit tx are prevented" $ \tx -> do
      let ledger = cardanoLedger Fixture.defaultGlobals Fixture.defaultLedgerEnv
      let st =
            Open
              OpenState
                { parameters = HeadParameters defaultContestationPeriod threeParties
                , coordinatedHeadState =
                    CoordinatedHeadState
                      { localUTxO = mempty
                      , allTxs = mempty
                      , localTxs = []
                      , confirmedSnapshot = InitialSnapshot testHeadId mempty
                      , seenSnapshot = NoSeenSnapshot
                      , pendingDeposits = mempty
                      , decommitTx = Nothing
                      , version = 0
                      }
                , chainState = Prelude.error "should not be used"
                , headId = testHeadId
                , headSeed = testHeadSeed
                , currentSlot = ChainSlot 1
                }

      let tx' = fromLedgerTx (toLedgerTx tx & bodyTxL . inputsTxBodyL .~ mempty)
      let input = receiveMessage $ ReqDec{transaction = tx'}
      update bobEnv ledger st input `shouldSatisfy` \case
        Wait WaitOnNotApplicableDecommitTx{notApplicableReason = DecommitTxInvalid{}} _ -> True
        _ -> False

-- * Properties

prop_ignoresUnrelatedOnInitTx :: Property
prop_ignoresUnrelatedOnInitTx =
  forAll arbitrary $ \env ->
    forAll (genUnrelatedInit env) $ \unrelatedInit -> do
      let outcome = update env simpleLedger inIdleState (observeTx unrelatedInit)
      counterexample ("Outcome: " <> show outcome) $
        outcome `hasStateChangedSatisfying` \case
          IgnoredHeadInitializing{} -> True
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

genClosedState :: Gen (HeadState SimpleTx)
genClosedState = do
  closedState <- arbitrary
  pure $ Closed $ closedState{headId = testHeadId}

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
      , pendingDeposits = mempty
      , decommitTx = Nothing
      , version = 0
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
      , version = 0
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

hasStateChangedSatisfying :: (HasCallStack, IsChainState tx) => Outcome tx -> (StateChanged tx -> Bool) -> IO ()
hasStateChangedSatisfying outcome predicate =
  case outcome of
    Wait{} -> failure "Expected an effect, but got Wait outcome"
    Error{} -> failure "Expected an effect, but got Error outcome"
    Continue{stateChanges} ->
      unless (any predicate stateChanges) $
        failure $
          "Expected an state change satisfying the predicate, but got: " <> show stateChanges

hasNoStateChangedSatisfying :: (HasCallStack, IsChainState tx) => Outcome tx -> (StateChanged tx -> Bool) -> IO ()
hasNoStateChangedSatisfying outcome predicate =
  case outcome of
    Wait{} -> failure "Expected an effect, but got Wait outcome"
    Error{} -> failure "Expected an effect, but got Error outcome"
    Continue{stateChanges} ->
      when (any predicate stateChanges) $
        failure $
          "Expected no state change satisfying the predicate, but got: " <> show stateChanges

testSnapshot ::
  Monoid (UTxOType tx) =>
  SnapshotNumber ->
  SnapshotVersion ->
  [tx] ->
  UTxOType tx ->
  Snapshot tx
testSnapshot number version confirmed utxo =
  Snapshot
    { headId = testHeadId
    , version
    , number
    , confirmed
    , utxo
    , utxoToCommit = mempty
    , utxoToDecommit = mempty
    }
