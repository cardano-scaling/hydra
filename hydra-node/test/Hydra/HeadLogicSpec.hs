{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Unit tests of the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude
import Hydra.Tx.Secret (Secret, mkSecret)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (bodyTxL, inputsTxBodyL)
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Control.Lens ((.~))
import Control.Monad (foldM)
import Data.List qualified as List
import Data.Map.Strict (notMember)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (Fanout, Recover, SideLoadSnapshot))
import Hydra.API.ServerOutput (ClientMessage (..), DecommitInvalidReason (..))
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..), fromLedgerTx, mkVkAddress, toLedgerTx, txOutValue, unSlotNo, pattern TxValidityUpperBound)
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Chain (
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState)
import Hydra.Chain.Direct.State (ChainStateAt (..))
import Hydra.Chain.Direct.TimeHandle (TimeHandle, mkTimeHandle, safeZone, slotToUTCTime)
import Hydra.HeadLogic (ClosedState (..), CoordinatedHeadState (..), Effect (..), HeadState (..), Input (..), LogicError (..), OpenState (..), Outcome (..), RequirementFailure (..), SideLoadRequirementFailure (..), StateChanged (..), TTL, WaitReason (..), aggregateState, cause, newState, noop, update)
import Hydra.HeadLogic.State (IdleState (..), SeenSnapshot (..), getHeadParameters, mkSeenSnapshot)
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano (cardanoLedger, mkRangedTx, mkSimpleTx)
import Hydra.Ledger.Cardano.TimeSpec (genUTCTime)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), SimpleTxOut (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Network (Connectivity)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node (mkNetworkInput)
import Hydra.Node.DepositPeriod (toNominalDiffTime)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (Active), NodeState (..), initNodeState, initialChainTime)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..), unsyncedPeriodToNominalDiffTime)
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod, defaultUnsyncedPeriod)
import Hydra.Prelude qualified as Prelude
import Hydra.Tx (HeadId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Crypto (aggregate, generateSigningKey, sign)
import Hydra.Tx.Crypto qualified as Crypto
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Party (Party (..), deriveParty)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)
import Test.Gen.Cardano.Api.Typed (genBlockHeaderHash)
import Test.Hydra.API.ClientInput ()
import Test.Hydra.Chain ()
import Test.Hydra.HeadLogic.Input ()
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Ledger.Cardano.Fixtures (eraHistoryWithHorizonAt)
import Test.Hydra.Ledger.Simple ()
import Test.Hydra.Network.Message ()
import Test.Hydra.Node.Environment ()
import Test.Hydra.Node.Fixture qualified as Fixture
import Test.Hydra.Node.State ()
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, deriveOnChainId, fanoutChunkSize, fanoutOutputThreshold, testHeadId, testHeadSeed)
import Test.Hydra.Tx.Gen (genKeyPair, genOutputFor)
import Test.QuickCheck (Property, counterexample, elements, forAll, forAllShrink, oneof, shuffle, suchThat)
import Test.QuickCheck.Gen (generate)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, pick, run)

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
            , depositPeriod = defaultDepositPeriod
            , unsyncedPeriod = defaultUnsyncedPeriod
            , participants = deriveOnChainId <$> threeParties
            , configuredPeers = ""
            }
        aliceEnv =
          Environment
            { party = alice
            , signingKey = aliceSk
            , otherParties = [bob, carol]
            , contestationPeriod = defaultContestationPeriod
            , depositPeriod = defaultDepositPeriod
            , unsyncedPeriod = defaultUnsyncedPeriod
            , participants = deriveOnChainId <$> threeParties
            , configuredPeers = ""
            }

    describe "Coordinated Head Protocol" $ do
      let ledger = simpleLedger

      let coordinatedHeadState =
            CoordinatedHeadState
              { localUTxO = mempty
              , allTxs = mempty
              , localTxs = mempty
              , confirmedSnapshot = InitialSnapshot testHeadId
              , seenSnapshot = NoSeenSnapshot
              , currentDepositTxId = Nothing
              , decommitTx = Nothing
              , version = 0
              }

      it "reports if a requested tx is expired" $ do
        let inputs = utxoRef 1
            tx = SimpleTx 2 inputs mempty
            ttl = 0
            reqTx = NetworkInput ttl $ ReceivedMessage{sender = alice, msg = ReqTx tx}
            s0 = inOpenState threeParties

        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 reqTx `hasStateChangedSatisfying` \case
          TxInvalid{transaction} -> transaction == tx
          _ -> False

      it "waits if a requested tx is not (yet) applicable" $ do
        let reqTx = receiveMessage $ ReqTx $ SimpleTx 2 inputs mempty
            inputs = utxoRef 1
            s0 = inOpenState threeParties

        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 reqTx
          `assertWait` WaitOnNotApplicableTx (ValidationError "cannot apply transaction")

      it "confirms snapshot given it receives AckSn from all parties" $ do
        let reqSn :: Input tx
            reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot1 = testSnapshot 1 0 [] mempty
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

          -- XXX: this is hiding unexpected 'Error' outcomes
          s <- runHeadLogic bobEnv ledger s0 $ do
            step $ receiveMessage $ ReqTx tx1
            step $ receiveMessage $ ReqTx tx2
            step $ receiveMessage $ ReqTx tx3
            step $ receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
            getState

          case headState s of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{localTxs}} -> do
              localTxs `shouldBe` Seq.fromList [tx2, tx3]
            _ -> fail "expected Open state"

      describe "Deposit" $ do
        let plusTime = flip addUTCTime
        it "on tick, ignores deposits from other heads when picking the next active deposit for ReqSn" $ do
          now <- getCurrentTime
          otherHeadId :: HeadId <- generate arbitrary
          let depositTime = plusTime now
              deadline = depositTime 5 `plusTime` toNominalDiffTime (depositPeriod aliceEnv) `plusTime` toNominalDiffTime (depositPeriod aliceEnv)
              deposit1 = Deposit{headId = otherHeadId, deposited = utxoRef 1, created = depositTime 1, deadline, status = Active}
              deposit2 = Deposit{headId = testHeadId, deposited = utxoRef 2, created = depositTime 2, deadline, status = Active}
              -- open state with pending deposits from another head
              party = [alice]
              openState = (inOpenState party){pendingDeposits = Map.fromList [(1, deposit1), (2, deposit2)]}
          let input = ChainInput $ Tick{chainTime = depositTime 3, chainPoint = 3}

          let outcome = update aliceEnv ledger now openState input

          outcome `hasEffectSatisfying` \case
            NetworkEffect ReqSn{depositTxId} -> depositTxId == Just 2
            _ -> False

        prop "does not track depositTx of another head" $ \otherHeadId -> do
          let depositOtherHead =
                observeTx $
                  OnDepositTx
                    { headId = otherHeadId
                    , deposited = mempty
                    , depositTxId = 1
                    , created = genUTCTime `generateWith` 41
                    , deadline = genUTCTime `generateWith` 42
                    }
              s0 = inOpenState threeParties
          now <- nowFromSlot s0.chainPointTime.currentSlot
          update bobEnv ledger now s0 depositOtherHead `hasNoStateChangedSatisfying` \case
            DepositRecorded{headId} -> headId == otherHeadId
            _ -> False

        prop "does not emit DepositRecovered for OnRecoverTx of another head while Open" $ \otherHeadId -> do
          let recoverOtherHead =
                observeTx $
                  OnRecoverTx
                    { headId = otherHeadId
                    , recoveredTxId = 1
                    , recoveredUTxO = mempty
                    }
              s0 = inOpenState threeParties
          now <- nowFromSlot s0.chainPointTime.currentSlot
          update bobEnv ledger now s0 recoverOtherHead `hasNoStateChangedSatisfying` \case
            DepositRecovered{headId} -> headId == otherHeadId
            _ -> False

        it "emits DepositRecorded for own head while Closed" $ do
          let depositTxId' = 1
              depositedUtxo = utxoRef 1
          let depositOwnHead =
                observeTx $
                  OnDepositTx
                    { headId = testHeadId
                    , deposited = depositedUtxo
                    , depositTxId = depositTxId'
                    , created = genUTCTime `generateWith` 41
                    , deadline = genUTCTime `generateWith` 42
                    }
              s0 = inClosedState threeParties
          now <- nowFromSlot s0.chainPointTime.currentSlot
          update aliceEnv ledger now s0 depositOwnHead `hasStateChangedSatisfying` \case
            DepositRecorded{headId, depositTxId, deposited} -> headId == testHeadId && depositTxId == depositTxId' && deposited == depositedUtxo
            _ -> False

        it "emits DepositRecovered for own head while Closed" $ do
          now <- getCurrentTime
          let ownDepositId = 1
              recoveredUtxo = utxoRef 1
              ownDeposit =
                Deposit
                  { headId = testHeadId
                  , deposited = recoveredUtxo
                  , created = now
                  , deadline = addUTCTime 3600 now
                  , status = Active
                  }
              s0 = (inClosedState threeParties){pendingDeposits = Map.singleton ownDepositId ownDeposit}
              recoverOwnDeposit =
                observeTx $
                  OnRecoverTx
                    { headId = testHeadId
                    , recoveredTxId = ownDepositId
                    , recoveredUTxO = recoveredUtxo
                    }
          now' <- nowFromSlot s0.chainPointTime.currentSlot
          update aliceEnv ledger now' s0 recoverOwnDeposit `hasStateChangedSatisfying` \case
            DepositRecovered{headId, depositTxId, recovered} -> headId == testHeadId && depositTxId == ownDepositId && recovered == recoveredUtxo
            _ -> False

        it "emits DepositRecovered while Idle (post-fanout recovery)" $ do
          let ownDepositId = 1
              recoveredUtxo = utxoRef 1
              recoverDeposit =
                observeTx $
                  OnRecoverTx
                    { headId = testHeadId
                    , recoveredTxId = ownDepositId
                    , recoveredUTxO = recoveredUtxo
                    }
          now <- nowFromSlot inIdleState.chainPointTime.currentSlot
          update aliceEnv ledger now inIdleState recoverDeposit `hasStateChangedSatisfying` \case
            DepositRecovered{headId, depositTxId, recovered} -> headId == testHeadId && depositTxId == ownDepositId && recovered == recoveredUtxo
            _ -> False

        it "allows ClientInput Recover in Idle state for deposit surviving from previous head" $ do
          now <- getCurrentTime
          let depositTxId' = 1
              deposit =
                Deposit
                  { headId = testHeadId
                  , deposited = utxoRef 1
                  , created = now
                  , deadline = addUTCTime 3600 now
                  , status = Active
                  }
              -- Deposits from a previous head are never cleared on fanout, so they
              -- remain in pendingDeposits when the node transitions to Idle.
              s0 = (inSync (Idle IdleState{chainState = 0})){pendingDeposits = Map.singleton depositTxId' deposit}
          now' <- nowFromSlot s0.chainPointTime.currentSlot
          update aliceEnv ledger now' s0 (ClientInput (Recover depositTxId'))
            `hasEffectSatisfying` \case
              OnChainEffect{postChainTx = RecoverTx{headId, recoverTxId}} -> headId == testHeadId && recoverTxId == depositTxId'
              _ -> False

        prop "ignores OnDepositTx while Idle" $ \anyHeadId -> do
          let depositedUtxo = utxoRef 1
              depositAnyHead =
                observeTx $
                  OnDepositTx
                    { headId = anyHeadId
                    , deposited = depositedUtxo
                    , depositTxId = 1
                    , created = genUTCTime `generateWith` 41
                    , deadline = genUTCTime `generateWith` 42
                    }
          now <- nowFromSlot inIdleState.chainPointTime.currentSlot
          update aliceEnv ledger now inIdleState depositAnyHead `hasNoStateChangedSatisfying` \case
            DepositRecorded{headId, deposited} -> headId == anyHeadId && deposited == depositedUtxo
            _ -> False

        it "on tick, picks the next active deposit in arrival when in Open state order for ReqSn" $ do
          now <- getCurrentTime
          let party = [alice]
              depositTime = plusTime now
          let deadline = depositTime 5 `plusTime` toNominalDiffTime (depositPeriod aliceEnv) `plusTime` toNominalDiffTime (depositPeriod aliceEnv)
              deposit1 = OnDepositTx{headId = testHeadId, depositTxId = 1, deposited = utxoRef 1, created = depositTime 1, deadline}
              deposit2 = OnDepositTx{headId = testHeadId, depositTxId = 2, deposited = utxoRef 2, created = depositTime 2, deadline}
              deposit3 = OnDepositTx{headId = testHeadId, depositTxId = 3, deposited = utxoRef 3, created = depositTime 3, deadline}

          nodeState <- runHeadLogic aliceEnv ledger (inOpenState party) $ do
            step (observeTxAtSlot 1 deposit1)
            step (observeTxAtSlot 2 deposit2)
            step (observeTxAtSlot 3 deposit3)
            getState

          -- XXX: chainTime should be > created + depositPeriod && < deadline - depositPeriod
          -- so deposits are considered Active
          let chainTime = depositTime 4 `plusTime` toNominalDiffTime (depositPeriod aliceEnv)
          let input = ChainInput $ Tick{chainTime, chainPoint = 4}

          let outcome = update aliceEnv ledger now nodeState input

          forM_ [1, 2, 3] $ \depositId ->
            outcome `hasStateChangedSatisfying` \case
              DepositActivated{depositTxId} -> depositTxId == depositId
              _ -> False

          outcome `hasEffectSatisfying` \case
            NetworkEffect ReqSn{depositTxId} -> depositTxId == Just 1
            _ -> False

        it "deposit activated while snapshot in-flight is picked up by next chained snapshot" $ do
          -- Regression: a deposit that becomes Active while a snapshot is in-flight
          -- (so the tick cannot request a snapshot for it) must be included in the
          -- next chained ReqSn once the in-flight snapshot confirms.
          --
          -- After DepositActivated is aggregated the deposit sits in pendingDeposits
          -- with status=Active, but currentDepositTxId stays Nothing (the bug).
          -- When maybeRequestNextSnapshot fires it calls
          --   setExistingDeposit pendingDeposits Nothing = Nothing
          -- so the deposit is silently dropped from every subsequent ReqSn.
          now <- getCurrentTime
          let
            depositId = 999
            deposit =
              Deposit
                { headId = testHeadId
                , deposited = utxoRef 50
                , created = now
                , deadline = addUTCTime 3600 now
                , status = Active
                }
            -- Single-party head: alice is always leader, so maybeRequestNextSnapshot
            -- fires for sn=2 when she receives her own AckSn for sn=1.
            singleParty = [alice]
            -- sn=1 in SeenSnapshot — no deposit was included (activated too late).
            snapshot1 = testSnapshot 1 0 [] mempty
            -- Pending L2 tx ensures not (null localTxs) → maybeRequestNextSnapshot fires.
            tx2 = aValidTx 2
            -- State as it would be after DepositActivated was processed:
            -- pendingDeposits has the Active deposit, currentDepositTxId is still Nothing.
            s0 =
              ( inOpenState' singleParty $
                  coordinatedHeadState
                    { seenSnapshot = mkSeenSnapshot snapshot1 Map.empty
                    , localTxs = pure tx2
                    , currentDepositTxId = Nothing
                    }
              )
                { pendingDeposits = Map.singleton depositId deposit
                }

          -- Alice's AckSn confirms sn=1; maybeRequestNextSnapshot fires for sn=2.
          now' <- nowFromSlot s0.chainPointTime.currentSlot
          let ackSn = receiveMessageFrom alice $ AckSn (sign aliceSk snapshot1) 1
          let outcome = update aliceEnv ledger now' s0 ackSn

          -- The chained ReqSn for sn=2 must include the active deposit.
          outcome `hasEffectSatisfying` \case
            NetworkEffect ReqSn{depositTxId} -> depositTxId == Just depositId
            _ -> False

        it "DepositActivated from another head does not set currentDepositTxId" $ do
          now <- getCurrentTime
          otherHeadId :: HeadId <- generate arbitrary
          let foreignDeposit =
                Deposit
                  { headId = otherHeadId
                  , deposited = utxoRef 99
                  , created = now
                  , deadline = addUTCTime 3600 now
                  , status = Active
                  }
              s0 = inOpenState threeParties
          let s1 = aggregateState s0 $ Continue [DepositActivated{depositTxId = 99, chainTime = now, deposit = foreignDeposit}] []
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} ->
              chs.currentDepositTxId `shouldBe` Nothing
            _ -> fail "expected Open state"

        it "DepositRecovered from another head does not clear currentDepositTxId" $ do
          now <- getCurrentTime
          otherHeadId :: HeadId <- generate arbitrary
          let ownDepositId = 1
              foreignDepositId = 99
              foreignDeposit =
                Deposit
                  { headId = otherHeadId
                  , deposited = utxoRef 99
                  , created = now
                  , deadline = addUTCTime 3600 now
                  , status = Active
                  }
              s0 =
                (inOpenState' threeParties coordinatedHeadState{currentDepositTxId = Just ownDepositId})
                  { pendingDeposits = Map.singleton foreignDepositId foreignDeposit
                  }
          let s1 = aggregateState s0 $ Continue [DepositRecovered{chainState = 0, headId = otherHeadId, depositTxId = foreignDepositId, recovered = mempty}] []
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} ->
              chs.currentDepositTxId `shouldBe` Just ownDepositId
            _ -> fail "expected Open state"

        it "CommitFinalized from another head does not update version or localUTxO" $ do
          otherHeadId :: HeadId <- generate arbitrary
          let ownUTxO = utxoRefs [1, 2]
              s0 =
                inOpenState'
                  threeParties
                  coordinatedHeadState
                    { localUTxO = ownUTxO
                    , version = 3
                    }
          let s1 = aggregateState s0 $ Continue [CommitFinalized{chainState = 0, headId = otherHeadId, newVersion = 99, depositTxId = 42}] []
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} -> do
              chs.version `shouldBe` 3
              chs.localUTxO `shouldBe` ownUTxO
            _ -> fail "expected Open state"

        it "HeadClosed from another head does not transition Open to Closed during replay" $ do
          otherHeadId :: HeadId <- generate arbitrary
          let s0 = inOpenState threeParties
              closedEvent =
                HeadClosed
                  { headId = otherHeadId
                  , snapshotNumber = 0
                  , chainState = 0
                  , contestationDeadline = arbitrary `generateWith` 42
                  }
          let s1 = aggregateState s0 $ Continue [closedEvent] []
          case s1 of
            NodeInSync{headState = Open _} -> pure ()
            other -> fail $ "expected Open state, got: " <> show other

        it "HeadFannedOut from another head does not transition Closed to Idle during replay" $ do
          otherHeadId :: HeadId <- generate arbitrary
          let s0 = inClosedState threeParties
              fanoutEvent =
                HeadFannedOut
                  { headId = otherHeadId
                  , finalizedOutputs = mempty
                  , chainState = 0
                  }
          let s1 = aggregateState s0 $ Continue [fanoutEvent] []
          case s1 of
            NodeInSync{headState = Closed _} -> pure ()
            other -> fail $ "expected Closed state, got: " <> show other

        it "posts IncrementTx using the snapshot's deposit txid, not by content match" $ do
          -- Regression for #2681: with two pending deposits sharing 'deposited'
          -- content but differing in txid, the old code did 'find' by content
          -- and could pick the wrong one. The fix keys off 'currentDepositTxId'.
          now <- getCurrentTime
          let depositTime = flip addUTCTime now
              deadline = depositTime 600
              depositedUtxo = utxoRef 50
              txid1 = 41 :: Integer
              txid2 = 42 :: Integer
              mkDeposit ts =
                Deposit
                  { headId = testHeadId
                  , deposited = depositedUtxo
                  , created = depositTime ts
                  , deadline
                  , status = Active
                  }
              snapshot1 =
                Snapshot
                  { headId = testHeadId
                  , version = 0
                  , number = 1
                  , confirmed = []
                  , utxo = mempty
                  , utxoToCommit = Just depositedUtxo
                  , utxoToDecommit = Nothing
                  , accumulator = Accumulator.buildFromSnapshotUTxOs mempty (Just depositedUtxo) Nothing
                  }
              s0 =
                ( inOpenState'
                    [alice]
                    coordinatedHeadState
                      { seenSnapshot = mkSeenSnapshot snapshot1 Map.empty
                      , currentDepositTxId = Just txid2
                      }
                )
                  { pendingDeposits = Map.fromList [(txid1, mkDeposit 1), (txid2, mkDeposit 2)]
                  }
              ackSn = receiveMessage $ AckSn (sign aliceSk snapshot1) 1
              outcome = update aliceEnv ledger now s0 ackSn
          outcome `hasEffectSatisfying` \case
            OnChainEffect{postChainTx = IncrementTx{depositTxId}} -> depositTxId == txid2
            _ -> False

        it "does not post IncrementTx when DepositActivated fires for a non-commit snapshot mid-ack" $ do
          -- Regression: 'DepositActivated' can set 'currentDepositTxId' via
          -- '<|>' while a snapshot with no 'utxoToCommit' is in-flight. The
          -- gate on 'snapshot.utxoToCommit = Just _' prevents posting a
          -- malformed IncrementTx in that case.
          now <- getCurrentTime
          let depositTime = flip addUTCTime now
              deadline = depositTime 600
              depositTxId' = 99 :: Integer
              deposit =
                Deposit
                  { headId = testHeadId
                  , deposited = utxoRef 99
                  , created = depositTime 1
                  , deadline
                  , status = Active
                  }
              snapshot1 = testSnapshot 1 0 [] mempty
              s0 =
                ( inOpenState'
                    [alice]
                    coordinatedHeadState
                      { seenSnapshot = mkSeenSnapshot snapshot1 Map.empty
                      , currentDepositTxId = Just depositTxId'
                      }
                )
                  { pendingDeposits = Map.singleton depositTxId' deposit
                  }
              ackSn = receiveMessage $ AckSn (sign aliceSk snapshot1) 1
              outcome = update aliceEnv ledger now s0 ackSn
          outcome `hasNoEffectSatisfying` \case
            OnChainEffect{postChainTx = IncrementTx{}} -> True
            _ -> False

      describe "Decommit" $ do
        it "observes DecommitRecorded and ReqDec in an Open state" $ do
          let outputs = utxoRef 1
              transaction = SimpleTx 1 mempty outputs
              input = receiveMessage ReqDec{transaction}
              st = inOpenState threeParties
          now <- nowFromSlot st.chainPointTime.currentSlot
          update aliceEnv ledger now st input `hasStateChangedSatisfying` \case
            DecommitRecorded{headId, decommitTx} -> headId == testHeadId && utxoFromTx decommitTx == outputs
            _ -> False

        it "ignores ReqDec when not in Open state" $ do
          let reqDec = ReqDec{transaction = SimpleTx 1 mempty (utxoRef 1)}
          let input = receiveMessage reqDec
              st = inClosedState threeParties
          now <- nowFromSlot st.chainPointTime.currentSlot
          update aliceEnv ledger now st input
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
          now <- nowFromSlot s0.chainPointTime.currentSlot
          update bobEnv ledger now s0 reqDecEvent `hasStateChangedSatisfying` \case
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

            now <- nowFromSlot s1.chainPointTime.currentSlot
            update bobEnv ledger now s1 (receiveMessageFrom bob ReqDec{transaction = decommitTx2})
              `assertWait` WaitOnNotApplicableDecommitTx
                { notApplicableReason =
                    DecommitAlreadyInFlight
                      { otherDecommitTxId = txId decommitTx1
                      }
                }

        it "waits if a requested decommit tx is not (yet) applicable" $ do
          let decommitTx = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
              s0 = inOpenState threeParties
              reqDecEvent = receiveMessage ReqDec{transaction = decommitTx}

          now <- nowFromSlot s0.chainPointTime.currentSlot
          update aliceEnv ledger now s0 reqDecEvent
            `assertWait` WaitOnNotApplicableDecommitTx (DecommitTxInvalid mempty (ValidationError "cannot apply transaction"))

        it "updates decommitTx on valid ReqDec" $ do
          let decommitTx' = SimpleTx 1 mempty (utxoRef 1)
          let reqDec = ReqDec{transaction = decommitTx'}
              reqDecEvent = receiveMessage reqDec
              s0 = inOpenState threeParties

          headState s0 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> isNothing decommitTx
            _ -> False

          s1 <- runHeadLogic aliceEnv ledger s0 $ do
            step reqDecEvent
            getState

          headState s1 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> decommitTx == Just decommitTx'
            _ -> False

          -- running the 'ReqDec' again should not alter the recorded state
          s2 <- runHeadLogic aliceEnv ledger s1 $ do
            step reqDecEvent
            getState

          headState s2 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> decommitTx == Just decommitTx'
            _ -> False

        it "emits ReqSn on valid RecDec" $ do
          let localUTxO = utxoRefs [2]
          let decommitTx' = SimpleTx{txSimpleId = 1, txInputs = localUTxO, txOutputs = utxoRefs [4]}
          let s0 = inOpenState' threeParties $ coordinatedHeadState{localUTxO}

          headState s0 `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}}) -> isNothing decommitTx
            _ -> False

          let reqDecEvent = receiveMessage ReqDec{transaction = decommitTx'}

          now <- nowFromSlot s0.chainPointTime.currentSlot
          let s1 = update aliceEnv ledger now s0 reqDecEvent

          let reqSn = ReqSn{snapshotVersion = 0, snapshotNumber = 1, transactionIds = [], decommitTx = Just decommitTx', depositTxId = Nothing}
          s1 `hasEffect` NetworkEffect reqSn

        it "does not get stuck when DecommitFinalized races with in-flight ReqSn" $ do
          let localUTxO = utxoRefs [1]
              confirmedSn =
                ConfirmedSnapshot
                  { snapshot = testSnapshot 0 3 [] localUTxO
                  , signatures = Crypto.aggregate []
                  }
              -- State just after leader sent ReqSn(v=3, sn=1): snapshot in flight
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { localUTxO
                    , version = 3
                    , confirmedSnapshot = confirmedSn
                    , seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}
                    , decommitTx = Just (SimpleTx 10 mempty (utxoRef 99))
                    }

          -- 1. DecommitFinalized arrives, bumping version to 4
          let decrementObservation = observeTx $ OnDecrementTx{headId = testHeadId, newVersion = 4, distributedUTxO = mempty}
          now <- nowFromSlot s0.chainPointTime.currentSlot
          let decommitFinalizedOutcome = update aliceEnv ledger now s0 decrementObservation
          let s1 = aggregateState s0 decommitFinalizedOutcome

          -- Verify seenSnapshot was reset (not stuck as RequestedSnapshot)
          -- After DecommitFinalized, seenSnapshot resets to LastSeenSnapshot{lastSeen=confirmedSn}.
          -- This allows maybeRequestSnapshotAfterDecommit to fire a fresh ReqSn immediately.
          -- NOTE: localUTxO is already correct at this point — DecommitRecorded removed the
          -- decommit outputs from localUTxO when ReqDec was first processed, before ReqSn was sent.
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} -> do
              chs.version `shouldBe` 4
              chs.seenSnapshot `shouldBe` LastSeenSnapshot{lastSeen = 0}
              chs.decommitTx `shouldBe` Nothing
            _ -> fail "expected Open state"

          -- 2. The stale ReqSn(v=3) arrives. The receiver's version is now 4,
          --    so it cannot be processed; we wait (and TTL eventually drops it)
          --    rather than erroring, since the symmetric race where a follower
          --    is briefly behind a leader's bumped version must recover via
          --    retry.
          let staleReqSn :: Input SimpleTx
              staleReqSn = receiveMessageFrom alice $ ReqSn 3 1 [] Nothing Nothing
          now' <- nowFromSlot s1.chainPointTime.currentSlot
          update aliceEnv ledger now' s1 staleReqSn `shouldSatisfy` \case
            Wait WaitOnSnapshotVersion{waitingForVersion = 3} _ -> True
            _ -> False

          -- 3. A new ReqTx arrives — alice (leader for sn=2) can request a new
          -- snapshot because snapshotInFlight is now False after the reset
          let newTx = aValidTx 42
              reqTx = receiveMessage $ ReqTx newTx
          s2 <- runHeadLogic aliceEnv ledger s1 $ do
            step reqTx
            getState

          -- Verify the head is not stuck: seenSnapshot should have advanced
          -- (snapshot number will be based on confirmedSnapshot.number, not lastSeen)
          case s2 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} ->
              chs.seenSnapshot `shouldSatisfy` \case
                RequestedSnapshot{} -> True
                _ -> False
            _ -> fail "expected Open state"

        it "DecommitFinalized with RequestedSnapshot resets seenSnapshot to confirmedSn" $ do
          let localUTxO = utxoRefs [1]
              confirmedSn =
                ConfirmedSnapshot
                  { snapshot = testSnapshot 0 3 [] localUTxO
                  , signatures = Crypto.aggregate []
                  }
              -- State: leader sent ReqSn(v=3, sn=1) with lastSeen=0
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { localUTxO
                    , version = 3
                    , confirmedSnapshot = confirmedSn
                    , seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}
                    , decommitTx = Just (SimpleTx 10 mempty (utxoRef 99))
                    }

          -- DecommitFinalized arrives before AckSn messages
          let decrementObservation = observeTx $ OnDecrementTx{headId = testHeadId, newVersion = 4, distributedUTxO = mempty}
          now <- nowFromSlot s0.chainPointTime.currentSlot
          let decommitFinalizedOutcome = update aliceEnv ledger now s0 decrementObservation
          let s1 = aggregateState s0 decommitFinalizedOutcome

          -- Verify seenSnapshot resets to confirmedSn (0), regardless of requested (1)
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} -> do
              chs.version `shouldBe` 4
              chs.seenSnapshot `shouldBe` LastSeenSnapshot{lastSeen = 0}
              chs.decommitTx `shouldBe` Nothing
            _ -> fail "expected Open state"

        it "AckSn for decommit snapshot is noop after DecommitFinalized" $ do
          let localUTxO = utxoRefs [1]
              snapshot1 = testSnapshot 0 4 [] localUTxO & \s -> s{number = 1}
              confirmedSn =
                ConfirmedSnapshot
                  { snapshot = testSnapshot 0 3 [] localUTxO
                  , signatures = Crypto.aggregate []
                  }
              -- State after DecommitFinalized: lastSeen=1
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { localUTxO
                    , version = 4
                    , confirmedSnapshot = confirmedSn
                    , seenSnapshot = LastSeenSnapshot{lastSeen = 1}
                    , decommitTx = Nothing
                    }

          -- AckSn for snapshot 1 arrives late
          let ackSn = AckSn (sign aliceSk snapshot1) 1
              input = receiveMessageFrom alice ackSn
          now <- nowFromSlot s0.chainPointTime.currentSlot

          -- Should be noop (or error), not Wait
          update aliceEnv ledger now s0 input `shouldSatisfy` \case
            Continue{} -> True
            Error{} -> True
            Wait{} -> False -- Must NOT Wait (infinite AckSn requeue)
        it "DecommitFinalized with SeenSnapshot preserves seenSnapshot so AckSns can still be collected" $ do
          let localUTxO = utxoRefs [1]
              decommitTx = SimpleTx 10 mempty (utxoRef 99)
              snapshot1 = testSnapshot 0 3 [] localUTxO & \s -> s{number = 1}
              confirmedSn =
                ConfirmedSnapshot
                  { snapshot = testSnapshot 0 3 [] localUTxO
                  , signatures = Crypto.aggregate []
                  }
              -- State: follower has seen ReqSn and is collecting signatures
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { localUTxO
                    , version = 3
                    , confirmedSnapshot = confirmedSn
                    , seenSnapshot = mkSeenSnapshot snapshot1 mempty
                    , decommitTx = Just decommitTx
                    }

          -- DecommitFinalized arrives while collecting signatures
          let decrementObservation = observeTx $ OnDecrementTx{headId = testHeadId, newVersion = 4, distributedUTxO = mempty}
          now <- nowFromSlot s0.chainPointTime.currentSlot
          let decommitFinalizedOutcome = update bobEnv ledger now s0 decrementObservation
          let s1 = aggregateState s0 decommitFinalizedOutcome

          -- DecommitFinalized preserves SeenSnapshot so AckSns can still be collected.
          -- seenSnapshot stays as SeenSnapshot (not reset to LastSeenSnapshot).
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} -> do
              chs.version `shouldBe` 4
              chs.decommitTx `shouldBe` Nothing
              chs.seenSnapshot `shouldBe` mkSeenSnapshot snapshot1 mempty
            _ -> fail "expected Open state"

        it "DecommitFinalized with SeenSnapshot does not re-request snapshot already in-flight" $ do
          let localUTxO = utxoRefs [1]
              decommitTx = SimpleTx 10 mempty (utxoRef 99)
              snapshot1 = testSnapshot 0 3 [] localUTxO & \s -> s{number = 1}
              confirmedSn =
                ConfirmedSnapshot
                  { snapshot = testSnapshot 0 3 [] localUTxO
                  , signatures = Crypto.aggregate []
                  }
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { localUTxO
                    , version = 3
                    , confirmedSnapshot = confirmedSn
                    , seenSnapshot = mkSeenSnapshot snapshot1 mempty
                    , decommitTx = Just decommitTx
                    }

          let decrementObservation = observeTx $ OnDecrementTx{headId = testHeadId, newVersion = 4, distributedUTxO = mempty}
          now <- nowFromSlot s0.chainPointTime.currentSlot
          let outcome = update aliceEnv ledger now s0 decrementObservation

          -- No new snapshot should be requested: the one in SeenSnapshot will complete
          outcome `hasNoStateChangedSatisfying` \case
            SnapshotRequestDecided{} -> True
            _ -> False

          -- seenSnapshot must be preserved so AckSns can still be collected
          let s1 = aggregateState s0 outcome
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} ->
              chs.seenSnapshot `shouldBe` mkSeenSnapshot snapshot1 mempty
            _ -> fail "expected Open state"

        it "CommitFinalized with SeenSnapshot does not re-request snapshot already in-flight" $ do
          let localUTxO = utxoRefs [1]
              snapshot1 = testSnapshot 0 3 [] localUTxO & \s -> s{number = 1}
              confirmedSn =
                ConfirmedSnapshot
                  { snapshot = testSnapshot 0 3 [] localUTxO
                  , signatures = Crypto.aggregate []
                  }
              depositTxId = 42
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { localUTxO
                    , version = 3
                    , confirmedSnapshot = confirmedSn
                    , seenSnapshot = mkSeenSnapshot snapshot1 mempty
                    , currentDepositTxId = Just depositTxId
                    }

          let incrementObservation = observeTx $ OnIncrementTx{headId = testHeadId, newVersion = 4, depositTxId}
          now <- nowFromSlot s0.chainPointTime.currentSlot
          let outcome = update aliceEnv ledger now s0 incrementObservation

          -- No new snapshot should be requested: the one in SeenSnapshot will complete
          outcome `hasNoStateChangedSatisfying` \case
            SnapshotRequestDecided{} -> True
            _ -> False

          -- seenSnapshot must be preserved so AckSns can still be collected
          let s1 = aggregateState s0 outcome
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} ->
              chs.seenSnapshot `shouldBe` mkSeenSnapshot snapshot1 mempty
            _ -> fail "expected Open state"

        it "CommitFinalized with RequestedSnapshot resets seenSnapshot to confirmedSn" $ do
          let localUTxO = utxoRefs [1]
              confirmedSn =
                ConfirmedSnapshot
                  { snapshot = testSnapshot 0 3 [] localUTxO
                  , signatures = Crypto.aggregate []
                  }
              depositTxId = 42
              -- State after leader sent ReqSn(sn=1) with pending deposit: snapshot in flight
              s0 =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { localUTxO
                    , version = 3
                    , confirmedSnapshot = confirmedSn
                    , seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}
                    , currentDepositTxId = Just depositTxId
                    }

          -- CommitFinalized arrives before AckSn messages (race condition)
          let incrementObservation = observeTx $ OnIncrementTx{headId = testHeadId, newVersion = 4, depositTxId}
          now <- nowFromSlot s0.chainPointTime.currentSlot
          let commitFinalizedOutcome = update aliceEnv ledger now s0 incrementObservation
          let s1 = aggregateState s0 commitFinalizedOutcome

          -- seenSnapshot resets to confirmedSn (0), not requested (1)
          case s1 of
            NodeInSync{headState = Open OpenState{coordinatedHeadState = chs}} -> do
              chs.version `shouldBe` 4
              chs.currentDepositTxId `shouldBe` Nothing
              chs.seenSnapshot `shouldBe` LastSeenSnapshot{lastSeen = 0}
            _ -> fail "expected Open state"

      describe "Tracks Transaction Ids" $ do
        it "keeps transactions in allTxs given it receives a ReqTx" $ do
          let s0 = inOpenState threeParties
              t1 = SimpleTx 1 mempty (utxoRef 1)

          sa <- runHeadLogic bobEnv ledger s0 $ do
            step $ receiveMessage $ ReqTx t1
            getState

          headState sa `shouldSatisfy` \case
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

          headState s1 `shouldSatisfy` \case
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

          headState sa `shouldSatisfy` \case
            (Open OpenState{coordinatedHeadState = CoordinatedHeadState{allTxs}}) -> txId t1 `notMember` allTxs
            _ -> False

      it "rejects last AckSn if one signature was from a different snapshot" $ do
        let reqSn :: Input tx
            reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
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

        now <- nowFromSlot waitingForLastAck.chainPointTime.currentSlot
        update bobEnv ledger now waitingForLastAck (invalidAckFrom bobSk bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if one signature was from a different key" $ do
        let reqSn :: Input tx
            reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot = testSnapshot 1 0 [] mempty
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot) 1
        waitingForLastAck <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)
            getState

        now <- nowFromSlot waitingForLastAck.chainPointTime.currentSlot
        update bobEnv ledger now waitingForLastAck (ackFrom (generateSigningKey "foo") bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if one signature was from a completely different message" $ do
        let reqSn :: Input tx
            reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot1 = testSnapshot 1 0 [] mempty
            ackFrom :: Secret (Crypto.SigningKey Crypto.HydraKey) -> Party -> Input SimpleTx
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot1) 1
            invalidAckFrom :: Secret (Crypto.SigningKey Crypto.HydraKey) -> Party -> Input SimpleTx
            invalidAckFrom sk vk =
              receiveMessageFrom vk $
                AckSn (coerce $ sign sk ("foo" :: ByteString)) 1
        waitingForLastAck <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (invalidAckFrom bobSk bob)
            getState

        now <- nowFromSlot waitingForLastAck.chainPointTime.currentSlot
        update bobEnv ledger now waitingForLastAck (ackFrom aliceSk alice)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == [vkey bob]
            _ -> False

      it "rejects last AckSn if already received signature from this party" $ do
        let reqSn :: Input tx
            reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot1 = testSnapshot 1 0 [] mempty
            ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot1) 1
        waitingForAck <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn
            step (ackFrom carolSk carol)
            getState

        now <- nowFromSlot waitingForAck.chainPointTime.currentSlot
        update bobEnv ledger now waitingForAck (ackFrom carolSk carol)
          `shouldSatisfy` \case
            Error (RequireFailed SnapshotAlreadySigned{receivedSignature}) -> receivedSignature == carol
            _ -> False

      it "ignores valid AckSn if snapshot already confirmed" $ do
        let reqSn :: Input tx
            reqSn = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            snapshot1 = testSnapshot 1 0 [] mempty
            ackFrom sk = receiveMessageFrom (deriveParty sk) $ AckSn (sign sk snapshot1) 1

        s0 <- runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
          step reqSn
          step (ackFrom carolSk)
          step (ackFrom bobSk)
          step (ackFrom aliceSk)
          getState

        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 (ackFrom carolSk)
          `shouldBe` noop

      it "rejects snapshot request with transaction not applicable to previous snapshot" $ do
        let reqTx42 = receiveMessage $ ReqTx (SimpleTx 42 mempty (utxoRef 1))
            reqTx1 = receiveMessage $ ReqTx (SimpleTx 1 (utxoRef 1) (utxoRef 2))
            input = receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
            s0 = inOpenState threeParties

        s2 <- runHeadLogic bobEnv ledger s0 $ do
          step reqTx42
          step reqTx1
          getState

        now <- nowFromSlot s2.chainPointTime.currentSlot
        update bobEnv ledger now s2 input
          `shouldBe` Error (RequireFailed (SnapshotDoesNotApply 1 1 (ValidationError "cannot apply transaction")))

      it "rejects snapshot request with UTxO set exceeding the accumulator limit" $ do
        -- alice is leader for sn=1 (isLeader: (1-1) mod 3 = 0 = index alice)
        let bigCount = Accumulator.maxAccumulatorSize + 1
            bigOutputs = utxoRefs (map fromIntegral [1 .. bigCount])
            tx = SimpleTx 1 mempty bigOutputs
            st =
              inOpenState' threeParties $
                coordinatedHeadState{allTxs = Map.fromList [(1, tx)]}
            reqSn = receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st reqSn
          `shouldBe` Error (RequireFailed ReqSnUTxOSetTooLarge{utxoCount = bigCount, maxAllowed = Accumulator.maxAccumulatorSize})

      it "waits if we receive a snapshot with unseen transactions" $ do
        let s0 = inOpenState threeParties
            reqSn = receiveMessage $ ReqSn 0 1 [1] Nothing Nothing
        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 reqSn
          `assertWait` WaitOnTxs [1]

      it "waits if we receive an AckSn for an unseen snapshot" $ do
        let snapshot = testSnapshot 1 0 [] mempty
            input = receiveMessage $ AckSn (sign aliceSk snapshot) 1
            s0 = inOpenState threeParties
        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 input
          `assertWait` WaitOnSeenSnapshot

      -- TODO: Write property tests for various future / old snapshot behavior.
      -- That way we could cover variations of snapshot numbers and state of
      -- snapshot collection.

      it "rejects if we receive a too far future snapshot" $ do
        let input :: Input tx
            input = receiveMessageFrom bob $ ReqSn 0 2 [] Nothing Nothing
            st = inOpenState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "waits if we receive a future snapshot while collecting signatures" $ do
        let reqSn1 :: Input tx
            reqSn1 = receiveMessage $ ReqSn 0 1 [] Nothing Nothing
            reqSn2 :: Input tx
            reqSn2 = receiveMessageFrom bob $ ReqSn 0 2 [] Nothing Nothing
        st <-
          runHeadLogic bobEnv ledger (inOpenState threeParties) $ do
            step reqSn1
            getState

        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st reqSn2
          `assertWait` WaitOnSnapshotNumber 1

      it "acks signed snapshot from the constant leader" $ do
        let leader = alice
            snapshot = testSnapshot 1 0 [] mempty
            input = receiveMessageFrom leader $ ReqSn 0 (number snapshot) [] Nothing Nothing
            sig = sign bobSk snapshot
            st = inOpenState threeParties
            ack = AckSn sig (number snapshot)
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st input `hasEffect` NetworkEffect ack

      it "does not ack snapshots from non-leaders" $ do
        let input :: Input tx
            input = receiveMessageFrom notTheLeader $ ReqSn 0 1 [] Nothing Nothing
            notTheLeader = bob
            st = inOpenState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st input `shouldSatisfy` \case
          Error (RequireFailed ReqSnNotLeader{requestedSn = 1, leader}) -> leader == notTheLeader
          _ -> False

      it "rejects too-old snapshots" $ do
        let input :: Input tx
            input = receiveMessageFrom theLeader $ ReqSn 0 2 [] Nothing Nothing
            theLeader = alice
            snapshot = testSnapshot 2 0 [] mempty
            st =
              inOpenState' threeParties $
                coordinatedHeadState{confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])}
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "rejects too-old snapshots when collecting signatures" $ do
        let input :: Input tx
            input = receiveMessageFrom theLeader $ ReqSn 0 2 [] Nothing Nothing
            theLeader = alice
            snapshot = testSnapshot 2 0 [] mempty
            st =
              inOpenState' threeParties $
                coordinatedHeadState
                  { confirmedSnapshot = ConfirmedSnapshot snapshot (Crypto.aggregate [])
                  , seenSnapshot = mkSeenSnapshot (testSnapshot 3 0 [] mempty) mempty
                  }
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 3)

      it "rejects too-new snapshots from the leader" $ do
        let input :: Input tx
            input = receiveMessageFrom theLeader $ ReqSn 0 3 [] Nothing Nothing
            theLeader = carol
            st = inOpenState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st input `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 3 0)

      it "waits on out-of-sync snapshot version" $ do
        -- A ReqSn whose version does not match our local version is parked as
        -- a Wait so the TTL/retry machinery can recover when a follower is
        -- briefly behind the leader after an in-flight OnIncrement/OnDecrement.
        let validSnNumber = 1
            invalidSnVersion = 1
            input = receiveMessageFrom theLeader $ ReqSn invalidSnVersion validSnNumber [] Nothing Nothing
            theLeader = alice
            st = inOpenState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st input `shouldSatisfy` \case
          Wait WaitOnSnapshotVersion{waitingForVersion = 1} _ -> True
          _ -> False

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

        now <- nowFromSlot s3.chainPointTime.currentSlot
        update bobEnv ledger now s3 secondReqSn `shouldSatisfy` \case
          Error RequireFailed{} -> True
          _ -> False

      it "rejects same version snapshot requests with differing decommit txs" $ do
        let decommitTx1 = SimpleTx 1 (utxoRef 1) (utxoRef 3)
            decommitTx2 = SimpleTx 2 (utxoRef 2) (utxoRef 4)
            activeUTxO = utxoRefs [1, 2]
            utxoToDecommit = Just $ utxoRefs [3]
            snapshot =
              Snapshot
                { headId = testHeadId
                , version = 0
                , number = 1
                , confirmed = []
                , utxo = activeUTxO
                , utxoToCommit = Nothing
                , utxoToDecommit = utxoToDecommit
                , accumulator = Accumulator.buildFromSnapshotUTxOs activeUTxO Nothing utxoToDecommit
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

      describe "Deposit after rollback" $ do
        let singleParty = [alice]
            plusTime = flip addUTCTime

        it "ReqSn still references stale currentDepositTxId after deposit is recovered" $ do
          -- This test reproduced the bug where:
          -- 1. A deposit is included in a snapshot (setting currentDepositTxId in local state)
          -- 2. The IncrementTx is posted on-chain
          -- 3. A chain rollback erases the IncrementTx observation
          -- 4. The deposit is recovered on-chain (removed from pendingDeposits)
          -- 5. The stale currentDepositTxId causes future ReqSn to reference a
          --    deposit that no longer exists, leading to infinite WaitOnDepositObserved
          now <- getCurrentTime
          let aliceEnv' =
                aliceEnv
                  { depositPeriod = 60
                  , contestationPeriod = 60
                  , otherParties = []
                  , participants = deriveOnChainId <$> [alice]
                  }
          let depositTime = plusTime now
              deadline = depositTime 600
              depositTxId = 42 :: Integer
              depositedUtxo = utxoRef depositTxId
              deposit =
                OnDepositTx
                  { headId = testHeadId
                  , depositTxId
                  , deposited = depositedUtxo
                  , created = depositTime 1
                  , deadline
                  }

          -- Step 1: Start in open state, observe deposit
          s1 <- runHeadLogic aliceEnv' ledger (inOpenState singleParty) $ do
            step (observeTxAtSlot 1 deposit)
            getState

          -- Step 2: Tick to activate deposit and trigger ReqSn
          let chainTime = depositTime 2 `plusTime` toNominalDiffTime (depositPeriod aliceEnv')
              tickInput = ChainInput $ Tick{chainTime, chainPoint = 2}
          s2 <- runHeadLogic aliceEnv' ledger s1 $ do
            step tickInput
            getState

          -- Verify deposit is now active. DepositActivated queues the deposit
          -- immediately into currentDepositTxId (via <|> in the aggregate).
          case headState s2 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{currentDepositTxId}} ->
              currentDepositTxId `shouldBe` Just depositTxId
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          -- Step 3: Process ReqSn with the deposit (as if received from network)
          let reqSnWithDeposit = receiveMessage $ ReqSn 0 1 [] Nothing (Just depositTxId)
          s3 <- runHeadLogic aliceEnv' ledger s2 $ do
            step reqSnWithDeposit
            getState

          -- Verify currentDepositTxId is now set
          case headState s3 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{currentDepositTxId}} ->
              currentDepositTxId `shouldBe` Just depositTxId
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          -- Step 4: AckSn from alice (single party) → SnapshotConfirmed + CommitApproved + IncrementTx posted
          let snapshot1 =
                Snapshot
                  { headId = testHeadId
                  , version = 0
                  , number = 1
                  , confirmed = []
                  , utxo = mempty
                  , utxoToCommit = Just depositedUtxo
                  , utxoToDecommit = Nothing
                  , accumulator = Accumulator.buildFromSnapshotUTxOs mempty (Just depositedUtxo) Nothing
                  }
              ackSn = receiveMessage $ AckSn (sign aliceSk snapshot1) 1
          s4 <- runHeadLogic aliceEnv' ledger s3 $ do
            step ackSn
            getState

          -- Verify SnapshotConfirmed happened and currentDepositTxId is still set
          case headState s4 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{currentDepositTxId, confirmedSnapshot}} -> do
              currentDepositTxId `shouldBe` Just depositTxId
              case confirmedSnapshot of
                ConfirmedSnapshot{snapshot = Snapshot{number}} -> number `shouldBe` 1
                _ -> expectationFailure "Expected ConfirmedSnapshot"
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          -- Step 5: Chain rollback (erases the IncrementTx observation)
          let rollbackInput = ChainInput Rollback{rolledBackChainState = SimpleChainState 0, chainTime = now}
          s5 <- runHeadLogic aliceEnv' ledger s4 $ do
            step rollbackInput
            getState

          -- currentDepositTxId should still be set (this is part of the bug)
          case headState s5 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{currentDepositTxId}} ->
              currentDepositTxId `shouldBe` Just depositTxId
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          -- Step 6: Deposit is recovered on-chain (removed from pendingDeposits)
          let recoverInput =
                observeTxAtSlot
                  2
                  OnRecoverTx
                    { headId = testHeadId
                    , recoveredTxId = depositTxId
                    , recoveredUTxO = depositedUtxo
                    }
          s6 <- runHeadLogic aliceEnv' ledger s5 $ do
            step recoverInput
            getState

          -- Verify that deposit was removed from pendingDeposits after recover (fix for the bug)
          case headState s6 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{currentDepositTxId}} ->
              currentDepositTxId `shouldBe` Nothing
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          Map.lookup depositTxId s6.pendingDeposits `shouldBe` Nothing

          -- Step 7: New transaction arrives, leader creates new ReqSn
          let newTx = aValidTx 1
          let reqTxInput = receiveMessage $ ReqTx newTx

          let outcome = update aliceEnv' ledger now s6 reqTxInput

          -- The ReqSn emitted by the leader does not reference the recovered deposit
          outcome `hasEffectSatisfying` \case
            NetworkEffect ReqSn{depositTxId = reqDepositTxId} ->
              isNothing reqDepositTxId
            _ -> False

          -- Step 8: When node processes this ReqSn, it will wait forever
          s7 <- runHeadLogic aliceEnv' ledger s6 $ do
            step reqTxInput
            getState

          let staleReqSn = receiveMessage $ ReqSn 0 2 [txId newTx] Nothing (Just depositTxId)
          let reqSnOutcome = update aliceEnv' ledger now s7 staleReqSn
          -- Fix bug: Error out instead of waiting for deposit to be observed forever
          reqSnOutcome `shouldBe` Error (RequireFailed $ RequestedDepositNotFoundLocally depositTxId)

        it "re-posts IncrementTx on chain rollback when deposit is pending" $ do
          -- After a snapshot is confirmed with a deposit (CommitApproved + IncrementTx posted),
          -- if a chain rollback occurs, the node should re-post the IncrementTx because
          -- the rollback may have erased the original on-chain observation.
          now <- getCurrentTime
          let aliceEnv' =
                aliceEnv
                  { depositPeriod = 60
                  , contestationPeriod = 60
                  , otherParties = []
                  , participants = deriveOnChainId <$> [alice]
                  }
          let depositTime = plusTime now
              deadline = depositTime 600
              depositTxId = 42 :: Integer
              depositedUtxo = utxoRef depositTxId
              deposit =
                OnDepositTx
                  { headId = testHeadId
                  , depositTxId
                  , deposited = depositedUtxo
                  , created = depositTime 1
                  , deadline
                  }

          -- Observe deposit, activate it via tick, process ReqSn and AckSn
          -- to reach a state where CommitApproved happened and IncrementTx was posted
          s1 <- runHeadLogic aliceEnv' ledger (inOpenState singleParty) $ do
            step (observeTxAtSlot 1 deposit)
            getState

          let chainTime = depositTime 2 `plusTime` toNominalDiffTime (depositPeriod aliceEnv')
              tickInput = ChainInput $ Tick{chainTime, chainPoint = 2}
          s2 <- runHeadLogic aliceEnv' ledger s1 $ do
            step tickInput
            getState

          let reqSnWithDeposit = receiveMessage $ ReqSn 0 1 [] Nothing (Just depositTxId)
          s3 <- runHeadLogic aliceEnv' ledger s2 $ do
            step reqSnWithDeposit
            getState

          let snapshot1 =
                Snapshot
                  { headId = testHeadId
                  , version = 0
                  , number = 1
                  , confirmed = []
                  , utxo = mempty
                  , utxoToCommit = Just depositedUtxo
                  , utxoToDecommit = Nothing
                  , accumulator = Accumulator.buildFromSnapshotUTxOs mempty (Just depositedUtxo) Nothing
                  }
              ackSn = receiveMessage $ AckSn (sign aliceSk snapshot1) 1
          s4 <- runHeadLogic aliceEnv' ledger s3 $ do
            step ackSn
            getState

          -- Verify we are in the expected state: currentDepositTxId set, snapshot confirmed
          case headState s4 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{currentDepositTxId, confirmedSnapshot}} -> do
              currentDepositTxId `shouldBe` Just depositTxId
              case confirmedSnapshot of
                ConfirmedSnapshot{snapshot = Snapshot{number}} -> number `shouldBe` 1
                _ -> expectationFailure "Expected ConfirmedSnapshot"
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          -- Chain rollback should re-post the IncrementTx
          let rollbackInput = ChainInput Rollback{rolledBackChainState = SimpleChainState 0, chainTime = now}
          let rollbackOutcome = update aliceEnv' ledger now s4 rollbackInput

          rollbackOutcome `hasEffectSatisfying` \case
            OnChainEffect{postChainTx} ->
              case postChainTx of
                IncrementTx{} -> True
                _ -> False
            _ -> False

        it "re-posts DecrementTx on chain rollback when decommit is pending" $ do
          -- After a snapshot is confirmed with a decommit (DecommitApproved + DecrementTx posted),
          -- if a chain rollback occurs, the node should re-post the DecrementTx because
          -- the rollback may have erased the original on-chain observation.
          now <- getCurrentTime
          let aliceEnv' =
                aliceEnv
                  { depositPeriod = 60
                  , contestationPeriod = 60
                  , otherParties = []
                  , participants = deriveOnChainId <$> [alice]
                  }

          -- NOTE: The simple ledger does not check inputs if none present.
          let decommitTx' = aValidTx 3
              s0 = inOpenState singleParty

          -- Step 1: Submit decommit request
          s1 <- runHeadLogic aliceEnv' ledger s0 $ do
            step $ receiveMessage ReqDec{transaction = decommitTx'}
            getState

          -- Verify decommitTx is recorded
          case headState s1 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx}} ->
              decommitTx `shouldBe` Just decommitTx'
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          -- Step 2: Process ReqSn with the decommit tx
          let reqSnWithDecommit = receiveMessage $ ReqSn 0 1 [] (Just decommitTx') Nothing
          s2 <- runHeadLogic aliceEnv' ledger s1 $ do
            step reqSnWithDecommit
            getState

          -- Step 3: AckSn from alice → SnapshotConfirmed + DecommitApproved + DecrementTx posted
          -- The snapshot after applying decommit:
          --   confirmedUTxO = initialUtxo = {1}
          --   applyTransactions {1} [SimpleTx 10 {1} {3}] = {3}
          --   utxoToDecommit = utxoFromTx decommitTx' = txOutputs = {3}
          --   activeUTxO = {3} \ {3} = {}
          let snapshot1 =
                Snapshot
                  { headId = testHeadId
                  , version = 0
                  , number = 1
                  , confirmed = []
                  , utxo = mempty -- activeUTxO after decommit
                  , utxoToCommit = Nothing
                  , utxoToDecommit = Just (utxoRef 3) -- outputs of decommit tx
                  , accumulator = Accumulator.buildFromSnapshotUTxOs mempty Nothing (Just (utxoRef 3))
                  }
              ackSn = receiveMessage $ AckSn (sign aliceSk snapshot1) 1
          s3 <- runHeadLogic aliceEnv' ledger s2 $ do
            step ackSn
            getState

          -- Verify SnapshotConfirmed happened and decommitTx is still set
          case headState s3 of
            Open OpenState{coordinatedHeadState = CoordinatedHeadState{decommitTx, confirmedSnapshot}} -> do
              decommitTx `shouldBe` Just decommitTx'
              case confirmedSnapshot of
                ConfirmedSnapshot{snapshot = Snapshot{number, utxoToDecommit}} -> do
                  number `shouldBe` 1
                  utxoToDecommit `shouldBe` Just (utxoRef 3)
                _ -> expectationFailure "Expected ConfirmedSnapshot"
            other -> expectationFailure $ "Expected Open state, got: " <> show other

          -- Step 4: Chain rollback should re-post the DecrementTx
          let rollbackInput = ChainInput Rollback{rolledBackChainState = SimpleChainState 0, chainTime = now}
          let rollbackOutcome = update aliceEnv' ledger now s3 rollbackInput

          rollbackOutcome `hasEffectSatisfying` \case
            OnChainEffect{postChainTx} ->
              case postChainTx of
                DecrementTx{} -> True
                _ -> False
            _ -> False

      it "ignores in-flight ReqTx when closed" $ do
        let s0 = inClosedState threeParties
            input = receiveMessage $ ReqTx (aValidTx 42)
        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 input `shouldBe` Error (UnhandledInput input (headState s0))

      it "ignores in-flight ReqDec when closed" $ do
        let s0 = inClosedState threeParties
            input = receiveMessage $ ReqDec{transaction = aValidTx 42}
        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 input `shouldBe` Error (UnhandledInput input (headState s0))

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
              someChainPoint = arbitrary `generateWith` 42
          let stepTimePastDeadline = ChainInput $ Tick oneSecondsPastDeadline someChainPoint

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
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters $ headState s0)
        runHeadLogic bobEnv ledger s0 $ do
          o1 <- step $ observeTx (OnCloseTx testHeadId 0 deadline)
          lift $ o1 `hasEffect` chainEffect (ContestTx testHeadId params snapshotVersion latestConfirmedSnapshot)
          s1 <- getState
          lift $
            headState s1 `shouldSatisfy` \case
              Closed ClosedState{} -> True
              _ -> False

      it "re-contests when detecting contest with old snapshot" $ do
        let snapshotVersion = 0
            snapshot2 = testSnapshot 2 snapshotVersion [] mempty
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot2 (Crypto.aggregate [])
            s0 = inClosedState' threeParties latestConfirmedSnapshot
            deadline = arbitrary `generateWith` 42
            params = fromMaybe (HeadParameters defaultContestationPeriod threeParties) (getHeadParameters (headState s0))
        now <- nowFromSlot s0.chainPointTime.currentSlot
        update bobEnv ledger now s0 (observeTx $ OnContestTx testHeadId 1 deadline)
          `hasEffect` chainEffect (ContestTx testHeadId params snapshotVersion latestConfirmedSnapshot)

      it "ignores unrelated initTx" prop_ignoresUnrelatedOnInitTx

      prop "node synced status transitions according to chain time and unsynced policy" $
        forAllShrink arbitrary shrink $ \headState ->
          forAllShrink
            ( arbitrary @[Input SimpleTx]
                `suchThat` ( not
                              . any
                                ( \case
                                    ChainInput Tick{} -> True
                                    _ -> False
                                )
                           )
            )
            shrink
            $ \noTickInputs -> monadicIO $ do
              stillCatchingUp <-
                run $
                  foldM
                    ( \nodeState input ->
                        runHeadLogic bobEnv ledger nodeState $ do
                          step input
                          getState
                    )
                    (catchingUp headState)
                    noTickInputs

              assert $ case stillCatchingUp of
                NodeCatchingUp{} -> True
                _ -> False

              now <- run getCurrentTime
              nodeInSync <- run $ do
                runHeadLogic bobEnv ledger stillCatchingUp $ do
                  step $ ChainInput Tick{chainTime = now, chainPoint = 10}
                  getState

              assert $ case nodeInSync of
                NodeInSync{} -> True
                _ -> False

              nodeOutOfSync <- run $ do
                let delta = unsyncedPeriodToNominalDiffTime bobEnv.unsyncedPeriod
                    -- make chain time too old: beyond unsynced threshold
                    oldChainTime = addUTCTime (negate (delta + 1)) now
                runHeadLogic bobEnv ledger nodeInSync $ do
                  step $ ChainInput Tick{chainTime = oldChainTime, chainPoint = 100}
                  getState

              assert $ case nodeOutOfSync of
                NodeCatchingUp{} -> True
                _ -> False

      -- safety: cannot stay in-sync beyond the contestation window
      prop "node must be out of sync after full contestation period" $
        forAllShrink arbitrary shrink $ \headState ->
          monadicIO $ do
            now <- run getCurrentTime
            let delta = bobEnv.contestationPeriod
            -- make chain time too old: elapsed time >= full contestation period
            let oldChainTime = addUTCTime (negate $ CP.toNominalDiffTime delta) now

            nodeAfter <- run $ runHeadLogic bobEnv ledger (inSync headState) $ do
              step $ ChainInput Tick{chainTime = oldChainTime, chainPoint = 1}
              getState

            assert $ case nodeAfter of
              NodeCatchingUp{} -> True
              _ -> False

      -- liveness: cannot drop out of sync under normal block timing
      prop "node remains in sync under normal block cadence" $
        forAllShrink arbitrary shrink $ \headState ->
          monadicIO $ do
            now <- run getCurrentTime
            let normalBlockInterval = 20
            let nextTime = addUTCTime normalBlockInterval now

            nodeAfter <- run $ runHeadLogic bobEnv ledger (inSync headState) $ do
              step $ ChainInput Tick{chainTime = nextTime, chainPoint = 1}
              getState

            assert $ case nodeAfter of
              NodeInSync{} -> True
              _ -> False

      prop "connectivity messages passthrough without affecting the current state" $
        \(ttl, connectivityMessage, nodeState) -> do
          let input = connectivityChanged ttl connectivityMessage
          case nodeState of
            NodeCatchingUp{} -> do
              now <- getCurrentTime
              let outcome = update bobEnv ledger now nodeState input
              outcome `shouldSatisfy` \case
                Wait{reason = WaitOnNodeInSync{currentSlot}, stateChanges} ->
                  null stateChanges && currentSlot == nodeState.chainPointTime.currentSlot
                _ -> False
            NodeInSync{} -> do
              now <- nowFromSlot nodeState.chainPointTime.currentSlot
              let outcome = update bobEnv ledger now nodeState input
              outcome `shouldSatisfy` \case
                Continue{stateChanges, effects} ->
                  null effects
                    && all
                      ( \case
                          -- NOTE: match only network related outcomes
                          PeerConnected{} -> True
                          PeerDisconnected{} -> True
                          NetworkVersionMismatch{} -> True
                          NetworkClusterIDMismatch{} -> True
                          NetworkConnected{} -> True
                          NetworkDisconnected{} -> True
                          _ -> False
                      )
                      stateChanges
                _ -> False

      prop "ignores decrementTx of another head" $ \otherHeadId -> do
        let decrementOtherHead = observeTx $ OnDecrementTx{headId = otherHeadId, newVersion = 1, distributedUTxO = mempty}
            st = inOpenState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st decrementOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores closeTx of another head" $ \otherHeadId snapshotNumber contestationDeadline -> do
        let closeOtherHead = observeTx $ OnCloseTx{headId = otherHeadId, snapshotNumber, contestationDeadline}
            st = inOpenState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st closeOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores contestTx of another head" $ \otherHeadId snapshotNumber contestationDeadline -> do
        let contestOtherHead = observeTx $ OnContestTx{headId = otherHeadId, snapshotNumber, contestationDeadline}
            st = inClosedState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st contestOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "ignores fanoutTx of another head" $ \otherHeadId fanoutUTxO -> do
        let collectOtherHead = observeTx $ OnFanoutTx{headId = otherHeadId, fanoutUTxO}
            st = inClosedState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st collectOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "fanout utxo always relies on observed utxo" $ \fanoutUTxO ->
        forAllShrink genClosedState shrink $ \closedState -> monadicIO $ do
          let fanoutHead = observeTx $ OnFanoutTx{headId = testHeadId, fanoutUTxO}
          now <- run $ nowFromSlot closedState.chainPointTime.currentSlot
          let outcome = update bobEnv ledger now closedState fanoutHead
          monitor $ counterexample ("Outcome: " <> show outcome)
          run $
            outcome `hasStateChangedSatisfying` \case
              HeadFannedOut{finalizedOutputs} -> finalizedOutputs == fanoutUTxO
              _ -> False

      prop "fanout utxo includes accumulated UTxO from prior partial fanouts" $
        \fanoutUTxO priorDistributed ->
          forAllShrink genClosedState shrink $ \closedState -> monadicIO $ do
            let stWithPrior = case closedState.headState of
                  Closed cst ->
                    closedState{headState = Closed cst{distributedFanoutOutputs = priorDistributed}}
                  _ -> closedState
                fanoutHead = observeTx $ OnFanoutTx{headId = testHeadId, fanoutUTxO}
            now <- run $ nowFromSlot closedState.chainPointTime.currentSlot
            let outcome = update bobEnv ledger now stWithPrior fanoutHead
            monitor $ counterexample ("Outcome: " <> show outcome)
            run $
              outcome `hasStateChangedSatisfying` \case
                HeadFannedOut{finalizedOutputs} -> finalizedOutputs == priorDistributed <> fanoutUTxO
                _ -> False

      prop "ignores partial fanoutTx of another head" $ \otherHeadId distributedUTxO -> do
        let partialFanoutOtherHead = observeTx $ OnPartialFanoutTx{headId = otherHeadId, distributedOutputs = distributedUTxO}
            st = inClosedState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        update bobEnv ledger now st partialFanoutOtherHead
          `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

      prop "partial fanout emits HeadPartialFannedOut and triggers next fanout" $ \distributedUTxO ->
        forAllShrink genClosedState shrink $ \closedState -> monadicIO $ do
          let partialFanoutHead = observeTx $ OnPartialFanoutTx{headId = testHeadId, distributedOutputs = distributedUTxO}
          now <- run $ nowFromSlot closedState.chainPointTime.currentSlot
          let outcome = update bobEnv ledger now closedState partialFanoutHead
          monitor $ counterexample ("Outcome: " <> show outcome)
          -- Should emit HeadPartialFannedOut state change
          run $
            outcome `hasStateChangedSatisfying` \case
              HeadPartialFannedOut{} -> True
              _ -> False
          -- Should also trigger the next fanout automatically. After observing a
          -- PartialFanout the phase is always FanoutInProgress, so HeadLogic
          -- always emits FinalPartialFanoutTx (Handlers decides whether to
          -- actually do a partial chunk or finalize based on tx evaluation).
          run $
            outcome `hasEffectSatisfying` \case
              OnChainEffect{postChainTx = FinalPartialFanoutTx{}} -> True
              _ -> False

      it "partial fanout with large remaining triggers FinalPartialFanoutTx" $ do
        let bigRemaining = Set.fromList [SimpleTxOut i | i <- [1 .. fromIntegral fanoutOutputThreshold + 1]]
            st = inClosedStateWithRemaining threeParties (Just bigRemaining)
        now <- nowFromSlot st.chainPointTime.currentSlot
        let outcome = update bobEnv ledger now st (observeTx OnPartialFanoutTx{headId = testHeadId, distributedOutputs = mempty})
        outcome `hasEffectSatisfying` \case
          OnChainEffect{postChainTx = FinalPartialFanoutTx{utxoToDistribute}} ->
            Set.size utxoToDistribute == Set.size bigRemaining
          _ -> False

      it "partial fanout reduces remainingUTxO by distributedUTxO" $ do
        let allItems = Set.fromList [SimpleTxOut i | i <- [1 .. fromIntegral fanoutOutputThreshold + fromIntegral fanoutChunkSize + 1]]
            (distributedList, _) = splitAt fanoutChunkSize (Set.toList allItems)
            distributed = Set.fromList distributedList
            st = inClosedStateWithRemaining threeParties (Just allItems)
        now <- nowFromSlot st.chainPointTime.currentSlot
        let outcome = update bobEnv ledger now st (observeTx OnPartialFanoutTx{headId = testHeadId, distributedOutputs = distributed})
        outcome `hasStateChangedSatisfying` \case
          HeadPartialFannedOut{remainingOutputs} ->
            Set.size remainingOutputs == Set.size allItems - Set.size distributed
          _ -> False

      it "partial fanout of non-prefix items tracks remaining by content, not by count" $ do
        -- An adversary can submit a valid partial fanout of UTxOs that are NOT the
        -- prefix of the full set. The remaining computation must use content-based
        -- set-difference, not a count-based positional split.
        let total = fanoutOutputThreshold + fanoutChunkSize + 1
            allItems = Set.fromList [SimpleTxOut i | i <- [1 .. fromIntegral total]]
            -- Adversarial distribution: the LAST fanoutChunkSize items, not the first.
            attackedDistributed =
              Set.fromList
                [SimpleTxOut i | i <- [fromIntegral (total - fanoutChunkSize + 1) .. fromIntegral total]]
            st = inClosedStateWithRemaining threeParties (Just allItems)
        now <- nowFromSlot st.chainPointTime.currentSlot
        let outcome =
              update
                bobEnv
                ledger
                now
                st
                (observeTx OnPartialFanoutTx{headId = testHeadId, distributedOutputs = attackedDistributed})
        outcome `hasStateChangedSatisfying` \case
          HeadPartialFannedOut{remainingOutputs} ->
            remainingOutputs == Set.difference allItems attackedDistributed
          _ -> False

      it "partial fanout with small remaining triggers FinalPartialFanoutTx" $ do
        let smallRemaining = Set.fromList [SimpleTxOut 1, SimpleTxOut 2]
            st = inClosedStateWithRemaining threeParties (Just smallRemaining)
        now <- nowFromSlot st.chainPointTime.currentSlot
        let outcome = update bobEnv ledger now st (observeTx OnPartialFanoutTx{headId = testHeadId, distributedOutputs = mempty})
        outcome `hasEffectSatisfying` \case
          OnChainEffect{postChainTx = FinalPartialFanoutTx{utxoToDistribute}} -> utxoToDistribute == smallRemaining
          _ -> False

      it "client fanout uses remaining UTxO after partial fanout (FinalPartialFanoutTx)" $ do
        let remaining = Set.fromList [SimpleTxOut 3, SimpleTxOut 4]
            st = inClosedStateWithRemaining threeParties (Just remaining)
        now <- nowFromSlot st.chainPointTime.currentSlot
        let outcome = update bobEnv ledger now st (ClientInput Fanout)
        outcome `hasEffectSatisfying` \case
          OnChainEffect{postChainTx = FinalPartialFanoutTx{utxoToDistribute}} -> utxoToDistribute == remaining
          _ -> False

      it "client fanout without prior partial fanout uses full snapshot utxo" $ do
        let st = inClosedState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        let outcome = update bobEnv ledger now st (ClientInput Fanout)
        outcome `hasEffectSatisfying` \case
          OnChainEffect{postChainTx = FanoutTx{utxo}} -> utxo == mempty
          _ -> False

      it "client fanout always triggers FanoutTx for FreshFanout phase" $ do
        let bigUTxO = Set.fromList [SimpleTxOut i | i <- [1 .. fromIntegral fanoutOutputThreshold + 1]]
            snap = testSnapshot 1 0 [] bigUTxO
            st = inClosedState' threeParties (ConfirmedSnapshot snap (Crypto.aggregate []))
        now <- nowFromSlot st.chainPointTime.currentSlot
        let outcome = update bobEnv ledger now st (ClientInput Fanout)
        outcome `hasEffectSatisfying` \case
          OnChainEffect{postChainTx = FanoutTx{utxo}} -> utxo == bigUTxO
          _ -> False

      it "client fanout resumes from remaining UTxOs when prior step posting failed due to insufficient funds" $ do
        -- Build a snapshot large enough to require at least two partial fanout steps:
        -- first step distributes fanoutChunkSize, leaving fanoutOutputThreshold + 1 → FinalPartialFanoutTx.
        let totalCount = fanoutOutputThreshold + fanoutChunkSize + 1
            allUTxO = Set.fromList [SimpleTxOut i | i <- [1 .. fromIntegral totalCount]]
            snap = testSnapshot 1 0 [] allUTxO
            initialSt = inClosedState' threeParties (ConfirmedSnapshot snap (Crypto.aggregate []))
        -- Compute exactly what the first partial fanout step would have distributed.
        let (distributed1List, remaining1List) = splitAt fanoutChunkSize (Set.toList allUTxO)
            distributed1 = Set.fromList distributed1List
            remaining1 = Set.fromList remaining1List
        -- Step 1 was confirmed on-chain (HeadPartialFannedOut observed).
        -- The auto-emitted step 2 effect was never posted because the operator
        -- ran out of funds. State stays Closed with remainingFanoutUTxO = Just remaining1.
        let step1 =
              HeadPartialFannedOut
                { headId = testHeadId
                , distributedOutputs = distributed1
                , remainingOutputs = remaining1
                , chainState = SimpleChainState{slot = 0}
                }
            stAfterStep1 = aggregateState initialSt (newState step1)
        case headState stAfterStep1 of
          Closed ClosedState{remainingFanoutOutputs} ->
            remainingFanoutOutputs `shouldBe` Just remaining1
          other -> failure $ "Expected Closed state, got: " <> show other
        -- Operator tops up funds and calls Fanout again.
        now <- nowFromSlot stAfterStep1.chainPointTime.currentSlot
        let outcome = update bobEnv ledger now stAfterStep1 (ClientInput Fanout)
        -- Must continue from remaining1: the next chunk is disjoint from distributed1.
        -- If fanout restarted from scratch it would re-distribute distributed1,
        -- making Set.disjoint fail.
        outcome `hasEffectSatisfying` \case
          OnChainEffect{postChainTx = FinalPartialFanoutTx{utxoToDistribute}} ->
            Set.isSubsetOf utxoToDistribute remaining1
              && Set.disjoint utxoToDistribute distributed1
          _ -> False

      it "StalePartialFanoutTx PostTxError is silently ignored (no PostTxOnChainFailed to client)" $ do
        -- This covers the race condition where another node already posted the
        -- same partial fanout step. The chain observation loop self-heals by
        -- emitting the correct next effect; the stale attempt must not surface
        -- as an error to the API client.
        let st = inClosedState threeParties
        now <- nowFromSlot st.chainPointTime.currentSlot
        let postTxError = ChainInput PostTxError{postChainTx = FinalPartialFanoutTx{utxoToDistribute = mempty, presettledUTxO = mempty, headSeed = testHeadSeed, contestationDeadline = arbitrary `generateWith` 42}, postTxError = StalePartialFanoutTx, failingTx = Nothing}
            outcome = update bobEnv ledger now st postTxError
        outcome `hasNoEffectSatisfying` \case
          ClientEffect{} -> True
          _ -> False

      it "aggregate HeadPartialFannedOut keeps state Closed with remaining UTxO" $ do
        let remaining = Set.fromList [SimpleTxOut 5, SimpleTxOut 6]
            distributed = Set.fromList [SimpleTxOut 1, SimpleTxOut 2]
            closedSt = inClosedState threeParties
        case headState closedSt of
          Closed{} ->
            let stateChange =
                  HeadPartialFannedOut
                    { headId = testHeadId
                    , distributedOutputs = distributed
                    , remainingOutputs = remaining
                    , chainState = SimpleChainState{slot = 0}
                    }
                result = aggregateState closedSt (newState stateChange)
             in case headState result of
                  Closed ClosedState{remainingFanoutOutputs, distributedFanoutOutputs} -> do
                    remainingFanoutOutputs `shouldBe` Just remaining
                    distributedFanoutOutputs `shouldBe` distributed
                  other -> failure $ "Expected Closed state, got: " <> show other
          other -> failure $ "Expected Closed state, got: " <> show other

      it "aggregate HeadPartialFannedOut accumulates distributedFanoutUTxO across steps" $ do
        let firstDistributed = Set.fromList [SimpleTxOut 1, SimpleTxOut 2]
            secondDistributed = Set.fromList [SimpleTxOut 3, SimpleTxOut 4]
            remaining = Set.fromList [SimpleTxOut 5, SimpleTxOut 6]
            closedSt = inClosedState threeParties
        case headState closedSt of
          Closed{} ->
            let step1 =
                  HeadPartialFannedOut
                    { headId = testHeadId
                    , distributedOutputs = firstDistributed
                    , remainingOutputs = secondDistributed <> remaining
                    , chainState = SimpleChainState{slot = 0}
                    }
                afterStep1 = aggregateState closedSt (newState step1)
                step2 =
                  HeadPartialFannedOut
                    { headId = testHeadId
                    , distributedOutputs = secondDistributed
                    , remainingOutputs = remaining
                    , chainState = SimpleChainState{slot = 0}
                    }
                afterStep2 = aggregateState afterStep1 (newState step2)
             in case headState afterStep2 of
                  Closed ClosedState{distributedFanoutOutputs} ->
                    distributedFanoutOutputs `shouldBe` firstDistributed <> secondDistributed
                  other -> failure $ "Expected Closed state, got: " <> show other
          other -> failure $ "Expected Closed state, got: " <> show other

      describe "SideLoad InitialSnapshot" $ do
        it "accept side load initial snapshot with idempotence" $ do
          let s0 = inOpenState threeParties
              initialSn = InitialSnapshot @SimpleTx testHeadId
              snapshot0 = getSnapshot initialSn
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0
          sideLoadedState <- runHeadLogic bobEnv ledger s0 $ do
            step $ ClientInput (SideLoadSnapshot initialSn)
            getState
          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot0

        prop "ignores side load initial snapshot of another head" $ \otherHeadId -> do
          let s0 = inOpenState threeParties
              initialSn = InitialSnapshot @SimpleTx testHeadId
              snapshot0 = getSnapshot initialSn
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0
          let initialSnapshotOtherHead = InitialSnapshot otherHeadId
          now <- nowFromSlot s0.chainPointTime.currentSlot
          update bobEnv ledger now s0 (ClientInput (SideLoadSnapshot initialSnapshotOtherHead))
            `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})
          getConfirmedSnapshot s0 `shouldBe` Just snapshot0

      describe "SideLoad ConfirmedSnapshot" $ do
        -- Given a list of transactions each depending on the previous.
        let tx1 = SimpleTx 1 mempty (utxoRef 2) -- No inputs, requires no specific starting state
            tx2 = SimpleTx 2 (utxoRef 2) (utxoRef 3)
            tx3 = SimpleTx 3 (utxoRef 3) (utxoRef 4)
            snapshot1 = testSnapshot 1 0 [tx1] (utxoRef 2)
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
                  , localTxs = Seq.fromList [tx2, tx3]
                  , confirmedSnapshot = ConfirmedSnapshot snapshot1 multisig1
                  , seenSnapshot = RequestedSnapshot{lastSeen = 1, requested = 2}
                  }

        it "accept new side load confirmed snapshot" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = testSnapshot 2 0 [tx2] (utxoRef 3)
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2, sign carolSk snapshot2]

          sideLoadedState <- runHeadLogic bobEnv ledger startingState $ do
            step $ ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2)
            getState

          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot2

        it "reject side load confirmed snapshot because old snapshot number" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = testSnapshot 2 0 [tx2] (utxoRef 3)
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2, sign carolSk snapshot2]

          sideLoadedState <- runHeadLogic bobEnv ledger startingState $ do
            step $ ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2)
            getState

          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot2

          now <- nowFromSlot sideLoadedState.chainPointTime.currentSlot
          let outcome = update bobEnv ledger now sideLoadedState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot1 multisig1))
          outcome `hasEffectSatisfying` \case
            ClientEffect (SideLoadSnapshotRejected{requirementFailure = SideLoadSnNumberInvalid 1 2}) -> True
            _ -> False

        it "reject side load confirmed snapshot because missing signature" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = testSnapshot 2 0 [tx2] (utxoRef 3)
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          now <- nowFromSlot startingState.chainPointTime.currentSlot
          let outcome = update bobEnv ledger now startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
          outcome `hasEffectSatisfying` \case
            ClientEffect (SideLoadSnapshotRejected{requirementFailure = SideLoadInvalidMultisignature{vkeys}}) ->
              vkeys == [vkey alice, vkey bob, vkey carol]
            _ -> False

        it "reject side load confirmed snapshot because wrong snapshot version" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot2 = testSnapshot 2 1 [tx2] (utxoRef 3)
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          now <- nowFromSlot startingState.chainPointTime.currentSlot
          let outcome = update bobEnv ledger now startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
          outcome `hasEffectSatisfying` \case
            ClientEffect (SideLoadSnapshotRejected{requirementFailure = SideLoadSvNumberInvalid 1 0}) -> True
            _ -> False

        prop "reject side load confirmed snapshot because wrong snapshot utxoToDecommit" $ \utxoToDecommit -> do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1
          let utxo' = utxoRef 3
              utxoToDecom = Just utxoToDecommit
              accumulator = Accumulator.buildFromSnapshotUTxOs utxo' Nothing utxoToDecom
              snapshot2 = Snapshot testHeadId 0 2 [tx2] utxo' Nothing utxoToDecom accumulator
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          now <- nowFromSlot startingState.chainPointTime.currentSlot
          let outcome = update bobEnv ledger now startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
          outcome `hasEffectSatisfying` \case
            ClientEffect (SideLoadSnapshotRejected{requirementFailure = SideLoadUTxOToDecommitInvalid (Just utxoToDecommit') Nothing}) ->
              utxoToDecommit' == utxoToDecommit
            _ -> False

        prop "reject side load confirmed snapshot because wrong snapshot utxoToCommit" $ \utxoToCommit -> do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let utxo' = utxoRef 3
              utxoToCom = Just utxoToCommit
              accumulator = Accumulator.buildFromSnapshotUTxOs utxo' utxoToCom Nothing
              snapshot2 = Snapshot testHeadId 0 2 [tx2] utxo' utxoToCom Nothing accumulator
              multisig2 = aggregate [sign aliceSk snapshot2, sign bobSk snapshot2]

          now <- nowFromSlot startingState.chainPointTime.currentSlot
          let outcome = update bobEnv ledger now startingState (ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot2 multisig2))
          outcome `hasEffectSatisfying` \case
            ClientEffect (SideLoadSnapshotRejected{requirementFailure = SideLoadUTxOToCommitInvalid (Just utxoToCommit') Nothing}) ->
              utxoToCommit' == utxoToCommit
            _ -> False

        it "accept side load confirmed snapshot with idempotence" $ do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          sideLoadedState <- runHeadLogic bobEnv ledger startingState $ do
            step $ ClientInput (SideLoadSnapshot $ ConfirmedSnapshot snapshot1 multisig1)
            getState

          getConfirmedSnapshot sideLoadedState `shouldBe` Just snapshot1

        prop "ignores side load confirmed snapshot of another head" $ \otherHeadId -> do
          getConfirmedSnapshot startingState `shouldBe` Just snapshot1

          let snapshot1OtherHead = (testSnapshot 1 0 [tx1] (utxoRef 2)){headId = otherHeadId} :: Snapshot SimpleTx
              multisig1OtherHead = aggregate [sign aliceSk snapshot1OtherHead, sign bobSk snapshot1OtherHead, sign carolSk snapshot1OtherHead]
              confirmedSnapshotOtherHead = ConfirmedSnapshot snapshot1OtherHead multisig1OtherHead

          now <- nowFromSlot startingState.chainPointTime.currentSlot
          update bobEnv ledger now startingState (ClientInput (SideLoadSnapshot confirmedSnapshotOtherHead))
            `shouldBe` Error (NotOurHead{ourHeadId = testHeadId, otherHeadId})

        it "recovered deposit must not appear in new snapshot utxo after sideload" $ do
          -- Regression test for https://github.com/cardano-scaling/hydra/issues/2629
          --
          -- After a deposit snapshot is confirmed but IncrementTx never finalises
          -- and the deposit is recovered on L1, sideloading that snapshot and then
          -- triggering a new L2 snapshot must not reintroduce the recovered deposit
          -- into the new snapshot's utxo.
          now <- getCurrentTime
          let plusTime = flip addUTCTime
              depositTime = plusTime now
              depositTxId = 42 :: Integer
              depositedUTxO = utxoRef depositTxId
              aliceEnv' =
                aliceEnv
                  { otherParties = []
                  , participants = deriveOnChainId <$> [alice]
                  }
              singleParty = [alice]
              activeDeposit =
                Deposit
                  { headId = testHeadId
                  , deposited = depositedUTxO
                  , created = depositTime 1
                  , deadline = depositTime 600
                  , status = Active
                  }
              depositSnapshot =
                Snapshot
                  { headId = testHeadId
                  , version = 0
                  , number = 1
                  , confirmed = []
                  , utxo = mempty
                  , utxoToCommit = Just depositedUTxO
                  , utxoToDecommit = Nothing
                  , accumulator = Accumulator.buildFromSnapshotUTxOs mempty (Just depositedUTxO) Nothing
                  }
              depositMultisig = aggregate [sign aliceSk depositSnapshot]
          -- Start with deposit already active and tracked
          let s0 =
                ( inOpenState' singleParty $
                    coordinatedHeadState{currentDepositTxId = Just depositTxId}
                )
                  { pendingDeposits = Map.singleton depositTxId activeDeposit
                  }
          -- Step 1: Confirm snapshot 1 including the deposit in utxoToCommit
          s1 <- runHeadLogic aliceEnv' ledger s0 $ do
            step $ receiveMessage $ ReqSn 0 1 [] Nothing (Just depositTxId)
            step $ receiveMessage $ AckSn (sign aliceSk depositSnapshot) 1
            getState
          -- Step 2: Recover the deposit (IncrementTx never finalized on-chain)
          s2 <- runHeadLogic aliceEnv' ledger s1 $ do
            step $
              observeTxAtSlot 2 $
                OnRecoverTx
                  { headId = testHeadId
                  , recoveredTxId = depositTxId
                  , recoveredUTxO = depositedUTxO
                  }
            getState
          Map.lookup depositTxId s2.pendingDeposits `shouldBe` Nothing
          -- Step 3: Sideload snapshot 1 to unstick the head
          s3 <- runHeadLogic aliceEnv' ledger s2 $ do
            step $ ClientInput $ SideLoadSnapshot $ ConfirmedSnapshot depositSnapshot depositMultisig
            getState
          -- Step 4: Submit a new L2 transaction
          let newTx = aValidTx 1
          s4 <- runHeadLogic aliceEnv' ledger s3 $ do
            step $ receiveMessage $ ReqTx newTx
            getState
          -- Step 5: Process the new snapshot request — the recovered deposit must not
          -- appear in the new snapshot's utxo field
          let reqSn2 = receiveMessage $ ReqSn 0 2 [txId newTx] Nothing Nothing
          outcome <- runHeadLogic aliceEnv' ledger s4 $ step reqSn2
          -- Extract the new snapshot's utxo and show both sides of the invariant:
          -- depositedUTxO is on L1 (it was recovered), and must not also be on L2.
          case outcome of
            Continue{stateChanges} -> do
              let mSnapUtxo =
                    listToMaybe
                      [ utxo
                      | SnapshotRequested{requestedSnapshot = Snapshot{utxo}} <- stateChanges
                      ]
              case mSnapUtxo of
                Nothing -> expectationFailure "Expected SnapshotRequested state change"
                Just snapUtxo ->
                  -- The intersection of {recovered L1 deposit} and {new L2 snapshot utxo}
                  -- must be empty — the same UTxO cannot exist on both layers.
                  Set.intersection depositedUTxO snapUtxo `shouldBe` mempty
            _ -> expectationFailure $ "Expected Continue outcome, got: " <> show outcome

    describe "Coordinated Head Protocol using real Tx" $ do
      let ledger = cardanoLedger Fixture.defaultGlobals Fixture.defaultLedgerEnv
      prop "on tick, picks the next active deposit in arrival when in Open state order for ReqSn" $ \now -> monadicIO $ do
        let singleParty = [alice]
            plusTime = flip addUTCTime
            depositTime = plusTime now
        let deadline = depositTime 5 `plusTime` toNominalDiffTime (depositPeriod aliceEnv) `plusTime` toNominalDiffTime (depositPeriod aliceEnv)
        -- party payment keys
        (vk, sk) <- pick genKeyPair
        -- helper to build deposit tx
        let mkDepositTx = do
              txOut <- genOutputFor vk
              utxo <- (,txOut) <$> genTxIn
              mkSimpleTx
                utxo
                (mkVkAddress Fixture.testNetworkId vk, txOutValue txOut)
                (mkSecret sk)
                & \case
                  Left _ -> Prelude.error "cannot generate deposit tx"
                  Right tx -> pure (uncurry UTxO.singleton utxo, tx)
        -- single party on empty Open state
        let st0 =
              inSync $
                Open
                  OpenState
                    { parameters = HeadParameters defaultContestationPeriod singleParty
                    , coordinatedHeadState =
                        CoordinatedHeadState
                          { localUTxO = mempty
                          , allTxs = mempty
                          , localTxs = mempty
                          , confirmedSnapshot = InitialSnapshot testHeadId
                          , seenSnapshot = NoSeenSnapshot
                          , currentDepositTxId = Nothing
                          , decommitTx = Nothing
                          , version = 0
                          }
                    , chainState = ChainStateAt{spendableUTxO = mempty, recordedAt = Nothing}
                    , headId = testHeadId
                    , headSeed = testHeadSeed
                    }
        -- deposit txs
        (deposited1, depositTx1) <- pick mkDepositTx
        (deposited2, depositTx2) <- pick mkDepositTx
        (deposited3, depositTx3) <- pick mkDepositTx
        -- deposit observations
        let observeRealTxAtSlot slot observedTx = do
              nextBlockHash <- genBlockHeaderHash
              pure
                ChainInput
                  { chainEvent =
                      Observation
                        { observedTx
                        , newChainState = ChainStateAt{spendableUTxO = mempty, recordedAt = Just $ ChainPoint slot nextBlockHash}
                        }
                  }
            deposit1 = OnDepositTx{headId = testHeadId, depositTxId = txId depositTx1, deposited = deposited1, created = depositTime 1, deadline}
            deposit2 = OnDepositTx{headId = testHeadId, depositTxId = txId depositTx2, deposited = deposited2, created = depositTime 2, deadline}
            deposit3 = OnDepositTx{headId = testHeadId, depositTxId = txId depositTx3, deposited = deposited3, created = depositTime 3, deadline}
        depositObservation1 <- pick (hedgehog $ observeRealTxAtSlot 1 deposit1)
        depositObservation2 <- pick (hedgehog $ observeRealTxAtSlot 2 deposit2)
        depositObservation3 <- pick (hedgehog $ observeRealTxAtSlot 3 deposit3)
        nodeState <-
          run $
            runHeadLogic bobEnv ledger st0 $ do
              step depositObservation1
              step depositObservation2
              step depositObservation3
              getState

        -- XXX: chainTime should be > created + depositPeriod && < deadline - depositPeriod
        -- so deposits are considered Active
        let chainTime = depositTime 4 `plusTime` toNominalDiffTime (depositPeriod aliceEnv)
        blockHash' <- pick (hedgehog genBlockHeaderHash)
        let input = ChainInput $ Tick{chainTime, chainPoint = ChainPoint 4 blockHash'}

        let outcome = update aliceEnv ledger now nodeState input

        forM_ [txId depositTx1, txId depositTx2, txId depositTx3] $ \depositId ->
          assert $ case outcome of
            Wait{} -> False
            Error{} -> False
            Continue{stateChanges} ->
              any
                ( \case
                    DepositActivated{depositTxId} -> depositTxId == depositId
                    _ -> False
                )
                stateChanges

        assert $ case outcome of
          Wait{} -> False
          Error{} -> False
          Continue{effects} ->
            any
              ( \case
                  NetworkEffect ReqSn{depositTxId} -> depositTxId == Just (txId depositTx1)
                  _ -> False
              )
              effects

      prop "any tx with expiring upper validity range gets pruned" $ \slotNo -> monadicIO $ do
        (utxo, expiringTransaction) <- pick $ do
          (vk, sk) <- genKeyPair
          txOut <- genOutputFor vk
          utxo <- (,txOut) <$> genTxIn
          mkRangedTx
            utxo
            (mkVkAddress Fixture.testNetworkId vk, txOutValue txOut)
            (mkSecret sk)
            (Nothing, Just $ TxValidityUpperBound slotNo)
            & \case
              Left _ -> Prelude.error "cannot generate expired tx"
              Right tx -> pure (utxo, tx)
        chainTime <- run getCurrentTime
        let st0 =
              NodeInSync
                { headState =
                    Open
                      OpenState
                        { parameters = HeadParameters defaultContestationPeriod threeParties
                        , coordinatedHeadState =
                            CoordinatedHeadState
                              { localUTxO = uncurry UTxO.singleton utxo
                              , allTxs = mempty
                              , localTxs = pure expiringTransaction
                              , confirmedSnapshot = InitialSnapshot testHeadId
                              , seenSnapshot = NoSeenSnapshot
                              , currentDepositTxId = Nothing
                              , decommitTx = Nothing
                              , version = 0
                              }
                        , chainState = ChainStateAt{spendableUTxO = mempty, recordedAt = Nothing}
                        , headId = testHeadId
                        , headSeed = testHeadSeed
                        }
                , pendingDeposits = mempty
                , chainPointTime =
                    ChainPointTime
                      { currentSlot = ChainSlot . fromIntegral . unSlotNo $ slotNo + 1
                      , currentChainTime = chainTime
                      , drift = 0
                      }
                }

        st <-
          run $
            runHeadLogic bobEnv ledger st0 $ do
              step (receiveMessage $ ReqSn 0 1 [] Nothing Nothing)
              getState

        assert $ case headState st of
          Open
            OpenState
              { coordinatedHeadState =
                CoordinatedHeadState{localTxs}
              } -> null localTxs
          _ -> False

    prop "empty inputs in decommit tx are prevented" $ \tx -> do
      chainTime <- getCurrentTime
      let ledger = cardanoLedger Fixture.defaultGlobals Fixture.defaultLedgerEnv
      let st =
            NodeInSync
              { headState =
                  Open
                    OpenState
                      { parameters = HeadParameters defaultContestationPeriod threeParties
                      , coordinatedHeadState =
                          CoordinatedHeadState
                            { localUTxO = mempty
                            , allTxs = mempty
                            , localTxs = mempty
                            , confirmedSnapshot = InitialSnapshot testHeadId
                            , seenSnapshot = NoSeenSnapshot
                            , currentDepositTxId = Nothing
                            , decommitTx = Nothing
                            , version = 0
                            }
                      , chainState = ChainStateAt{spendableUTxO = mempty, recordedAt = Nothing}
                      , headId = testHeadId
                      , headSeed = testHeadSeed
                      }
              , pendingDeposits = mempty
              , chainPointTime =
                  ChainPointTime
                    { currentSlot = ChainSlot 1
                    , currentChainTime = chainTime
                    , drift = 0
                    }
              }

      let tx' = fromLedgerTx (toLedgerTx tx & bodyTxL . inputsTxBodyL .~ mempty)
      let input = receiveMessage $ ReqDec{transaction = tx'}
      now <- nowFromSlot st.chainPointTime.currentSlot
      update bobEnv ledger now st input `shouldSatisfy` \case
        Wait WaitOnNotApplicableDecommitTx{notApplicableReason = DecommitTxInvalid{}} _ -> True
        _ -> False

-- * Properties

prop_ignoresUnrelatedOnInitTx :: Property
prop_ignoresUnrelatedOnInitTx =
  forAll arbitrary $ \env ->
    forAll (genUnrelatedInit env) $ \unrelatedInit -> monadicIO $ do
      let idleSt = inIdleState
      now <- run getCurrentTime
      let outcome = update env simpleLedger now idleSt (observeTx unrelatedInit)
      monitor $ counterexample ("Outcome: " <> show outcome)
      run $
        outcome `hasStateChangedSatisfying` \case
          IgnoredHeadInitializing{} -> True
          _ -> False
 where
  genUnrelatedInit :: Environment -> Gen (OnChainTx tx)
  genUnrelatedInit env =
    oneof
      [ genOnInitWithDifferentContestationPeriod env
      , genOnInitWithoutParty env
      , genOnInitWithoutOnChainId env
      ]

  genOnInitWithDifferentContestationPeriod :: Environment -> Gen (OnChainTx tx)
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

  genOnInitWithoutParty :: Environment -> Gen (OnChainTx tx)
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

  genOnInitWithoutOnChainId :: Environment -> Gen (OnChainTx tx)
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

genClosedState :: Gen (NodeState SimpleTx)
genClosedState = do
  closedState <- arbitrary
  pure $ inSync (Closed $ closedState{headId = testHeadId, remainingFanoutOutputs = Nothing, distributedFanoutOutputs = mempty})

-- * Utilities

-- | Create a network input about a received protocol message with default ttl
-- and 'alice' as the sender.
receiveMessage :: Message tx -> Input tx
receiveMessage = receiveMessageFrom alice

-- | Create a network input about a received protocol message with default ttl
-- from given sender.
receiveMessageFrom :: Party -> Message tx -> Input tx
receiveMessageFrom = mkNetworkInput

-- | Create a chain effect with fixed chain state and slot.
chainEffect :: PostChainTx SimpleTx -> Effect SimpleTx
chainEffect postChainTx =
  OnChainEffect
    { postChainTx
    }

-- | Create an observation chain input with chain state at given slot.
observeTxAtSlot :: ChainSlot -> OnChainTx SimpleTx -> Input SimpleTx
observeTxAtSlot slot observedTx =
  ChainInput
    { chainEvent =
        Observation
          { observedTx
          , newChainState = SimpleChainState{slot}
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

inIdleState :: NodeState SimpleTx
inIdleState = initNodeState 0

inUnsyncedIdleState :: NodeState SimpleTx
inUnsyncedIdleState = catchingUp (Idle IdleState{chainState = 0})

-- XXX: This is always called with threeParties and simpleLedger
inOpenState ::
  [Party] ->
  NodeState SimpleTx
inOpenState parties =
  inOpenState' parties $
    CoordinatedHeadState
      { localUTxO = u0
      , allTxs = mempty
      , localTxs = mempty
      , confirmedSnapshot
      , seenSnapshot = NoSeenSnapshot
      , currentDepositTxId = Nothing
      , decommitTx = Nothing
      , version = 0
      }
 where
  u0 = mempty
  confirmedSnapshot = InitialSnapshot @SimpleTx testHeadId

inOpenState' ::
  [Party] ->
  CoordinatedHeadState SimpleTx ->
  NodeState SimpleTx
inOpenState' parties coordinatedHeadState =
  inSync $
    Open
      OpenState
        { parameters
        , coordinatedHeadState
        , chainState = 0
        , headId = testHeadId
        , headSeed = testHeadSeed
        }
 where
  parameters = HeadParameters defaultContestationPeriod parties

-- XXX: This is always called with 'threeParties'
inClosedState :: [Party] -> NodeState SimpleTx
inClosedState parties = inClosedState' parties snapshot0
 where
  snapshot0 = InitialSnapshot @SimpleTx testHeadId

inClosedState' :: [Party] -> ConfirmedSnapshot SimpleTx -> NodeState SimpleTx
inClosedState' parties confirmedSnapshot =
  inSync $
    Closed
      ClosedState
        { parameters
        , confirmedSnapshot
        , contestationDeadline
        , readyToFanoutSent = False
        , chainState = 0
        , headId = testHeadId
        , headSeed = testHeadSeed
        , version = 0
        , remainingFanoutOutputs = Nothing
        , distributedFanoutOutputs = mempty
        }
 where
  parameters = HeadParameters defaultContestationPeriod parties

  contestationDeadline = arbitrary `generateWith` 42

inClosedStateWithRemaining :: [Party] -> Maybe (Set SimpleTxOut) -> NodeState SimpleTx
inClosedStateWithRemaining parties remaining =
  inSync $
    Closed
      ClosedState
        { parameters
        , -- Use a snapshot whose utxo matches the remaining set so that
          -- filterUTxOByOutputs can find entries when emitNextFanoutStep
          -- converts Set (TxOutType tx) back to UTxOType tx.
          confirmedSnapshot =
            ConfirmedSnapshot
              (testSnapshot 0 0 [] (fromMaybe mempty remaining))
              (Crypto.aggregate [])
        , contestationDeadline
        , readyToFanoutSent = False
        , chainState = 0
        , headId = testHeadId
        , headSeed = testHeadSeed
        , version = 0
        , remainingFanoutOutputs = remaining
        , distributedFanoutOutputs = mempty
        }
 where
  parameters = HeadParameters defaultContestationPeriod parties
  contestationDeadline = arbitrary `generateWith` 42

getConfirmedSnapshot :: IsTx tx => NodeState tx -> Maybe (Snapshot tx)
getConfirmedSnapshot = \case
  NodeInSync{headState = Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}}} ->
    Just (getSnapshot confirmedSnapshot)
  _ ->
    Nothing

data StepState tx = StepState
  { nodeState :: NodeState tx
  , env :: Environment
  , ledger :: Ledger tx
  }

runHeadLogic ::
  Monad m =>
  Environment ->
  Ledger tx ->
  NodeState tx ->
  StateT (StepState tx) m a ->
  m a
runHeadLogic env ledger nodeState = (`evalStateT` StepState{env, ledger, nodeState})

-- | Retrieves the latest 'NodeState' from within 'runHeadLogic'.
getState :: MonadState (StepState tx) m => m (NodeState tx)
getState = nodeState <$> get

-- | Calls 'update' and 'aggregate' to drive the 'runHeadLogic' monad forward.
step ::
  (MonadState (StepState tx) m, IsChainState tx, MonadTime m) =>
  Input tx ->
  m (Outcome tx)
step input = do
  StepState{nodeState, env, ledger} <- get
  now <- getCurrentTime
  let outcome = update env ledger now nodeState input
  let nodeState' = aggregateState nodeState outcome
  put StepState{env, ledger, nodeState = nodeState'}
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

nowFromSlot :: (MonadFail m, MonadTime m) => ChainSlot -> m UTCTime
nowFromSlot (ChainSlot slotNo) = do
  timeHandle <- mkTimeHandleAt (fromIntegral slotNo) <$> getCurrentTime
  case slotToUTCTime timeHandle (fromIntegral slotNo) of
    Left err -> fail $ "nowFromSlot: " <> show err
    Right time -> pure time

-- | Create a 'TimeHandle' for a given slot and current wall-clock time.
-- * Assumes 1-second slots (as in the devnet).
-- * SystemStart is derived so that the given slot corresponds to the given time.
-- * EraHistory is constructed to have a horizon sufficiently far in the future to accommodate time conversions.
mkTimeHandleAt :: SlotNo -> UTCTime -> TimeHandle
mkTimeHandleAt slotNo now =
  mkTimeHandle slotNo systemStart eraHistory
 where
  -- Assume 1 slot = 1 second (devnet)
  slotLength :: NominalDiffTime
  slotLength = 1

  -- Compute system start time from the relation: `now = startTime + slotNo * slotLength`
  startTime = addUTCTime (negate $ fromIntegral (unSlotNo slotNo) * slotLength) now
  systemStart = SystemStart startTime

  -- Choose a "safe" horizon far enough beyond the current slot
  horizonSlot = SlotNo $ unSlotNo slotNo + floor safeZone

  -- Construct an era history that's valid up to that horizon
  eraHistory = eraHistoryWithHorizonAt horizonSlot

catchingUp :: IsTx tx => HeadState tx -> NodeState tx
catchingUp headState = NodeCatchingUp{headState, pendingDeposits = mempty, chainPointTime = zeroChainPointTime}

inSync :: IsTx tx => HeadState tx -> NodeState tx
inSync headState = NodeInSync{headState, pendingDeposits = mempty, chainPointTime = zeroChainPointTime}

zeroChainPointTime :: ChainPointTime
zeroChainPointTime =
  ChainPointTime
    { currentSlot = ChainSlot 0
    , currentChainTime = initialChainTime
    , drift = 0
    }

testSnapshot ::
  forall tx.
  IsTx tx =>
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
    , accumulator = Accumulator.buildFromUTxO utxo
    }
