{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Unit tests of the the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Set as Set
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (genTxIn, mkVkAddress, txOutValue, unSlotNo, pattern TxValidityUpperBound)
import Hydra.Chain (
  ChainEvent (..),
  HeadId (..),
  HeadParameters (..),
  IsChainState,
  OnChainTx (..),
  PostChainTx (CollectComTx, ContestTx),
 )
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.State ()
import Hydra.Crypto (aggregate, generateSigningKey, sign)
import Hydra.HeadLogic (
  ClosedState (..),
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  InitialState (..),
  LogicError (..),
  OpenState (..),
  Outcome (..),
  RequirementFailure (..),
  SeenSnapshot (NoSeenSnapshot, SeenSnapshot),
  WaitReason (..),
  defaultTTL,
  update,
 )
import Hydra.Ledger (ChainSlot (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano (cardanoLedger, genKeyPair, genOutput, mkRangedTx)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef)
import Hydra.Network.Message (Message (AckSn, ReqSn, ReqTx))
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (Party (..))
import qualified Hydra.Prelude as Prelude
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Fixture (alice, aliceSk, allVKeys, bob, bobSk, carol, carolSk, cperiod)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

spec :: Spec
spec =
  parallel $ do
    describe "Types" $ do
      roundtripAndGoldenSpecs (Proxy @(Event SimpleTx))
      roundtripAndGoldenSpecs (Proxy @(HeadState SimpleTx))

    let threeParties = [alice, bob, carol]
        bobEnv =
          Environment
            { party = bob
            , signingKey = bobSk
            , otherParties = [alice, carol]
            , contestationPeriod = defaultContestationPeriod
            }

    describe "Coordinated Head Protocol" $ do
      let ledger = simpleLedger

      it "reports if a requested tx is expired" $ do
        let inputs = utxoRef 1
            tx = SimpleTx 2 inputs mempty
            ttl = 0
            reqTx = NetworkEvent ttl $ ReqTx alice tx
            s0 = inOpenState threeParties ledger

        update bobEnv ledger s0 reqTx `hasEffectSatisfying` \case
          ClientEffect TxInvalid{transaction} -> transaction == tx
          _ -> False

      it "waits if a requested tx is not (yet) applicable" $ do
        let reqTx = NetworkEvent defaultTTL $ ReqTx alice $ SimpleTx 2 inputs mempty
            inputs = utxoRef 1
            s0 = inOpenState threeParties ledger

        update bobEnv ledger s0 reqTx `shouldBe` Wait (WaitOnNotApplicableTx (ValidationError "cannot apply transaction"))

      it "confirms snapshot given it receives AckSn from all parties" $ do
        let reqSn = NetworkEvent defaultTTL $ ReqSn alice 1 []
            snapshot1 = Snapshot 1 mempty []
            ackFrom sk vk = NetworkEvent defaultTTL $ AckSn vk (sign sk snapshot1) 1
        snapshotInProgress <- runEvents bobEnv ledger (inOpenState threeParties ledger) $ do
          step reqSn
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)

        getConfirmedSnapshot snapshotInProgress `shouldBe` Just (Snapshot 0 mempty [])

        snapshotConfirmed <- runEvents bobEnv ledger snapshotInProgress $ step (ackFrom bobSk bob)
        getConfirmedSnapshot snapshotConfirmed `shouldBe` Just snapshot1

      it "rejects last AckSn if one signature was from a different snapshot" $ do
        let reqSn = NetworkEvent defaultTTL $ ReqSn alice 1 []
            snapshot = Snapshot 1 mempty []
            snapshot' = Snapshot 2 mempty []
            ackFrom sk vk = NetworkEvent defaultTTL $ AckSn vk (sign sk snapshot) 1
            invalidAckFrom sk vk = NetworkEvent defaultTTL $ AckSn vk (sign sk snapshot') 1
        waitingForLastAck <-
          runEvents bobEnv ledger (inOpenState threeParties ledger) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)

        update bobEnv ledger waitingForLastAck (invalidAckFrom bobSk bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == allVKeys
            _ -> False

      it "rejects last AckSn if one signature was from a different key" $ do
        let reqSn = NetworkEvent defaultTTL $ ReqSn alice 1 []
            snapshot = Snapshot 1 mempty []
            ackFrom sk vk = NetworkEvent defaultTTL $ AckSn vk (sign sk snapshot) 1
        waitingForLastAck <-
          runEvents bobEnv ledger (inOpenState threeParties ledger) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (ackFrom aliceSk alice)

        update bobEnv ledger waitingForLastAck (ackFrom (generateSigningKey "foo") bob)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == allVKeys
            _ -> False

      it "rejects last AckSn if one signature was from a completely different message" $ do
        let reqSn = NetworkEvent defaultTTL $ ReqSn alice 1 []
            snapshot1 = Snapshot 1 mempty []
            ackFrom sk vk = NetworkEvent defaultTTL $ AckSn vk (sign sk snapshot1) 1
            invalidAckFrom sk vk =
              NetworkEvent defaultTTL $
                AckSn vk (coerce $ sign sk ("foo" :: ByteString)) 1
        waitingForLastAck <-
          runEvents bobEnv ledger (inOpenState threeParties ledger) $ do
            step reqSn
            step (ackFrom carolSk carol)
            step (invalidAckFrom bobSk bob)

        update bobEnv ledger waitingForLastAck (ackFrom aliceSk alice)
          `shouldSatisfy` \case
            Error (RequireFailed InvalidMultisignature{vkeys}) -> vkeys == allVKeys
            _ -> False

      it "rejects last AckSn if already received signature from this party" $ do
        let reqSn = NetworkEvent defaultTTL $ ReqSn alice 1 []
            snapshot1 = Snapshot 1 mempty []
            ackFrom sk vk = NetworkEvent defaultTTL $ AckSn vk (sign sk snapshot1) 1
        waitingForAck <-
          runEvents bobEnv ledger (inOpenState threeParties ledger) $ do
            step reqSn
            step (ackFrom carolSk carol)

        update bobEnv ledger waitingForAck (ackFrom carolSk carol)
          `shouldSatisfy` \case
            Error (RequireFailed SnapshotAlreadySigned{receivedSignature}) -> receivedSignature == carol
            _ -> False

      it "waits if we receive a snapshot with not-yet-seen transactions" $ do
        let event = NetworkEvent defaultTTL $ ReqSn alice 1 [SimpleTx 1 (utxoRef 1) (utxoRef 2)]
        update bobEnv ledger (inOpenState threeParties ledger) event
          `shouldBe` Wait (WaitOnNotApplicableTx (ValidationError "cannot apply transaction"))

      it "waits if we receive an AckSn for an unseen snapshot" $ do
        let snapshot = Snapshot 1 mempty []
            event = NetworkEvent defaultTTL $ AckSn alice (sign aliceSk snapshot) 1
        update bobEnv ledger (inOpenState threeParties ledger) event `shouldBe` Wait WaitOnSeenSnapshot

      -- TODO: Write property tests for various future / old snapshot behavior.
      -- That way we could cover variations of snapshot numbers and state of
      -- snapshot collection.

      it "rejects if we receive a too far future snapshot" $ do
        let event = NetworkEvent defaultTTL $ ReqSn bob 2 []
            st = inOpenState threeParties ledger
        update bobEnv ledger st event `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "waits if we receive a future snapshot while collecting signatures" $ do
        let reqSn1 = NetworkEvent defaultTTL $ ReqSn alice 1 []
            reqSn2 = NetworkEvent defaultTTL $ ReqSn bob 2 []
        st <-
          runEvents bobEnv ledger (inOpenState threeParties ledger) $
            step reqSn1

        update bobEnv ledger st reqSn2 `shouldBe` Wait (WaitOnSnapshotNumber 1)

      it "acks signed snapshot from the constant leader" $ do
        let leader = alice
            snapshot = Snapshot 1 mempty []
            event = NetworkEvent defaultTTL $ ReqSn leader (number snapshot) []
            sig = sign bobSk snapshot
            st = inOpenState threeParties ledger
            ack = AckSn bob sig (number snapshot)
        update bobEnv ledger st event `hasEffect` NetworkEffect ack

      it "does not ack snapshots from non-leaders" $ do
        let event = NetworkEvent defaultTTL $ ReqSn notTheLeader 1 []
            notTheLeader = bob
            st = inOpenState threeParties ledger
        update bobEnv ledger st event `shouldSatisfy` \case
          Error (RequireFailed ReqSnNotLeader{requestedSn = 1, leader}) -> leader == notTheLeader
          _ -> False

      it "rejects too-old snapshots" $ do
        let event = NetworkEvent defaultTTL $ ReqSn theLeader 2 []
            theLeader = alice
            snapshot = Snapshot 2 mempty []
            st =
              inOpenState' threeParties $
                CoordinatedHeadState
                  { seenUTxO = mempty
                  , seenTxs = mempty
                  , confirmedSnapshot = ConfirmedSnapshot snapshot (aggregate [])
                  , seenSnapshot = NoSeenSnapshot
                  }
        update bobEnv ledger st event `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)

      it "rejects too-old snapshots when collecting signatures" $ do
        let event = NetworkEvent defaultTTL $ ReqSn theLeader 2 []
            theLeader = alice
            snapshot = Snapshot 2 mempty []
            st =
              inOpenState' threeParties $
                CoordinatedHeadState
                  { seenUTxO = mempty
                  , seenTxs = mempty
                  , confirmedSnapshot = ConfirmedSnapshot snapshot (aggregate [])
                  , seenSnapshot = SeenSnapshot (Snapshot 3 mempty []) mempty
                  }
        update bobEnv ledger st event `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 3)

      it "rejects too-new snapshots from the leader" $ do
        let event = NetworkEvent defaultTTL $ ReqSn theLeader 3 []
            theLeader = carol
            st = inOpenState threeParties ledger
        update bobEnv ledger st event `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 3 0)

      it "rejects overlapping snapshot requests from the leader" $ do
        let theLeader = alice
            nextSN = 1
            firstReqSn = NetworkEvent defaultTTL $ ReqSn theLeader nextSN [aValidTx 42]
            secondReqSn = NetworkEvent defaultTTL $ ReqSn theLeader nextSN [aValidTx 51]
        receivedReqSn <-
          assertUpdateState bobEnv ledger firstReqSn
            `evalStateT` inOpenState threeParties ledger

        update bobEnv ledger receivedReqSn secondReqSn `shouldSatisfy` \case
          Error RequireFailed{} -> True
          _ -> False

      it "ignores in-flight ReqTx when closed" $ do
        let s0 = inClosedState threeParties
            event = NetworkEvent defaultTTL $ ReqTx alice (aValidTx 42)
        update bobEnv ledger s0 event `shouldBe` Error (InvalidEvent event s0)

      it "everyone does collect on last commit after collect com" $ do
        let aliceCommit = OnCommitTx alice (utxoRef 1)
            bobCommit = OnCommitTx bob (utxoRef 2)
            carolCommit = OnCommitTx carol (utxoRef 3)
        waitingForLastCommit <-
          runEvents bobEnv ledger (inInitialState threeParties) $ do
            step (observeEventAtSlot 1 aliceCommit)
            step (observeEventAtSlot 2 bobCommit)

        -- Bob is not the last party, but still does post a collect
        update bobEnv ledger waitingForLastCommit (observeEventAtSlot 3 carolCommit)
          `hasEffectSatisfying` \case
            OnChainEffect{postChainTx = CollectComTx{}} -> True
            _ -> False

      it "cannot observe abort after collect com" $ do
        afterCollectCom <-
          runEvents bobEnv ledger (inInitialState threeParties) $
            step (observationEvent OnCollectComTx)

        let invalidEvent = observationEvent OnAbortTx
        update bobEnv ledger afterCollectCom invalidEvent
          `shouldBe` Error (InvalidEvent invalidEvent afterCollectCom)

      it "cannot observe collect com after abort" $ do
        afterAbort <-
          runEvents  bobEnv ledger (inInitialState threeParties) $
            step (observationEvent OnAbortTx)

        let invalidEvent = observationEvent OnCollectComTx
        update bobEnv ledger afterAbort invalidEvent
          `shouldBe` Error (InvalidEvent invalidEvent afterAbort)

      it "notifies user on head closing and when passing the contestation deadline" $ do
        let s0 = inOpenState threeParties ledger
            snapshotNumber = 0
            contestationDeadline = arbitrary `generateWith` 42
            observeCloseTx =
              observationEvent
                OnCloseTx
                  { headId = testHeadId
                  , snapshotNumber
                  , contestationDeadline
                  }
            clientEffect = ClientEffect HeadIsClosed{headId = testHeadId, snapshotNumber, contestationDeadline}
        let outcome1 = update bobEnv ledger s0 observeCloseTx
        outcome1 `hasEffect` clientEffect
        outcome1 `hasNoEffectSatisfying` \case
          ClientEffect (ReadyToFanout _) -> True
          _ -> False
        s1 <- assertNewState outcome1
        let oneSecondsPastDeadline = addUTCTime 1 contestationDeadline
            someChainSlot = arbitrary `generateWith` 42
            stepTimePastDeadline = OnChainEvent $ Tick oneSecondsPastDeadline someChainSlot
            s2 = update bobEnv ledger s1 stepTimePastDeadline
        s2 `hasEffect` ClientEffect (ReadyToFanout testHeadId)

      it "contests when detecting close with old snapshot" $ do
        let snapshot = Snapshot 2 mempty []
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot (aggregate [])
            s0 =
              inOpenState' threeParties $
                CoordinatedHeadState
                  { seenUTxO = mempty
                  , seenTxs = mempty
                  , confirmedSnapshot = latestConfirmedSnapshot
                  , seenSnapshot = NoSeenSnapshot
                  }
            deadline = arbitrary `generateWith` 42
            closeTxEvent = observationEvent $ OnCloseTx testHeadId 0 deadline
            contestTxEffect = chainEffect $ ContestTx latestConfirmedSnapshot
            s1 = update bobEnv ledger s0 closeTxEvent
        s1 `hasEffect` contestTxEffect
        s1 `shouldSatisfy` \case
          NewState (Closed ClosedState{}) _ -> True
          _ -> False

      it "re-contests when detecting contest with old snapshot" $ do
        let snapshot2 = Snapshot 2 mempty []
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot2 (aggregate [])
            s0 = inClosedState' threeParties latestConfirmedSnapshot
            contestSnapshot1Event = observationEvent $ OnContestTx 1
            contestTxEffect = chainEffect $ ContestTx latestConfirmedSnapshot
            s1 = update bobEnv ledger s0 contestSnapshot1Event
        s1 `hasEffect` contestTxEffect
        assertOnlyEffects s1

      it "ignores closeTx for another head" $ do
        let otherHeadId = HeadId "other head"
        let openState = inOpenState threeParties ledger
        let closeOtherHead =
              observationEvent $
                OnCloseTx
                  { headId = otherHeadId
                  , snapshotNumber = 1
                  , contestationDeadline = generateWith arbitrary 42
                  }

        update bobEnv ledger openState closeOtherHead
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
                  { parameters = HeadParameters cperiod threeParties
                  , coordinatedHeadState =
                      CoordinatedHeadState
                        { seenUTxO = UTxO.singleton utxo
                        , seenTxs = [expiringTransaction]
                        , confirmedSnapshot = InitialSnapshot $ UTxO.singleton utxo
                        , seenSnapshot = NoSeenSnapshot
                        }
                  , chainState = Prelude.error "should not be used"
                  , headId = testHeadId
                  , currentSlot = ChainSlot . fromIntegral . unSlotNo $ slotNo + 1
                  }

        st <-
          run $
            runEvents bobEnv ledger st0 $
              step (NetworkEvent defaultTTL $ ReqSn alice 1 [])

        assert $ case st of
          Open
            OpenState
              { coordinatedHeadState =
                CoordinatedHeadState{seenTxs}
              } -> null seenTxs
          _ -> False

runEvents :: Monad m => Environment -> Ledger tx -> HeadState tx -> StateT (StepState tx) m a -> m a
runEvents env ledger headState = (`evalStateT` StepState{env, ledger, headState})

--
-- Assertion utilities
--

-- | Create a chain effect with fixed chain state and slot.
chainEffect :: PostChainTx SimpleTx -> Effect SimpleTx
chainEffect postChainTx =
  OnChainEffect
    { postChainTx
    }

observeEventAtSlot :: Natural -> OnChainTx SimpleTx -> Event SimpleTx
observeEventAtSlot slot observedTx =
  OnChainEvent
    { chainEvent =
        Observation
          { observedTx
          , newChainState = SimpleChainState{slot = ChainSlot slot}
          }
    }

-- | Create an observation event with fixed chain state and slot.
observationEvent :: OnChainTx SimpleTx -> Event SimpleTx
observationEvent observedTx =
  OnChainEvent
    { chainEvent =
        Observation
          { observedTx
          , newChainState = SimpleChainState{slot = ChainSlot 0}
          }
    }

hasEffect :: (HasCallStack, IsChainState tx) => Outcome tx -> Effect tx -> IO ()
hasEffect (NewState _ effects) effect
  | effect `elem` effects = pure ()
  | otherwise = failure $ "Missing effect " <> show effect <> " in produced effects: " <> show effects
hasEffect (OnlyEffects effects) effect
  | effect `elem` effects = pure ()
  | otherwise = failure $ "Missing effect " <> show effect <> " in produced effects: " <> show effects
hasEffect o _ = failure $ "Unexpected outcome: " <> show o

hasEffectSatisfying :: (HasCallStack, IsChainState tx) => Outcome tx -> (Effect tx -> Bool) -> IO ()
hasEffectSatisfying outcome match =
  case outcome of
    NewState _ effects
      | any match effects -> pure ()
    OnlyEffects effects
      | any match effects -> pure ()
    _ -> failure $ "No effect matching predicate in produced effects: " <> show outcome

hasNoEffectSatisfying :: (HasCallStack, IsChainState tx) => Outcome tx -> (Effect tx -> Bool) -> IO ()
hasNoEffectSatisfying (NewState _ effects) predicate
  | any predicate effects = failure $ "Found unwanted effect in: " <> show effects
hasNoEffectSatisfying _ _ = pure ()

inInitialState :: [Party] -> HeadState SimpleTx
inInitialState parties =
  Initial
    InitialState
      { parameters
      , pendingCommits = Set.fromList parties
      , committed = mempty
      , chainState = SimpleChainState{slot = ChainSlot 0}
      , headId = testHeadId
      }
 where
  parameters = HeadParameters cperiod parties

inOpenState ::
  [Party] ->
  Ledger SimpleTx ->
  HeadState SimpleTx
inOpenState parties Ledger{initUTxO} =
  inOpenState' parties $ CoordinatedHeadState u0 mempty snapshot0 NoSeenSnapshot
 where
  u0 = initUTxO
  snapshot0 = InitialSnapshot u0

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
      , currentSlot = chainSlot
      }
 where
  parameters = HeadParameters cperiod parties

  chainSlot = ChainSlot 0

inClosedState :: [Party] -> HeadState SimpleTx
inClosedState parties = inClosedState' parties snapshot0
 where
  u0 = initUTxO simpleLedger
  snapshot0 = InitialSnapshot u0

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
      }
 where
  parameters = HeadParameters cperiod parties

  contestationDeadline = arbitrary `generateWith` 42

getConfirmedSnapshot :: HeadState tx -> Maybe (Snapshot tx)
getConfirmedSnapshot = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}} ->
    Just (getSnapshot confirmedSnapshot)
  _ ->
    Nothing

-- | Asserts that the update function will update the state (return a NewState) for this Event
assertUpdateState :: (MonadState (HeadState tx) m, HasCallStack, IsChainState tx) => Environment -> Ledger tx -> Event tx -> m (HeadState tx)
assertUpdateState env ledger event = do
  st <- get
  st' <- assertNewState $ update env ledger st event
  put st'
  pure st'

data StepState tx = StepState
  { headState :: HeadState tx
  , env :: Environment
  , ledger :: Ledger tx
  }

-- | Asserts that the update function will update the state (return a NewState) for this Event
step :: (MonadState (StepState tx) m, HasCallStack, IsChainState tx) => Event tx -> m (HeadState tx)
step event = do
  StepState{ headState, env, ledger} <- get
  headState' <- assertNewState $ update env ledger headState event
  put StepState{ env, ledger, headState = headState' }
  pure headState'

assertNewState ::
  (HasCallStack, IsChainState tx, Applicative m) =>
  Outcome tx ->
  m (HeadState tx)
assertNewState = \case
  NewState st _ -> pure st
  OnlyEffects effects -> Hydra.Prelude.error $ "Unexpected 'OnlyEffects' outcome: " <> show effects
  Error e -> Hydra.Prelude.error $ "Unexpected 'Error' outcome: " <> show e
  Wait r -> Hydra.Prelude.error $ "Unexpected 'Wait' outcome with reason: " <> show r

assertOnlyEffects ::
  (HasCallStack, IsChainState tx) =>
  Outcome tx ->
  IO ()
assertOnlyEffects = \case
  NewState st _ -> failure $ "Unexpected 'NewState' outcome: " <> show st
  OnlyEffects _ -> pure ()
  Error e -> failure $ "Unexpected 'Error' outcome: " <> show e
  Wait r -> failure $ "Unexpected 'Wait' outcome with reason: " <> show r

testHeadId :: HeadId
testHeadId = HeadId "1234"
