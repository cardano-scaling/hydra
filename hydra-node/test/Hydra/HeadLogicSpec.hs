{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit tests of the the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.Set as Set
import Hydra.Chain (
  ChainEvent (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (ContestTx),
 )
import Hydra.Crypto (aggregate, generateSigningKey, sign)
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  LogicError (..),
  Outcome (..),
  SeenSnapshot (NoSeenSnapshot, SeenSnapshot),
  WaitReason (..),
  update,
 )
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger, utxoRef)
import Hydra.Network (Host (..))
import Hydra.Network.Message (Message (AckSn, Connected, ReqSn, ReqTx))
import Hydra.Party (Party (..))
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, cperiod)
import Test.QuickCheck (forAll)
import Test.QuickCheck.Monadic (monadicIO, run)

spec :: Spec
spec = do
  parallel $ do
    describe "Types" $ do
      roundtripAndGoldenSpecs (Proxy @(Event SimpleTx))
      roundtripAndGoldenSpecs (Proxy @(HeadState SimpleTx))

    describe "Coordinated Head Protocol" $ do
      let threeParties = [alice, bob, carol]
          ledger = simpleLedger
          bobEnv =
            Environment
              { party = bob
              , signingKey = bobSk
              , otherParties = [alice, carol]
              }

      it "waits if a requested tx is not (yet) applicable" $ do
        let reqTx = NetworkEvent $ ReqTx alice $ SimpleTx 2 inputs mempty
            inputs = utxoRef 1
            s0 = inOpenState threeParties ledger

        update bobEnv ledger s0 reqTx `shouldBe` Wait (WaitOnNotApplicableTx (ValidationError "cannot apply transaction"))

      it "confirms snapshot given it receives AckSn from all parties" $ do
        let s0 = inOpenState threeParties ledger
            reqSn = NetworkEvent $ ReqSn alice 1 []
            snapshot1 = Snapshot 1 mempty []
            ackFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot1) 1
        s1 <- assertNewState $ update bobEnv ledger s0 reqSn
        s2 <- assertNewState $ update bobEnv ledger s1 (ackFrom carolSk carol)
        s3 <- assertNewState $ update bobEnv ledger s2 (ackFrom aliceSk alice)

        getConfirmedSnapshot s3 `shouldBe` Just (Snapshot 0 mempty [])

        s4 <- assertNewState $ update bobEnv ledger s3 (ackFrom bobSk bob)
        getConfirmedSnapshot s4 `shouldBe` Just snapshot1

      it "does not confirm snapshot when given a non-matching signature produced from a different message" $ do
        let s0 = inOpenState threeParties ledger
            reqSn = NetworkEvent $ ReqSn alice 1 []
            snapshot = Snapshot 1 mempty []
            snapshot' = Snapshot 2 mempty []
            ackFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot) 1
            invalidAckFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot') 1
        s1 <- assertNewState $ update bobEnv ledger s0 reqSn
        s2 <- assertNewState $ update bobEnv ledger s1 (ackFrom carolSk carol)
        s3 <- assertNewState $ update bobEnv ledger s2 (ackFrom aliceSk alice)
        s4 <- assertNewState $ update bobEnv ledger s3 (invalidAckFrom bobSk bob)

        getConfirmedSnapshot s4 `shouldBe` getConfirmedSnapshot s3

      it "does not confirm snapshot when given a non-matching signature produced from a different key" $ do
        let s0 = inOpenState threeParties ledger
            reqSn = NetworkEvent $ ReqSn alice 1 []
            snapshot = Snapshot 1 mempty []
            ackFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot) 1
        s1 <- assertNewState $ update bobEnv ledger s0 reqSn
        s2 <- assertNewState $ update bobEnv ledger s1 (ackFrom carolSk carol)
        s3 <- assertNewState $ update bobEnv ledger s2 (ackFrom aliceSk alice)
        s4 <- assertNewState $ update bobEnv ledger s3 (ackFrom (generateSigningKey "foo") bob)

        getConfirmedSnapshot s4 `shouldBe` getConfirmedSnapshot s3

      it "waits if we receive a snapshot with not-yet-seen transactions" $ do
        let event = NetworkEvent $ ReqSn alice 1 [SimpleTx 1 (utxoRef 1) (utxoRef 2)]
        update bobEnv ledger (inOpenState threeParties ledger) event
          `shouldBe` Wait (WaitOnNotApplicableTx (ValidationError "cannot apply transaction"))

      it "waits if we receive an AckSn for an unseen snapshot" $ do
        let snapshot = Snapshot 1 mempty []
            event = NetworkEvent $ AckSn alice (sign aliceSk snapshot) 1
        update bobEnv ledger (inOpenState threeParties ledger) event `shouldBe` Wait WaitOnSeenSnapshot

      -- TODO: write a property test for various future snapshots
      it "waits if we receive a future snapshot" $ do
        let event = NetworkEvent $ ReqSn bob 2 []
            st = inOpenState threeParties ledger
        update bobEnv ledger st event `shouldBe` Wait WaitOnSeenSnapshot

      it "waits if we receive a future snapshot while collecting signatures" $ do
        let s0 = inOpenState threeParties ledger
            reqSn1 = NetworkEvent $ ReqSn alice 1 []
            reqSn2 = NetworkEvent $ ReqSn bob 2 []
        s1 <- assertNewState $ update bobEnv ledger s0 reqSn1
        update bobEnv ledger s1 reqSn2 `shouldBe` Wait (WaitOnSnapshotNumber 1)

      it "acks signed snapshot from the constant leader" $ do
        let leader = alice
            snapshot = Snapshot 1 mempty []
            event = NetworkEvent $ ReqSn leader (number snapshot) []
            sig = sign bobSk snapshot
            st = inOpenState threeParties ledger
            ack = AckSn bob sig (number snapshot)
        update bobEnv ledger st event `hasEffect` NetworkEffect ack

      it "does not ack snapshots from non-leaders" $ do
        let event = NetworkEvent $ ReqSn notTheLeader 1 []
            notTheLeader = bob
            st = inOpenState threeParties ledger
        update bobEnv ledger st event `shouldBe` Error (InvalidEvent event st)

      -- TODO(SN): maybe this and the next are a property! at least DRY
      -- NOTE(AB): we should cover variations of snapshot numbers and state of snapshot
      -- collection
      it "rejects too-old snapshots" $ do
        let event = NetworkEvent $ ReqSn theLeader 2 []
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
        update bobEnv ledger st event `shouldBe` Error (InvalidEvent event st)

      it "rejects too-old snapshots when collecting signatures" $ do
        let event = NetworkEvent $ ReqSn theLeader 2 []
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
        update bobEnv ledger st event `shouldBe` Error (InvalidEvent event st)

      it "wait given too new snapshots from the leader" $ do
        let event = NetworkEvent $ ReqSn theLeader 3 []
            theLeader = carol
            st = inOpenState threeParties ledger
        update bobEnv ledger st event `shouldBe` Wait WaitOnSeenSnapshot

      it "rejects overlapping snapshot requests from the leader" $ do
        let s0 = inOpenState threeParties ledger
            theLeader = alice
            nextSN = 1
            firstReqSn = NetworkEvent $ ReqSn theLeader nextSN [aValidTx 42]
            secondReqSn = NetworkEvent $ ReqSn theLeader nextSN [aValidTx 51]

        s1 <- assertNewState $ update bobEnv ledger s0 firstReqSn
        update bobEnv ledger s1 secondReqSn `shouldBe` Error (InvalidEvent secondReqSn s1)

      it "ignores in-flight ReqTx when closed" $ do
        let s0 = inClosedState threeParties
            event = NetworkEvent $ ReqTx alice (aValidTx 42)
        update bobEnv ledger s0 event `shouldBe` Error (InvalidEvent event s0)

      it "notifies client when it receives a ping" $ do
        let peer = Host{hostname = "1.2.3.4", port = 1}
        update bobEnv ledger (inOpenState threeParties ledger) (NetworkEvent $ Connected peer)
          `hasEffect` ClientEffect (PeerConnected peer)

      it "cannot observe abort after collect com" $ do
        let s0 = inInitialState threeParties
        s1 <- assertNewState $ update bobEnv ledger s0 (OnChainEvent $ Observation OnCollectComTx)
        let invalidEvent = OnChainEvent $ Observation OnAbortTx
        let s2 = update bobEnv ledger s1 invalidEvent
        s2 `shouldBe` Error (InvalidEvent invalidEvent s1)

      it "cannot observe collect com after abort" $ do
        let s0 = inInitialState threeParties
        s1 <- assertNewState $ update bobEnv ledger s0 (OnChainEvent $ Observation OnAbortTx)
        let invalidEvent = OnChainEvent $ Observation OnCollectComTx
        let s2 = update bobEnv ledger s1 invalidEvent
        s2 `shouldBe` Error (InvalidEvent invalidEvent s1)

      it "notify user on head closing and when passing the contestation deadline" $ do
        let s0 = inOpenState threeParties ledger
            snapshotNumber = 0
            contestationDeadline = arbitrary `generateWith` 42
            observeCloseTx =
              OnChainEvent $
                Observation
                  OnCloseTx
                    { snapshotNumber
                    , contestationDeadline
                    }
            clientEffect = ClientEffect HeadIsClosed{snapshotNumber, contestationDeadline}
        let outcome1 = update bobEnv ledger s0 observeCloseTx
        outcome1 `hasEffect` clientEffect
        outcome1 `hasNoEffectSatisfying` \case
          ClientEffect ReadyToFanout -> True
          _ -> False
        s1 <- assertNewState outcome1
        let oneSecondsPastDeadline = addUTCTime 1 contestationDeadline
            stepTimePastDeadline = OnChainEvent $ Tick oneSecondsPastDeadline
            s2 = update bobEnv ledger s1 stepTimePastDeadline
        s2 `hasEffect` ClientEffect ReadyToFanout

      it "notify user on rollback" $
        forAll arbitrary $ \s -> monadicIO $ do
          let rollback = OnChainEvent (Rollback 2)
          let s' = update bobEnv ledger s rollback
          void $ run $ s' `hasEffect` ClientEffect RolledBack

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
            closeTxEvent = OnChainEvent $ Observation $ OnCloseTx 0 deadline
            contestTxEffect = OnChainEffect $ ContestTx latestConfirmedSnapshot
            s1 = update bobEnv ledger s0 closeTxEvent
        s1 `hasEffect` contestTxEffect
        s1 `shouldSatisfy` \case
          NewState ClosedState{} _ -> True
          _ -> False

      it "re-contests when detecting contest with old snapshot" $ do
        let snapshot2 = Snapshot 2 mempty []
            latestConfirmedSnapshot = ConfirmedSnapshot snapshot2 (aggregate [])
            s0 = inClosedState' threeParties latestConfirmedSnapshot
            contestSnapshot1Event = OnChainEvent $ Observation $ OnContestTx 1
            contestTxEffect = OnChainEffect $ ContestTx latestConfirmedSnapshot
            s1 = update bobEnv ledger s0 contestSnapshot1Event
        s1 `hasEffect` contestTxEffect
        assertOnlyEffects s1

--
-- Assertion utilities
--

hasEffect :: (HasCallStack, IsTx tx) => Outcome tx -> Effect tx -> IO ()
hasEffect (NewState _ effects) effect
  | effect `elem` effects = pure ()
  | otherwise = failure $ "Missing effect " <> show effect <> " in produced effects: " <> show effects
hasEffect (OnlyEffects effects) effect
  | effect `elem` effects = pure ()
  | otherwise = failure $ "Missing effect " <> show effect <> " in produced effects: " <> show effects
hasEffect o _ = failure $ "Unexpected outcome: " <> show o

hasEffectSatisfying :: (HasCallStack, IsTx tx) => Outcome tx -> (Effect tx -> Bool) -> IO (HeadState tx)
hasEffectSatisfying (NewState s effects) match
  | any match effects = pure s
  | otherwise = failure $ "No effect matching predicate in produced effects: " <> show effects
hasEffectSatisfying o _ = failure $ "Unexpected outcome: " <> show o

hasNoEffectSatisfying :: (HasCallStack, IsTx tx) => Outcome tx -> (Effect tx -> Bool) -> IO ()
hasNoEffectSatisfying (NewState _ effects) predicate
  | any predicate effects = failure $ "Found unwanted effect in: " <> show effects
hasNoEffectSatisfying _ _ = pure ()

isReqSn :: Effect tx -> Bool
isReqSn = \case
  NetworkEffect ReqSn{} -> True
  _ -> False

isAckSn :: Effect tx -> Bool
isAckSn = \case
  NetworkEffect AckSn{} -> True
  _ -> False

inInitialState :: [Party] -> HeadState SimpleTx
inInitialState parties =
  InitialState
    { parameters
    , pendingCommits = Set.fromList parties
    , committed = mempty
    , previousRecoverableState = IdleState
    }
 where
  parameters = HeadParameters cperiod parties

inOpenState ::
  [Party] ->
  Ledger tx ->
  HeadState tx
inOpenState parties Ledger{initUTxO} =
  inOpenState' parties $ CoordinatedHeadState u0 mempty snapshot0 NoSeenSnapshot
 where
  u0 = initUTxO
  snapshot0 = InitialSnapshot $ Snapshot 0 u0 mempty

inOpenState' ::
  [Party] ->
  CoordinatedHeadState tx ->
  HeadState tx
inOpenState' parties coordinatedHeadState =
  OpenState{parameters, coordinatedHeadState, previousRecoverableState}
 where
  parameters = HeadParameters cperiod parties
  previousRecoverableState =
    InitialState
      { parameters
      , pendingCommits = mempty
      , committed = mempty
      , previousRecoverableState = IdleState
      }

inClosedState :: [Party] -> HeadState SimpleTx
inClosedState parties = inClosedState' parties snapshot0
 where
  u0 = initUTxO simpleLedger
  snapshot0 = InitialSnapshot $ Snapshot 0 u0 mempty

inClosedState' :: [Party] -> ConfirmedSnapshot SimpleTx -> HeadState SimpleTx
inClosedState' parties confirmedSnapshot =
  ClosedState
    { parameters
    , previousRecoverableState
    , confirmedSnapshot
    , contestationDeadline
    , readyToFanoutSent = False
    }
 where
  parameters = HeadParameters cperiod parties
  previousRecoverableState = inOpenState parties simpleLedger
  contestationDeadline = arbitrary `generateWith` 42

getConfirmedSnapshot :: HeadState tx -> Maybe (Snapshot tx)
getConfirmedSnapshot = \case
  OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}} ->
    Just (getSnapshot confirmedSnapshot)
  _ ->
    Nothing

assertNewState :: IsTx tx => Outcome tx -> IO (HeadState tx)
assertNewState = \case
  NewState st _ -> pure st
  OnlyEffects effects -> failure $ "Unexpected 'OnlyEffects' outcome: " <> show effects
  Error e -> failure $ "Unexpected 'Error' outcome: " <> show e
  Wait r -> failure $ "Unexpected 'Wait' outcome with reason: " <> show r

assertOnlyEffects :: IsTx tx => Outcome tx -> IO ()
assertOnlyEffects = \case
  NewState st _ -> failure $ "Unexpected 'NewState' outcome: " <> show st
  OnlyEffects _ -> pure ()
  Error e -> failure $ "Unexpected 'Error' outcome: " <> show e
  Wait r -> failure $ "Unexpected 'Wait' outcome with reason: " <> show r

applyEvent ::
  IsTx tx =>
  (HeadState tx -> Event tx -> Outcome tx) ->
  Event tx ->
  StateT (HeadState tx) IO ()
applyEvent action e = do
  s <- get
  s' <- lift $ assertNewState (action s e)
  put s'

assertStateUnchangedFrom :: IsTx tx => HeadState tx -> Outcome tx -> Expectation
assertStateUnchangedFrom st = \case
  NewState st' eff -> do
    st' `shouldBe` st
    eff `shouldBe` []
  anything -> failure $ "unexpected outcome: " <> show anything
