{-# LANGUAGE TypeApplications #-}

-- | Unit tests of the the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.List as List
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (HeadParameters), OnChainTx (OnAbortTx, OnCollectComTx))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (ClientEffect, Delay, NetworkEffect),
  Environment (..),
  Event (..),
  HeadState (..),
  LogicError (..),
  Outcome (..),
  SeenSnapshot (NoSeenSnapshot, SeenSnapshot),
  SnapshotStrategy (..),
  update,
 )
import Hydra.Ledger (Ledger (..), Party, Tx (..), deriveParty, sign)
import Hydra.Ledger.Simple (SimpleTx (..), TxIn (..), aValidTx, simpleLedger, utxoRef)
import Hydra.Network (Host (..))
import Hydra.Network.Message (Message (AckSn, Connected, ReqSn, ReqTx))
import Hydra.ServerOutput (ServerOutput (PeerConnected))
import Hydra.Snapshot (Snapshot (..))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)

spec :: Spec
spec = do
  describe "Event" $ do
    roundtripAndGoldenSpecs (Proxy @(Event SimpleTx))

  describe "Coordinated Head Protocol" $ do
    let threeParties = [1, 2, 3]
        ledger = simpleLedger
        env =
          Environment
            { party = 2
            , signingKey = 2
            , otherParties = [1, 3]
            , snapshotStrategy = NoSnapshots
            }

        envFor signingKey =
          let party = deriveParty signingKey
           in Environment
                { party
                , signingKey
                , otherParties = List.delete party threeParties
                , snapshotStrategy = SnapshotAfterEachTx
                }

        -- NOTE: This unrealistic Tx is just there to be always valid as
        -- it does not require any input
        simpleTx = SimpleTx 1 mempty (Set.fromList [TxIn 3, TxIn 4])

    it "waits if a requested tx is not (yet) applicable" $ do
      let reqTx = NetworkEvent $ ReqTx 1 $ SimpleTx 2 inputs mempty
          inputs = utxoRef 1
          s0 = inOpenState threeParties ledger

      update env ledger s0 reqTx `shouldBe` Wait

    it "requests new snapshot when receives ReqTx given node is leader" $ do
      let reqTx = NetworkEvent $ ReqTx 1 simpleTx
          leader = 1
          leaderEnv = envFor leader
          s0 = inOpenState threeParties ledger

      update leaderEnv ledger s0 reqTx
        `hasEffect_` Delay 0 DoSnapshot

    it "does not request new snapshot as non-leader" $ do
      let reqTx = NetworkEvent $ ReqTx 1 simpleTx
          nonLeaderEnv = envFor 2
          s0 = inOpenState threeParties ledger

      update nonLeaderEnv ledger s0 reqTx
        `hasNoEffectSatisfying` isDoSnapshot

    it "requests new snapshot as leader of a later snapshots" $ do
      let reqTx = NetworkEvent $ ReqTx 1 simpleTx
          bob = envFor 2
          -- Bob is the leader in a three party round for snapshot 5
          snapshot4 = Snapshot 4 mempty mempty
          s0 = inOpenState' threeParties $ CoordinatedHeadState mempty mempty snapshot4 NoSeenSnapshot

      update bob ledger s0 reqTx
        `hasEffect_` Delay 0 DoSnapshot

    it "delay snapshot request when already having one in flight" $ do
      let leaderEnv = envFor 1
          p = party leaderEnv
          s0 = inOpenState threeParties ledger
          firstReqSn = ReqSn p 1 [aValidTx 1]

      s1 <- assertNewState $ update leaderEnv ledger s0 (NetworkEvent firstReqSn)

      update leaderEnv ledger s1 DoSnapshot
        `shouldBe` Wait

    it "drop snapshot request when there's no seen transactions" $ do
      let leaderEnv = envFor 1
          s0 = inOpenState threeParties ledger

      update leaderEnv ledger s0 DoSnapshot
        `hasNoEffectSatisfying` isReqSn

    it "confirms snapshot given it receives AckSn from all parties" $ do
      let s0 = inOpenState threeParties ledger
          reqSn = NetworkEvent $ ReqSn 1 1 []
          snapshot1 = Snapshot 1 mempty []
          ackFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot1) 1
      s1 <- assertNewState $ update env ledger s0 reqSn
      s2 <- assertNewState $ update env ledger s1 (ackFrom 3 3)
      s3 <- assertNewState $ update env ledger s2 (ackFrom 1 1)

      getConfirmedSnapshot s3 `shouldBe` Just (Snapshot 0 mempty [])

      s4 <- assertNewState $ update env ledger s3 (ackFrom 2 2)
      getConfirmedSnapshot s4 `shouldBe` Just snapshot1

    it "does not confirm snapshot when given a non-matching signature produced from a different message" $ do
      let s0 = inOpenState threeParties ledger
          reqSn = NetworkEvent $ ReqSn 1 1 []
          snapshot = Snapshot 1 mempty []
          snapshot' = Snapshot 2 mempty []
          ackFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot) 1
          invalidAckFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot') 1
      s1 <- assertNewState $ update env ledger s0 reqSn
      s2 <- assertNewState $ update env ledger s1 (ackFrom 3 3)
      s3 <- assertNewState $ update env ledger s2 (ackFrom 1 1)
      s4 <- assertNewState $ update env ledger s3 (invalidAckFrom 2 2)

      getConfirmedSnapshot s4 `shouldBe` getConfirmedSnapshot s3

    it "does not confirm snapshot when given a non-matching signature produced from a different key" $ do
      let s0 = inOpenState threeParties ledger
          reqSn = NetworkEvent $ ReqSn 1 1 []
          snapshot = Snapshot 1 mempty []
          ackFrom sk vk = NetworkEvent $ AckSn vk (sign sk snapshot) 1
      s1 <- assertNewState $ update env ledger s0 reqSn
      s2 <- assertNewState $ update env ledger s1 (ackFrom 3 3)
      s3 <- assertNewState $ update env ledger s2 (ackFrom 1 1)
      s4 <- assertNewState $ update env ledger s3 (ackFrom 42 2)

      getConfirmedSnapshot s4 `shouldBe` getConfirmedSnapshot s3

    it "waits if we receive a snapshot with not-yet-seen transactions" $ do
      let event = NetworkEvent $ ReqSn 1 1 [SimpleTx 1 (utxoRef 1) (utxoRef 2)]
      update env ledger (inOpenState threeParties ledger) event `shouldBe` Wait

    it "waits if we receive an AckSn for an unseen snapshot" $ do
      let snapshot = Snapshot 1 mempty []
          event = NetworkEvent $ AckSn 1 (sign 1 snapshot) 1
      update env ledger (inOpenState threeParties ledger) event `shouldBe` Wait

    -- TODO: write a property test for various future snapshots
    it "waits if we receive a future snapshot" $ do
      let event = NetworkEvent $ ReqSn 2 2 []
          st = inOpenState threeParties ledger
      update env ledger st event `shouldBe` Wait

    it "waits if we receive a future snapshot while collecting signatures" $ do
      let s0 = inOpenState threeParties ledger
          reqSn1 = NetworkEvent $ ReqSn 1 1 []
          reqSn2 = NetworkEvent $ ReqSn 2 2 []
      s1 <- assertNewState $ update env ledger s0 reqSn1
      update env ledger s1 reqSn2 `shouldBe` Wait

    it "acks signed snapshot from the constant leader" $ do
      let leader = 1
          snapshot = Snapshot 1 mempty []
          event = NetworkEvent $ ReqSn leader (number snapshot) []
          sig = sign 2 snapshot
          st = inOpenState threeParties ledger
          ack = AckSn (party env) sig (number snapshot)
      update env ledger st event `hasEffect_` NetworkEffect ack

    it "does not ack snapshots from non-leaders" $ do
      let event = NetworkEvent $ ReqSn notTheLeader 1 []
          notTheLeader = 2
          st = inOpenState threeParties ledger
      update env ledger st event `shouldBe` Error (InvalidEvent event st)

    -- TODO(SN): maybe this and the next are a property! at least DRY
    -- NOTE(AB): we should cover variations of snapshot numbers and state of snapshot
    -- collection
    it "rejects too-old snapshots" $ do
      let event = NetworkEvent $ ReqSn theLeader 2 []
          theLeader = 1
          snapshot = Snapshot 2 mempty []
          st =
            inOpenState' threeParties $
              CoordinatedHeadState
                { seenUtxo = mempty
                , seenTxs = mempty
                , confirmedSnapshot = snapshot
                , seenSnapshot = NoSeenSnapshot
                }
      update env ledger st event `shouldBe` Error (InvalidEvent event st)

    it "rejects too-old snapshots when collecting signatures" $ do
      let event = NetworkEvent $ ReqSn theLeader 2 []
          theLeader = 1
          snapshot = Snapshot 2 mempty []
          st =
            inOpenState' threeParties $
              CoordinatedHeadState
                { seenUtxo = mempty
                , seenTxs = mempty
                , confirmedSnapshot = snapshot
                , seenSnapshot = SeenSnapshot (Snapshot 3 mempty []) mempty
                }
      update env ledger st event `shouldBe` Error (InvalidEvent event st)

    it "wait given too new snapshots from the leader" $ do
      let event = NetworkEvent $ ReqSn theLeader 3 []
          theLeader = 3
          st = inOpenState threeParties ledger
      update env ledger st event `shouldBe` Wait

    it "rejects overlapping snapshot requests from the leader" $ do
      let s0 = inOpenState threeParties ledger
          theLeader = 1
          nextSN = 1
          firstReqSn = NetworkEvent $ ReqSn theLeader nextSN [aValidTx 42]
          secondReqSn = NetworkEvent $ ReqSn theLeader nextSN [aValidTx 51]

      s1 <- assertNewState $ update env ledger s0 firstReqSn
      update env ledger s1 secondReqSn `shouldBe` Error (InvalidEvent secondReqSn s1)

    it "ignores in-flight ReqTx when closed" $ do
      let s0 = inClosedState threeParties
          event = NetworkEvent $ ReqTx 1 (aValidTx 42)
      update env ledger s0 event `shouldBe` Error (InvalidEvent event s0)

    it "notifies client when it receives a ping" $ do
      let peer = Host{hostname = "1.2.3.4", port = 1}
      update env ledger (inOpenState threeParties ledger) (NetworkEvent $ Connected peer)
        `hasEffect_` ClientEffect (PeerConnected peer)

    it "cannot observe abort after collect com" $ do
      let s0 = inInitialState threeParties
      s1 <- assertNewState $ update env ledger s0 (OnChainEvent OnCollectComTx)
      let invalidEvent = OnChainEvent OnAbortTx
      let s2 = update env ledger s1 invalidEvent
      s2 `shouldBe` Error (InvalidEvent invalidEvent s1)

    it "cannot observe collect com after abort" $ do
      let s0 = inInitialState threeParties
      s1 <- assertNewState $ update env ledger s0 (OnChainEvent OnAbortTx)
      let invalidEvent = OnChainEvent OnCollectComTx
      let s2 = update env ledger s1 invalidEvent
      s2 `shouldBe` Error (InvalidEvent invalidEvent s1)

--
-- Assertion utilities
--

hasEffect :: (HasCallStack, Tx tx) => Outcome tx -> Effect tx -> IO (HeadState tx)
hasEffect (NewState s effects) effect
  | effect `elem` effects = pure s
  | otherwise = failure $ "Missing effect " <> show effect <> " in produced effects: " <> show effects
hasEffect o _ = failure $ "Unexpected outcome: " <> show o

hasEffect_ :: (HasCallStack, Tx tx) => Outcome tx -> Effect tx -> IO ()
hasEffect_ o e = void $ hasEffect o e

hasEffectSatisfying :: (HasCallStack, Tx tx) => Outcome tx -> (Effect tx -> Bool) -> IO (HeadState tx)
hasEffectSatisfying (NewState s effects) match
  | any match effects = pure s
  | otherwise = failure $ "No effect matching predicate in produced effects: " <> show effects
hasEffectSatisfying o _ = failure $ "Unexpected outcome: " <> show o

hasNoEffectSatisfying :: (HasCallStack, Tx tx) => Outcome tx -> (Effect tx -> Bool) -> IO ()
hasNoEffectSatisfying (NewState _ effects) predicate
  | any predicate effects = failure $ "Found unwanted effect in: " <> show effects
hasNoEffectSatisfying _ _ = pure ()

isReqSn :: Effect tx -> Bool
isReqSn = \case
  NetworkEffect ReqSn{} -> True
  _ -> False

isDoSnapshot :: Effect tx -> Bool
isDoSnapshot = \case
  Delay _ DoSnapshot -> True
  _ -> False

isAckSn :: Effect tx -> Bool
isAckSn = \case
  NetworkEffect AckSn{} -> True
  _ -> False

inInitialState :: [Party] -> HeadState SimpleTx
inInitialState parties =
  InitialState parameters (Set.fromList parties) mempty
 where
  parameters = HeadParameters 42 parties

inOpenState ::
  [Party] ->
  Ledger tx ->
  HeadState tx
inOpenState parties Ledger{initUtxo} =
  inOpenState' parties $ CoordinatedHeadState u0 mempty snapshot0 NoSeenSnapshot
 where
  u0 = initUtxo
  snapshot0 = Snapshot 0 u0 mempty

inOpenState' ::
  [Party] ->
  CoordinatedHeadState tx ->
  HeadState tx
inOpenState' parties = OpenState parameters
 where
  parameters = HeadParameters 42 parties

inClosedState :: [Party] -> HeadState SimpleTx
inClosedState parties =
  ClosedState parameters mempty
 where
  parameters = HeadParameters 42 parties

getConfirmedSnapshot :: HeadState tx -> Maybe (Snapshot tx)
getConfirmedSnapshot = \case
  OpenState _ CoordinatedHeadState{confirmedSnapshot} -> Just confirmedSnapshot
  _ -> Nothing

assertNewState :: Tx tx => Outcome tx -> IO (HeadState tx)
assertNewState = \case
  NewState st _ -> pure st
  Error e -> failure $ "Unexpected 'Error' outcome: " <> show e
  Wait -> failure "Unexpected 'Wait' outcome"

applyEvent ::
  Tx tx =>
  (HeadState tx -> Event tx -> Outcome tx) ->
  Event tx ->
  StateT (HeadState tx) IO ()
applyEvent action e = do
  s <- get
  s' <- lift $ assertNewState (action s e)
  put s'

assertStateUnchangedFrom :: Tx tx => HeadState tx -> Outcome tx -> Expectation
assertStateUnchangedFrom st = \case
  NewState st' eff -> do
    st' `shouldBe` st
    eff `shouldBe` []
  anything -> failure $ "unexpected outcome: " <> show anything
