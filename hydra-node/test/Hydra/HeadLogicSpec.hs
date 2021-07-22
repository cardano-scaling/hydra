{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Unit tests of the the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Set as Set
import Hydra.HeadLogic (
  ClientInput (..),
  CoordinatedHeadState (..),
  Effect (ClientEffect, Delay, NetworkEffect),
  Environment (..),
  Event (..),
  HeadParameters (..),
  HeadState (..),
  HeadStatus (..),
  HydraMessage (..),
  LogicError (..),
  OnChainTx (..),
  Outcome (..),
  ServerOutput (..),
  Snapshot (..),
  SnapshotStrategy (..),
  update,
 )
import Hydra.Ledger (Ledger (..), Party, Tx (..), deriveParty, generateKey, sign)
import Hydra.Ledger.Simple (SimpleTx (..), TxIn (..), aValidTx, simpleLedger, utxoRef)
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  it,
  shouldBe,
 )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Property,
  counterexample,
  forAllShrink,
  (===),
 )
import Test.Util (failure)

spec :: Spec
spec = describe "Hydra Coordinated Head Protocol" $ do
  let threeParties = [1, 2, 3]
      ledger = simpleLedger
      env =
        Environment
          { party = 2
          , signingKey = 2
          , otherParties = [1, 3]
          , snapshotStrategy = NoSnapshots
          }

      envFor n =
        let signingKey = generateKey n
            party = deriveParty signingKey
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

  it "returns logic error if we receive a far-away snapshot (not the direct successor)" $ do
    let event = NetworkEvent $ ReqSn 1 2 []
        st = inOpenState threeParties ledger
    update env ledger st event `shouldBe` Error (InvalidEvent event st)

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

  it "does not ack too new snapshots" $ do
    let event = NetworkEvent $ ReqSn theLeader 3 []
        theLeader = 1
        st = inOpenState threeParties ledger
    update env ledger st event `shouldBe` Error (InvalidEvent event st)

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
    update env ledger (inOpenState threeParties ledger) (NetworkEvent $ Connected 1)
      `hasEffect_` ClientEffect (PeerConnected 1)

  it "cannot observe abort after collect com" $ do
    let s0 = inInitialState threeParties
    s1 <- assertNewState $ update env ledger s0 (OnChainEvent $ CollectComTx mempty)
    let invalidEvent = OnChainEvent $ AbortTx mempty
    let s2 = update env ledger s1 invalidEvent
    s2 `shouldBe` Error (InvalidEvent invalidEvent s1)

  it "cannot observe collect com after abort" $ do
    let s0 = inInitialState threeParties
    s1 <- assertNewState $ update env ledger s0 (OnChainEvent $ AbortTx mempty)
    let invalidEvent = OnChainEvent $ CollectComTx mempty
    let s2 = update env ledger s1 invalidEvent
    s2 `shouldBe` Error (InvalidEvent invalidEvent s1)

  -- TOOD: Replace with: https://hackage.haskell.org/package/hspec-golden-aeson
  describe "JSON instances" $ do
    prop "ClientInput - JSON roundtrips" $
      prop_roundtripJSON (Proxy @(ClientInput SimpleTx))
    prop "ServerOutput - JSON roundtrips" $
      prop_roundtripJSON (Proxy @(ServerOutput SimpleTx))

prop_roundtripJSON ::
  forall a.
  (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a) =>
  Proxy a ->
  Property
prop_roundtripJSON _proxy = forAllShrink arbitrary shrink $ \(a :: a) ->
  let encoded = Aeson.encode a
   in counterexample (show encoded) $ Aeson.eitherDecode' encoded === Right a

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
  HeadState
    { headStatus = InitialState (Set.fromList parties) mempty
    , headParameters =
        HeadParameters
          { contestationPeriod = 42
          , parties
          }
    }

inOpenState ::
  [Party] ->
  Ledger tx ->
  HeadState tx
inOpenState parties Ledger{initUTxO} =
  let u0 = initUTxO
      snapshot0 = Snapshot 0 u0 mempty
   in HeadState
        { headStatus = OpenState $ CoordinatedHeadState u0 mempty snapshot0 Nothing
        , headParameters =
            HeadParameters
              { contestationPeriod = 42
              , parties
              }
        }

inClosedState :: [Party] -> HeadState SimpleTx
inClosedState parties =
  HeadState
    { headStatus = ClosedState mempty
    , headParameters =
        HeadParameters
          { contestationPeriod = 42
          , parties
          }
    }

getConfirmedSnapshot :: HeadState tx -> Maybe (Snapshot tx)
getConfirmedSnapshot HeadState{headStatus} = case headStatus of
  OpenState CoordinatedHeadState{confirmedSnapshot} -> Just confirmedSnapshot
  _ -> Nothing

assertNewState :: Tx tx => Outcome tx -> IO (HeadState tx)
assertNewState = \case
  NewState st _ -> pure st
  Error e -> failure $ "Unexpected 'Error' outcome: " <> (show e)
  Wait -> failure "Unexpected 'Wait' outcome"

assertStateUnchangedFrom :: Tx tx => HeadState tx -> Outcome tx -> Expectation
assertStateUnchangedFrom st = \case
  NewState st' eff -> do
    st' `shouldBe` st
    eff `shouldBe` []
  anything -> failure $ "unexpected outcome: " <> show anything
