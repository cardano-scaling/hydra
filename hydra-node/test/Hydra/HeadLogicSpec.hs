{-# LANGUAGE TypeApplications #-}

-- | Unit tests of the the protocol logic in 'HeadLogic'. These are very fine
-- grained and specific to individual steps in the protocol. More high-level of
-- the protocol logic, especially between multiple parties can be found in
-- 'Hydra.BehaviorSpec'.
module Hydra.HeadLogicSpec where

import Hydra.Prelude

import qualified Data.List as List
import qualified Data.Set as Set
import Hydra.HeadLogic (
  ClientResponse (PeerConnected),
  CoordinatedHeadState (..),
  Effect (ClientEffect, NetworkEffect),
  Environment (..),
  Event (..),
  HeadParameters (..),
  HeadState (..),
  HeadStatus (..),
  HydraMessage (..),
  LogicError (..),
  OnChainTx (..),
  Outcome (..),
  Snapshot (..),
  SnapshotStrategy (..),
  update,
 )
import Hydra.Ledger (Ledger (..), Party, Tx, deriveParty, generateKey, sign)
import Hydra.Ledger.Builder (aValidTx, utxoRef)
import Hydra.Ledger.Simple (SimpleTx (..), TxIn (..), simpleLedger)
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  it,
  shouldBe,
 )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, elements, forAll)
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Property (collect)
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
        s0 = initialState threeParties ledger

    update env ledger s0 reqTx `shouldBe` Wait

  it "requests snapshot when receives ReqTx given node is leader" $ do
    let reqTx = NetworkEvent $ ReqTx 1 simpleTx
        leader = 1
        leaderEnv = envFor leader
        s0 = initialState threeParties ledger

    update leaderEnv ledger s0 reqTx
      `hasEffect_` NetworkEffect (ReqSn (party leaderEnv) 1 [simpleTx])

  it "does not request snapshots as non-leader" $ do
    let reqTx = NetworkEvent $ ReqTx 1 simpleTx
        nonLeaderEnv = envFor 2
        s0 = initialState threeParties ledger

    update nonLeaderEnv ledger s0 reqTx
      `hasNoEffectSatisfying` isReqSn

  it "does not request snapshot when already having one in flight" $ do
    let leaderEnv = envFor 1
        p = party leaderEnv
        s0 = initialState threeParties ledger
        firstReqSn = ReqSn p 1 [aValidTx 1]
    -- In the first processing loop, all ReqTx will lead to a ReqSn
    s1 <-
      update leaderEnv ledger s0 (NetworkEvent $ ReqTx p (aValidTx 1))
        `hasEffect` NetworkEffect firstReqSn
    s2 <-
      update leaderEnv ledger s1 (NetworkEvent $ ReqTx p (aValidTx 2))
        `hasEffect` NetworkEffect (ReqSn p 1 [aValidTx 2, aValidTx 1])
    -- Eventually, the leader's own ReqSn will be processed, resulting in an
    -- AckSn and no further ReqTx should result in a ReqSn
    s3 <-
      update leaderEnv ledger s2 (NetworkEvent firstReqSn)
        `hasEffectSatisfying` isAckSn
    update leaderEnv ledger s3 (NetworkEvent $ ReqTx p (aValidTx 3))
      `hasNoEffectSatisfying` isReqSn

  it "confirms snapshot given it receives AckSn from all parties" $ do
    let s0 = initialState threeParties ledger
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
    let s0 = initialState threeParties ledger
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
    let s0 = initialState threeParties ledger
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
    update env ledger (initialState threeParties ledger) event `shouldBe` Wait

  it "returns logic error if we receive a far-away snapshot (not the direct successor)" $ do
    let event = NetworkEvent $ ReqSn 1 2 []
        st = initialState threeParties ledger
    update env ledger st event `shouldBe` Error (InvalidEvent event st)

  it "acks signed snapshot from the constant leader" $ do
    let leader = 1
        snapshot = Snapshot 1 mempty []
        event = NetworkEvent $ ReqSn leader (number snapshot) []
        sig = sign 2 snapshot
        st = initialState threeParties ledger
        ack = AckSn (party env) sig (number snapshot)
    update env ledger st event `hasEffect_` NetworkEffect ack

  it "does not ack snapshots from non-leaders" $ do
    let event = NetworkEvent $ ReqSn notTheLeader 1 []
        notTheLeader = 2
        st = initialState threeParties ledger
    update env ledger st event `shouldBe` Error (InvalidEvent event st)

  it "does not ack too new snapshots" $ do
    let event = NetworkEvent $ ReqSn theLeader 3 []
        theLeader = 1
        st = initialState threeParties ledger
    update env ledger st event `shouldBe` Error (InvalidEvent event st)

  it "rejects overlapping snapshot requests from the leader" $ do
    let s0 = initialState threeParties ledger
        theLeader = 1
        nextSN = 1
        firstReqSn = NetworkEvent $ ReqSn theLeader nextSN [aValidTx 42]
        secondReqSn = NetworkEvent $ ReqSn theLeader nextSN [aValidTx 51]

    s1 <- assertNewState $ update env ledger s0 firstReqSn
    update env ledger s1 secondReqSn `shouldBe` Error (InvalidEvent secondReqSn s1)

  it "notifies client when it receives a ping" $ do
    update env ledger (initialState threeParties ledger) (NetworkEvent $ Connected 1)
      `hasEffect_` ClientEffect (PeerConnected 1)

  prop "can handle OnChainEvent in any state" prop_handleOnChainEventInAnyState

genOnChainTx :: Gen (OnChainTx SimpleTx)
genOnChainTx =
  elements
    [ InitTx mempty
    , CommitTx 1 (Set.fromList [TxIn 1, TxIn 2])
    , CollectComTx mempty
    , CloseTx (Snapshot 0 mempty mempty)
    , ContestTx (Snapshot 0 mempty mempty)
    , FanoutTx (Set.fromList [TxIn 1, TxIn 2])
    ]

genHeadStatus :: Gen (HeadStatus SimpleTx)
genHeadStatus =
  elements
    [ InitState
    , FinalState
    , CollectingState mempty mempty
    , OpenState (CoordinatedHeadState mempty mempty (Snapshot 0 mempty mempty) Nothing)
    ]

prop_handleOnChainEventInAnyState :: Property
prop_handleOnChainEventInAnyState =
  forAll genHeadStatus $ \st ->
    forAll genOnChainTx $ \tx ->
      collect (tx, st) $
        case update env ledger (HeadState defaultHeadParameters st) (OnChainEvent tx) of
          NewState _ _ -> True
          Wait -> True
          Error _ -> False
 where
  env =
    Environment
      { party = 1
      , signingKey = 1
      , otherParties = mempty
      , snapshotStrategy = NoSnapshots
      }

  ledger = simpleLedger

  defaultHeadParameters = HeadParameters 3600 [1]

-- ** Assertion utilities

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

isAckSn :: Effect tx -> Bool
isAckSn = \case
  NetworkEffect AckSn{} -> True
  _ -> False

initialState ::
  [Party] ->
  Ledger tx ->
  HeadState tx
initialState parties Ledger{initUTxO} =
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

getConfirmedSnapshot :: HeadState tx -> Maybe (Snapshot tx)
getConfirmedSnapshot HeadState{headStatus} = case headStatus of
  OpenState CoordinatedHeadState{confirmedSnapshot} -> Just confirmedSnapshot
  _ -> Nothing

assertNewState :: Tx tx => Outcome tx -> IO (HeadState tx)
assertNewState = \case
  NewState st _ -> pure st
  Error e -> fail (show e)
  Wait -> fail "Found 'Wait'"

assertStateUnchangedFrom :: Tx tx => HeadState tx -> Outcome tx -> Expectation
assertStateUnchangedFrom st = \case
  NewState st' eff -> do
    st' `shouldBe` st
    eff `shouldBe` []
  anything -> failure $ "unexpected outcome: " <> show anything
