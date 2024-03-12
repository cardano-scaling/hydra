{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Hydra.HeadLogicSnapshotSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Hydra.Chain (HeadParameters (..))
import Hydra.Crypto (sign)
import Hydra.Environment (Environment (..))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  HeadState (..),
  Input (NetworkInput),
  OpenState (OpenState),
  SeenSnapshot (..),
  coordinatedHeadState,
  defaultTTL,
  isLeader,
  update,
 )
import Hydra.HeadLogicSpec (getState, getStateAndEventId, hasEffect, hasEffectSatisfying, hasNoEffectSatisfying, inOpenState, inOpenState', runHeadLogic, step)
import Hydra.Ledger (txId)
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger, utxoRef)
import Hydra.Network.Message (Message (..))
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, deriveOnChainId, testHeadId)
import Test.QuickCheck (Property, counterexample, forAll, oneof, (==>))
import Test.QuickCheck.Monadic (monadicST, pick)

spec :: Spec
spec = do
  parallel $ do
    let threeParties = [alice, bob, carol]
        u0 = mempty
        envFor signingKey =
          let party = deriveParty signingKey
              otherParties = List.delete party threeParties
           in Environment
                { party
                , signingKey
                , otherParties
                , contestationPeriod = defaultContestationPeriod
                , participants = deriveOnChainId <$> threeParties
                }

    let coordinatedHeadState =
          CoordinatedHeadState
            { localUTxO = u0
            , allTxs = mempty
            , localTxs = mempty
            , confirmedSnapshot = InitialSnapshot testHeadId u0
            , seenSnapshot = NoSeenSnapshot
            }
    let initialEventID = 0
    let sendReqSn = \case
          NetworkEffect ReqSn{} -> True
          _ -> False
    let snapshot1 = Snapshot testHeadId 1 mempty []

    let ackFrom sk vk = NetworkInput defaultTTL vk $ AckSn (sign sk snapshot1) 1

    describe "Generic Snapshot property" $ do
      prop "there's always a leader for every snapshot number" prop_thereIsAlwaysALeader

    describe "On ReqTx" $ do
      prop "always emit ReqSn given head has 1 member" prop_singleMemberHeadAlwaysSnapshotOnReqTx

      it "sends ReqSn when leader and no snapshot in flight" $ do
        let tx = aValidTx 1
            outcome = update (envFor aliceSk) simpleLedger initialEventID (inOpenState' [alice, bob] coordinatedHeadState) $ NetworkInput defaultTTL alice $ ReqTx tx

        outcome
          `hasEffect` NetworkEffect (ReqSn 1 [Hydra.Ledger.txId tx])

      it "does NOT send ReqSn when we are NOT the leader even if no snapshot in flight" $ do
        let tx = aValidTx 1
            st = coordinatedHeadState{localTxs = [tx]}
            stEventID = initialEventID
            outcome = update (envFor bobSk) simpleLedger stEventID (inOpenState' [alice, bob] st) $ NetworkInput defaultTTL bob $ ReqTx tx

        outcome `hasNoEffectSatisfying` sendReqSn

      it "does NOT send ReqSn when we are the leader but snapshot in flight" $ do
        let tx = aValidTx 1
            sn1 = Snapshot testHeadId 1 u0 mempty :: Snapshot SimpleTx
            st = coordinatedHeadState{seenSnapshot = SeenSnapshot sn1 mempty}
            stEventID = initialEventID
            outcome = update (envFor aliceSk) simpleLedger stEventID (inOpenState' [alice, bob] st) $ NetworkInput defaultTTL alice $ ReqTx tx

        outcome `hasNoEffectSatisfying` sendReqSn

      it "updates seenSnapshot state when sending ReqSn" $ do
        let tx = aValidTx 1
            st = inOpenState' threeParties coordinatedHeadState
            stEventID = initialEventID

        let st' =
              inOpenState' threeParties $
                coordinatedHeadState
                  { localTxs = [tx]
                  , allTxs = Map.singleton (Hydra.Ledger.txId tx) tx
                  , localUTxO = u0 <> utxoRef (Hydra.Ledger.txId tx)
                  , seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}
                  }
            st'EventID = succ stEventID

        (actualState, actualEventID) <- runHeadLogic (envFor aliceSk) simpleLedger st stEventID $ do
          step $ NetworkInput defaultTTL alice $ ReqTx tx
          getStateAndEventId
        actualState `shouldBe` st'
        actualEventID `shouldBe` st'EventID

    describe "On AckSn" $ do
      let bobEnv = envFor bobSk

      it "sends ReqSn  when leader and there are seen transactions" $ do
        (headState, headStateEventID) <- runHeadLogic bobEnv simpleLedger (inOpenState threeParties) initialEventID $ do
          step (NetworkInput defaultTTL alice $ ReqSn 1 [])
          step (NetworkInput defaultTTL carol $ ReqTx $ aValidTx 1)
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getStateAndEventId

        update bobEnv simpleLedger headStateEventID headState (ackFrom bobSk bob)
          `hasEffectSatisfying` sendReqSn

      it "does NOT send ReqSn when we are the leader but there are NO seen transactions" $ do
        (headState, headStateEventID) <- runHeadLogic bobEnv simpleLedger (inOpenState threeParties) initialEventID $ do
          step (NetworkInput defaultTTL alice $ ReqSn 1 [])
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getStateAndEventId

        update bobEnv simpleLedger headStateEventID headState (ackFrom bobSk bob)
          `hasNoEffectSatisfying` sendReqSn

      it "does NOT send ReqSn when we are NOT the leader but there are seen transactions" $ do
        let
          notLeaderEnv = envFor carolSk

        let initiateSigningASnapshot actor =
              step (NetworkInput defaultTTL actor $ ReqSn 1 [])
            newTxBeforeSnapshotAcknowledged =
              step (NetworkInput defaultTTL carol $ ReqTx $ aValidTx 1)

        (headState, headStateEventID) <- runHeadLogic notLeaderEnv simpleLedger (inOpenState threeParties) initialEventID $ do
          initiateSigningASnapshot alice
          step (ackFrom carolSk carol)
          newTxBeforeSnapshotAcknowledged
          step (ackFrom aliceSk alice)
          getStateAndEventId

        let everybodyAcknowleged = update notLeaderEnv simpleLedger headStateEventID headState $ ackFrom bobSk bob
        everybodyAcknowleged `hasNoEffectSatisfying` sendReqSn

      it "updates seenSnapshot state when sending ReqSn" $ do
        headState <- runHeadLogic bobEnv simpleLedger (inOpenState threeParties) initialEventID $ do
          step (NetworkInput defaultTTL alice $ ReqSn 1 [])
          step (NetworkInput defaultTTL carol $ ReqTx $ aValidTx 1)
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          step (ackFrom bobSk bob)
          getState

        case headState of
          Open OpenState{coordinatedHeadState = CoordinatedHeadState{seenSnapshot = actualSnapshot}} ->
            actualSnapshot `shouldBe` RequestedSnapshot{lastSeen = 1, requested = 2}
          other -> expectationFailure $ "Expected to be in open state: " <> show other

prop_singleMemberHeadAlwaysSnapshotOnReqTx :: ConfirmedSnapshot SimpleTx -> Property
prop_singleMemberHeadAlwaysSnapshotOnReqTx sn = monadicST $ do
  seenSnapshot <-
    pick $
      oneof
        [ pure NoSeenSnapshot
        , LastSeenSnapshot <$> arbitrary
        ]
  tx <- pick $ aValidTx <$> arbitrary
  let
    aliceEnv =
      let party = alice
       in Environment
            { party
            , signingKey = aliceSk
            , otherParties = []
            , contestationPeriod = defaultContestationPeriod
            , participants = [deriveOnChainId party]
            }
    st =
      CoordinatedHeadState
        { localUTxO = mempty
        , allTxs = mempty
        , localTxs = []
        , confirmedSnapshot = sn
        , seenSnapshot
        }
    stEventID = 0
    outcome = update aliceEnv simpleLedger stEventID (inOpenState' [alice] st) $ NetworkInput defaultTTL alice $ ReqTx tx
    Snapshot{number = confirmedSn} = getSnapshot sn
    nextSn = confirmedSn + 1
  pure $
    outcome `hasEffect` NetworkEffect (ReqSn nextSn [Hydra.Ledger.txId tx])
      & counterexample (show outcome)

prop_thereIsAlwaysALeader :: Property
prop_thereIsAlwaysALeader =
  forAll arbitrary $ \sn ->
    forAll arbitrary $ \params@HeadParameters{parties} ->
      not (null parties) ==>
        any (\p -> isLeader params p sn) parties
