{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Hydra.HeadLogicSnapshotSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Hydra.Chain (HeadParameters (..))
import Hydra.Crypto (sign)
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  Event (NetworkEvent),
  HeadState (..),
  OpenState (OpenState),
  SeenSnapshot (..),
  coordinatedHeadState,
  defaultTTL,
  isLeader,
  update,
 )
import Hydra.HeadLogic.Outcome (collectEffects)
import Hydra.HeadLogicSpec (getState, inOpenState, inOpenState', runEvents, step)
import Hydra.Ledger (Ledger (..), txId)
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
        Ledger{initUTxO} = simpleLedger
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
            { localUTxO = initUTxO
            , allTxs = mempty
            , localTxs = mempty
            , confirmedSnapshot = InitialSnapshot testHeadId initUTxO
            , seenSnapshot = NoSeenSnapshot
            }
    let sendReqSn =
          isJust
            . find
              ( \case
                  NetworkEffect ReqSn{} -> True
                  _ -> False
              )
    let snapshot1 = Snapshot testHeadId 1 mempty []

    let ackFrom sk vk = NetworkEvent defaultTTL vk $ AckSn (sign sk snapshot1) 1

    describe "Generic Snapshot property" $ do
      prop "there's always a leader for every snapshot number" prop_thereIsAlwaysALeader

    describe "On ReqTx" $ do
      prop "always emit ReqSn given head has 1 member" prop_singleMemberHeadAlwaysSnapshotOnReqTx

      it "sends ReqSn when leader and no snapshot in flight" $ do
        let tx = aValidTx 1
            outcome = update (envFor aliceSk) simpleLedger (inOpenState' [alice, bob] coordinatedHeadState) $ NetworkEvent defaultTTL alice $ ReqTx tx

        collectEffects outcome
          `shouldContain` [NetworkEffect (ReqSn 1 [txId tx])]

      it "does NOT send ReqSn when we are NOT the leader even if no snapshot in flight" $ do
        let tx = aValidTx 1
            st = coordinatedHeadState{localTxs = [tx]}
            outcome = update (envFor bobSk) simpleLedger (inOpenState' [alice, bob] st) $ NetworkEvent defaultTTL bob $ ReqTx tx

        collectEffects outcome `shouldNotSatisfy` sendReqSn

      it "does NOT send ReqSn when we are the leader but snapshot in flight" $ do
        let tx = aValidTx 1
            sn1 = Snapshot testHeadId 1 initUTxO mempty :: Snapshot SimpleTx
            st = coordinatedHeadState{seenSnapshot = SeenSnapshot sn1 mempty}
            outcome = update (envFor aliceSk) simpleLedger (inOpenState' [alice, bob] st) $ NetworkEvent defaultTTL alice $ ReqTx tx

        collectEffects outcome `shouldNotSatisfy` sendReqSn

      it "updates seenSnapshot state when sending ReqSn" $ do
        let tx = aValidTx 1
            st = inOpenState' threeParties coordinatedHeadState

        let st' =
              inOpenState' threeParties $
                coordinatedHeadState
                  { localTxs = [tx]
                  , allTxs = Map.singleton (txId tx) tx
                  , localUTxO = initUTxO <> utxoRef (txId tx)
                  , seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}
                  }

        actualState <- runEvents (envFor aliceSk) simpleLedger st $ do
          step $ NetworkEvent defaultTTL alice $ ReqTx tx
          getState
        actualState `shouldBe` st'

    describe "On AckSn" $ do
      let bobEnv = envFor bobSk

      it "sends ReqSn  when leader and there are seen transactions" $ do
        headState <- runEvents bobEnv simpleLedger (inOpenState threeParties simpleLedger) $ do
          step (NetworkEvent defaultTTL alice $ ReqSn 1 [])
          step (NetworkEvent defaultTTL carol $ ReqTx $ aValidTx 1)
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getState

        let outcome = update bobEnv simpleLedger headState $ ackFrom bobSk bob
        collectEffects outcome `shouldSatisfy` sendReqSn

      it "does NOT send ReqSn when we are the leader but there are NO seen transactions" $ do
        headState <- runEvents bobEnv simpleLedger (inOpenState threeParties simpleLedger) $ do
          step (NetworkEvent defaultTTL alice $ ReqSn 1 [])
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getState

        let outcome = update bobEnv simpleLedger headState $ ackFrom bobSk bob
        collectEffects outcome `shouldNotSatisfy` sendReqSn

      it "does NOT send ReqSn when we are NOT the leader but there are seen transactions" $ do
        let
          notLeaderEnv = envFor carolSk

        let initiateSigningASnapshot actor =
              step (NetworkEvent defaultTTL actor $ ReqSn 1 [])
            newTxBeforeSnapshotAcknowledged =
              step (NetworkEvent defaultTTL carol $ ReqTx $ aValidTx 1)

        headState <- runEvents notLeaderEnv simpleLedger (inOpenState threeParties simpleLedger) $ do
          initiateSigningASnapshot alice
          step (ackFrom carolSk carol)
          newTxBeforeSnapshotAcknowledged
          step (ackFrom aliceSk alice)
          getState

        let everybodyAcknowleged = update notLeaderEnv simpleLedger headState $ ackFrom bobSk bob
        collectEffects everybodyAcknowleged `shouldNotSatisfy` sendReqSn

      it "updates seenSnapshot state when sending ReqSn" $ do
        headState <- runEvents bobEnv simpleLedger (inOpenState threeParties simpleLedger) $ do
          step (NetworkEvent defaultTTL alice $ ReqSn 1 [])
          step (NetworkEvent defaultTTL carol $ ReqTx $ aValidTx 1)
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
    outcome = update aliceEnv simpleLedger (inOpenState' [alice] st) $ NetworkEvent defaultTTL alice $ ReqTx tx
    Snapshot{number = confirmedSn} = getSnapshot sn
    nextSn = confirmedSn + 1
  pure $
    ( collectEffects outcome
        `shouldContain` [NetworkEffect (ReqSn nextSn [txId tx])]
    )
      & counterexample (show outcome)

prop_thereIsAlwaysALeader :: Property
prop_thereIsAlwaysALeader =
  forAll arbitrary $ \sn ->
    forAll arbitrary $ \params@HeadParameters{parties} ->
      not (null parties) ==>
        any (\p -> isLeader params p sn) parties
