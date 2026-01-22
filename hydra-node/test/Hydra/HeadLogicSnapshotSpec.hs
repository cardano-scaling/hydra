{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Hydra.HeadLogicSnapshotSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Hydra.HeadLogic (CoordinatedHeadState (..), Effect (..), HeadState (..), OpenState (OpenState), Outcome, SeenSnapshot (..), coordinatedHeadState, isLeader, update)
import Hydra.HeadLogicSpec (StepState, getState, hasEffect, hasEffectSatisfying, hasNoEffectSatisfying, inOpenState, inOpenState', nowFromSlot, receiveMessage, receiveMessageFrom, runHeadLogic, step)
import Hydra.Ledger.Simple (SimpleChainPoint (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (NodeState (currentChainPoint, headState))
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod, defaultUnsyncedPeriod)
import Hydra.Tx.Crypto (sign)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (txId)
import Hydra.Tx.Party (Party, deriveParty)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Hydra.Tx.Fixture (
  alice,
  aliceSk,
  bob,
  bobSk,
  carol,
  carolSk,
  deriveOnChainId,
  testHeadId,
 )
import Test.QuickCheck (Property, counterexample, forAll, oneof, (==>))
import Test.QuickCheck.Monadic (monadicIO, pick, run)

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
                , depositPeriod = defaultDepositPeriod
                , unsyncedPeriod = defaultUnsyncedPeriod
                , participants = deriveOnChainId <$> threeParties
                , configuredPeers = ""
                }

    let coordinatedHeadState =
          CoordinatedHeadState
            { localUTxO = u0
            , allTxs = mempty
            , localTxs = mempty
            , confirmedSnapshot = InitialSnapshot testHeadId u0
            , seenSnapshot = NoSeenSnapshot
            , currentDepositTxId = Nothing
            , decommitTx = Nothing
            , version = 0
            }
    let sendReqSn :: Effect tx -> Bool
        sendReqSn = \case
          NetworkEffect ReqSn{} -> True
          _ -> False
    let snapshot1 = Snapshot testHeadId 0 1 [] mempty Nothing Nothing

    let ackFrom sk vk = receiveMessageFrom vk $ AckSn (sign sk snapshot1) 1

    describe "Generic Snapshot property" $ do
      prop "there's always a leader for every snapshot number" prop_thereIsAlwaysALeader

    describe "On ReqTx" $ do
      prop "always emit ReqSn given head has 1 member" prop_singleMemberHeadAlwaysSnapshotOnReqTx

      it "sends ReqSn when leader and no snapshot in flight" $ do
        let tx = aValidTx 1
            s0 = inOpenState' [alice, bob] coordinatedHeadState
        now <- nowFromSlot (slot $ currentChainPoint s0)
        let outcome = update (envFor aliceSk) simpleLedger now s0 $ receiveMessage $ ReqTx tx

        outcome
          `hasEffect` NetworkEffect (ReqSn 0 1 [txId tx] Nothing Nothing)

      it "does NOT send ReqSn when we are NOT the leader even if no snapshot in flight" $ do
        let tx = aValidTx 1
            st = coordinatedHeadState{localTxs = [tx]}
            s0 = inOpenState' [alice, bob] st
        now <- nowFromSlot s0.currentChainPoint.slot
        let outcome = update (envFor bobSk) simpleLedger now s0 $ receiveMessageFrom bob $ ReqTx tx

        outcome `hasNoEffectSatisfying` sendReqSn

      it "does NOT send ReqSn when we are the leader but snapshot in flight" $ do
        let tx = aValidTx 1
            sn1 = Snapshot testHeadId 1 1 [] u0 Nothing Nothing :: Snapshot SimpleTx
            st = coordinatedHeadState{seenSnapshot = SeenSnapshot sn1 mempty}
            s0 = inOpenState' [alice, bob] st
        now <- nowFromSlot s0.currentChainPoint.slot
        let outcome = update (envFor aliceSk) simpleLedger now s0 $ receiveMessage $ ReqTx tx

        outcome `hasNoEffectSatisfying` sendReqSn

      it "updates seenSnapshot state when sending ReqSn" $ do
        let tx = aValidTx 1
            st = inOpenState' threeParties coordinatedHeadState
            st' =
              inOpenState' threeParties $
                coordinatedHeadState
                  { localTxs = [tx]
                  , allTxs = Map.singleton (txId tx) tx
                  , localUTxO = u0 <> utxoRef (txId tx)
                  , seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}
                  }

        actualState <- runHeadLogic (envFor aliceSk) simpleLedger st $ do
          step $ receiveMessage $ ReqTx tx
          getState
        actualState `shouldBe` st'

    describe "On AckSn" $ do
      let bobEnv = envFor bobSk

      it "sends ReqSn  when leader and there are seen transactions" $ do
        headState <- runHeadLogic bobEnv simpleLedger (inOpenState threeParties) $ do
          step (receiveMessage $ ReqSn 0 1 [] Nothing Nothing)
          step (receiveMessageFrom carol $ ReqTx $ aValidTx 1)
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getState

        now <- nowFromSlot headState.currentChainPoint.slot
        update bobEnv simpleLedger now headState (ackFrom bobSk bob)
          `hasEffectSatisfying` sendReqSn

      it "does NOT send ReqSn when we are the leader but there are NO seen transactions" $ do
        headState <- runHeadLogic bobEnv simpleLedger (inOpenState threeParties) $ do
          step (receiveMessage $ ReqSn 0 1 [] Nothing Nothing)
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          getState

        now <- nowFromSlot headState.currentChainPoint.slot
        update bobEnv simpleLedger now headState (ackFrom bobSk bob)
          `hasNoEffectSatisfying` sendReqSn

      it "does NOT send ReqSn when we are NOT the leader but there are seen transactions" $ do
        let
          notLeaderEnv = envFor carolSk

        let initiateSigningASnapshot :: (MonadState (StepState SimpleTx) m, MonadTime m) => Party -> m (Outcome SimpleTx)
            initiateSigningASnapshot actor =
              step (receiveMessageFrom actor $ ReqSn 0 1 [] Nothing Nothing)
            newTxBeforeSnapshotAcknowledged =
              step (receiveMessageFrom carol $ ReqTx $ aValidTx 1)

        headState <- runHeadLogic notLeaderEnv simpleLedger (inOpenState threeParties) $ do
          initiateSigningASnapshot alice
          step (ackFrom carolSk carol)
          newTxBeforeSnapshotAcknowledged
          step (ackFrom aliceSk alice)
          getState

        now <- nowFromSlot headState.currentChainPoint.slot
        let everybodyAcknowledged = update notLeaderEnv simpleLedger now headState $ ackFrom bobSk bob
        everybodyAcknowledged `hasNoEffectSatisfying` sendReqSn

      it "updates seenSnapshot state when sending ReqSn" $ do
        nodeState <- runHeadLogic bobEnv simpleLedger (inOpenState threeParties) $ do
          step (receiveMessage $ ReqSn 0 1 [] Nothing Nothing)
          step (receiveMessageFrom carol $ ReqTx $ aValidTx 1)
          step (ackFrom carolSk carol)
          step (ackFrom aliceSk alice)
          step (ackFrom bobSk bob)
          getState

        case headState nodeState of
          Open OpenState{coordinatedHeadState = CoordinatedHeadState{seenSnapshot = actualSnapshot}} ->
            actualSnapshot `shouldBe` RequestedSnapshot{lastSeen = 1, requested = 2}
          other -> expectationFailure $ "Expected to be in open state: " <> show other

prop_singleMemberHeadAlwaysSnapshotOnReqTx :: ConfirmedSnapshot SimpleTx -> Property
prop_singleMemberHeadAlwaysSnapshotOnReqTx sn = monadicIO $ do
  (seenSnapshot, version) <-
    pick $
      oneof
        [ pure (NoSeenSnapshot, 0)
        , do
            n <- arbitrary
            let v = fromInteger (toInteger n)
            pure (LastSeenSnapshot n, v)
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
            , depositPeriod = defaultDepositPeriod
            , unsyncedPeriod = defaultUnsyncedPeriod
            , participants = [deriveOnChainId party]
            , configuredPeers = ""
            }
    st =
      CoordinatedHeadState
        { localUTxO = mempty
        , allTxs = mempty
        , localTxs = []
        , confirmedSnapshot = sn
        , seenSnapshot
        , currentDepositTxId = Nothing
        , decommitTx = Nothing
        , version
        }
    s0 = inOpenState' [alice] st
  now <- run $ nowFromSlot s0.currentChainPoint.slot
  let outcome = update aliceEnv simpleLedger now s0 $ receiveMessage $ ReqTx tx
      Snapshot{number = confirmedSn} = getSnapshot sn
      nextSn = confirmedSn + 1
  pure $
    outcome `hasEffect` NetworkEffect (ReqSn version nextSn [txId tx] Nothing Nothing)
      & counterexample (show outcome)

prop_thereIsAlwaysALeader :: Property
prop_thereIsAlwaysALeader =
  forAll arbitrary $ \sn ->
    forAll arbitrary $ \params@HeadParameters{parties} ->
      not (null parties) ==>
        any (\p -> isLeader params p sn) parties
