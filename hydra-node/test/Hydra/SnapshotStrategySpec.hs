{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Hydra.SnapshotStrategySpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Data.List as List
import Hydra.Chain (HeadParameters (..))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  Event (NetworkEvent),
  NoSnapshotReason (..),
  Outcome (..),
  SeenSnapshot (..),
  SnapshotOutcome (..),
  collectEffects,
  defaultTTL,
  emitSnapshot,
  isLeader,
  newSn,
  update,
 )
import Hydra.HeadLogicSpec (inOpenState')
import Hydra.Ledger (Ledger (..), txId)
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, cperiod)
import Test.QuickCheck (Property, counterexample, forAll, oneof, (==>))
import Test.QuickCheck.Monadic (monadicST, pick)

spec :: Spec
spec = do
  parallel $ do
    let threeParties = [alice, bob, carol]
        Ledger{initUTxO} = simpleLedger
        envFor signingKey =
          let party = deriveParty signingKey
           in Environment
                { party
                , signingKey
                , otherParties = List.delete party threeParties
                , contestationPeriod = defaultContestationPeriod
                }

    let params = HeadParameters cperiod threeParties

    let coordinatedHeadState =
          CoordinatedHeadState
            { seenUTxO = initUTxO
            , allTxs = mempty
            , seenTxs = mempty
            , confirmedSnapshot = InitialSnapshot initUTxO
            , seenSnapshot = NoSeenSnapshot
            }

    describe "New Snapshot Decision" $ do
      it "sends ReqSn given is leader and no snapshot in flight and there's a seen tx" $ do
        let tx = aValidTx 1
            st = coordinatedHeadState{seenTxs = [tx]}
        newSn (envFor aliceSk) params st `shouldBe` ShouldSnapshot 1 [tx]

      prop "always ReqSn given head has 1 member and there's a seen tx" prop_singleMemberHeadAlwaysSnapshot
      -- prop "given head has 1 member, always ReqSn after confirming a snapshot " prop_singleMemberHeadAlwaysSnapshotOnAckSn

      prop "there's always a leader for every snapshot number" prop_thereIsAlwaysALeader

      it "do not send ReqSn when we aren't leader" $ do
        let tx = aValidTx 1
            st = coordinatedHeadState{seenTxs = [tx]}
        newSn (envFor bobSk) params st `shouldBe` ShouldNotSnapshot (NotLeader 1)

      it "do not send ReqSn when there is a snapshot in flight" $ do
        let sn1 = Snapshot 1 initUTxO mempty :: Snapshot SimpleTx
            st = coordinatedHeadState{seenSnapshot = SeenSnapshot sn1 mempty}
        newSn (envFor aliceSk) params st `shouldBe` ShouldNotSnapshot (SnapshotInFlight 1)

      it "do not send ReqSn when there's no seen transactions" $ do
        newSn (envFor aliceSk) params coordinatedHeadState
          `shouldBe` ShouldNotSnapshot NoTransactionsToSnapshot

      describe "Snapshot Emission" $ do
        it "update seenSnapshot state when sending ReqSn" $ do
          let tx = aValidTx 1
              st = inOpenState' threeParties coordinatedHeadState{seenTxs = [tx]}
              st' =
                inOpenState' threeParties $
                  coordinatedHeadState
                    { seenTxs = [tx]
                    , seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}
                    }

          emitSnapshot (envFor aliceSk) (NewState st)
            `shouldBe` Combined (NewState st') (Effects [NetworkEffect $ ReqSn 1 [1]])

prop_singleMemberHeadAlwaysSnapshot :: ConfirmedSnapshot SimpleTx -> Property
prop_singleMemberHeadAlwaysSnapshot sn = monadicST $ do
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
            }
    st =
      CoordinatedHeadState
        { seenUTxO = mempty
        , allTxs = mempty
        , seenTxs = []
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
      length parties
        > 0
        ==> any (\p -> isLeader params p sn) parties
