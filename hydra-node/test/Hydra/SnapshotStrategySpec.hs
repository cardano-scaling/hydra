{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.SnapshotStrategySpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Data.List as List
import Hydra.Chain (HeadParameters (..))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  NoSnapshotReason (..),
  Outcome (..),
  SeenSnapshot (..),
  SnapshotOutcome (..),
  emitSnapshot,
  isLeader,
  newSn,
 )
import Hydra.HeadLogicSpec (inOpenState')
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, cperiod)
import Test.QuickCheck (Property, counterexample, forAll, label, oneof, (===), (==>))
import Test.QuickCheck.Monadic (monadicST, pick)
import qualified Prelude

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

    describe "New Snapshot Decision" $ do
      it "sends ReqSn given is leader and no snapshot in flight and there's a seen tx" $ do
        let tx = aValidTx 1
            st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = [tx]
                , confirmedSnapshot = InitialSnapshot initUTxO
                , seenSnapshot = NoSeenSnapshot
                }
        newSn (envFor aliceSk) params st `shouldBe` ShouldSnapshot 1 [tx]

      prop "always ReqSn given head has 1 member and there's a seen tx" prop_singleMemberHeadAlwaysSnapshot

      prop "there's always a leader for every snapsnot number" prop_thereIsAlwaysALeader

      it "do not send ReqSn when we aren't leader" $ do
        let tx = aValidTx 1
            st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = [tx]
                , confirmedSnapshot = InitialSnapshot initUTxO
                , seenSnapshot = NoSeenSnapshot
                }
        newSn (envFor bobSk) params st `shouldBe` ShouldNotSnapshot (NotLeader 1)

      it "do not send ReqSn when there is a snapshot in flight" $ do
        let sn1 = Snapshot 1 initUTxO mempty :: Snapshot SimpleTx
            st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = mempty
                , confirmedSnapshot = InitialSnapshot initUTxO
                , seenSnapshot = SeenSnapshot sn1 mempty
                }
        newSn (envFor aliceSk) params st `shouldBe` ShouldNotSnapshot (SnapshotInFlight 1)

      it "do not send ReqSn when there's no seen transactions" $ do
        let st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = mempty
                , confirmedSnapshot = InitialSnapshot initUTxO
                , seenSnapshot = NoSeenSnapshot
                } ::
                CoordinatedHeadState SimpleTx
        newSn (envFor aliceSk) params st `shouldBe` ShouldNotSnapshot NoTransactionsToSnapshot

      describe "Snapshot Emission" $ do
        it "update seenSnapshot state when sending ReqSn" $ do
          let tx = aValidTx 1
              coordinatedState =
                CoordinatedHeadState
                  { seenUTxO = initUTxO
                  , seenTxs = [tx]
                  , confirmedSnapshot = InitialSnapshot initUTxO
                  , seenSnapshot = NoSeenSnapshot
                  }
              st =
                inOpenState' threeParties coordinatedState
              st' =
                inOpenState' threeParties $
                  coordinatedState{seenSnapshot = RequestedSnapshot{lastSeen = 0, requested = 1}}

          emitSnapshot (envFor aliceSk) (NewState st [])
            `shouldBe` NewState st' [NetworkEffect $ ReqSn alice 1 [tx]]

prop_singleMemberHeadAlwaysSnapshot :: ConfirmedSnapshot SimpleTx -> Property
prop_singleMemberHeadAlwaysSnapshot sn = monadicST $ do
  seenSnapshot <-
    pick $
      oneof
        [ pure NoSeenSnapshot
        , LastSeenSnapshot <$> arbitrary
        ]
  let tx = aValidTx 1
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
          , seenTxs = [tx]
          , confirmedSnapshot = sn
          , seenSnapshot
          }
      params = HeadParameters cperiod [alice]
      decision = newSn aliceEnv params st
      Snapshot{number} = getSnapshot sn
  pure $
    decision
      === ShouldSnapshot (succ number) [tx]
      & counterexample ("decision: " <> show decision)
      & label (Prelude.head . Prelude.words . show $ sn)

prop_thereIsAlwaysALeader :: Property
prop_thereIsAlwaysALeader =
  forAll arbitrary $ \sn ->
    forAll arbitrary $ \params@HeadParameters{parties} ->
      length parties > 0
        ==> any (\p -> isLeader params p sn) parties
