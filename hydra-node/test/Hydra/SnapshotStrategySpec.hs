{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.SnapshotStrategySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.List as List
import Hydra.Chain (HeadParameters (HeadParameters))
import Hydra.Crypto (generateSigningKey)
import qualified Hydra.Crypto as Hydra
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  HeadState (..),
  NoSnapshotReason (..),
  SeenSnapshot (..),
  SnapshotOutcome (..),
  emitSnapshot,
  newSn,
 )
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))

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
                }

    let params = HeadParameters 42 threeParties

    describe "New Snapshot Decision" $ do
      it "sends ReqSn given is leader and no snapshot in flight and there's a seen tx" $ do
        let tx = aValidTx 1
            st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = [tx]
                , confirmedSnapshot = InitialSnapshot $ Snapshot 0 initUTxO mempty
                , seenSnapshot = NoSeenSnapshot
                }
        newSn (envFor aliceSk) params st `shouldBe` ShouldSnapshot 1 [tx]

      it "do not send ReqSn when we aren't leader" $ do
        let tx = aValidTx 1
            st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = [tx]
                , confirmedSnapshot = InitialSnapshot $ Snapshot 0 initUTxO mempty
                , seenSnapshot = NoSeenSnapshot
                }
        newSn (envFor bobSk) params st `shouldBe` ShouldNotSnapshot (NotLeader 1)

      it "do not send ReqSn when there is a snapshot in flight" $ do
        let sn1 = Snapshot 1 initUTxO mempty :: Snapshot SimpleTx
            st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = mempty
                , confirmedSnapshot = InitialSnapshot $ Snapshot 0 initUTxO mempty
                , seenSnapshot = SeenSnapshot sn1 mempty
                }
        newSn (envFor aliceSk) params st `shouldBe` ShouldNotSnapshot (SnapshotInFlight 1)

      it "do not send ReqSn when there's no seen transactions" $ do
        let st =
              CoordinatedHeadState
                { seenUTxO = initUTxO
                , seenTxs = mempty
                , confirmedSnapshot = InitialSnapshot $ Snapshot 0 initUTxO mempty
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
                  , confirmedSnapshot = InitialSnapshot $ Snapshot 0 initUTxO mempty
                  , seenSnapshot = NoSeenSnapshot
                  }
              st =
                inOpenState' @SimpleTx threeParties coordinatedState
              st' =
                inOpenState' @SimpleTx threeParties $
                  coordinatedState{seenSnapshot = RequestedSnapshot}

          emitSnapshot (envFor aliceSk) [] st
            `shouldBe` (st', [NetworkEffect $ ReqSn alice 1 [tx]])

aliceSk, bobSk, carolSk :: Hydra.SigningKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
carolSk = generateSigningKey "carol"

alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

--
-- Assertion utilities
--

inOpenState' ::
  [Party] ->
  CoordinatedHeadState tx ->
  HeadState tx
inOpenState' parties coordinatedHeadState =
  OpenState{parameters, coordinatedHeadState, previousRecoverableState}
 where
  parameters = HeadParameters 42 parties
  previousRecoverableState =
    InitialState
      { parameters
      , pendingCommits = mempty
      , committed = mempty
      , previousRecoverableState = ReadyState
      }
