{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.SnapshotStrategySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.List as List
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (HeadParameters))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  NoSnapshotReason (..),
  Outcome (..),
  SeenSnapshot (..),
  SnapshotOutcome (..),
  emitSnapshot,
  newSn,
 )
import Hydra.Ledger (IsTx (..), Ledger (..))
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (Snapshot (..))

spec :: Spec
spec = do
  parallel $ do
    let threeParties = [1, 2, 3]
        Ledger{initUtxo} = simpleLedger
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
                { seenUtxo = initUtxo
                , seenTxs = [tx]
                , confirmedSnapshot = Snapshot 0 initUtxo mempty
                , seenSnapshot = NoSeenSnapshot
                }
        newSn (envFor 1) params st `shouldBe` ShouldSnapshot 1 [tx]

      it "do not send ReqSn when we aren't leader" $ do
        let tx = aValidTx 1
            st =
              CoordinatedHeadState
                { seenUtxo = initUtxo
                , seenTxs = [tx]
                , confirmedSnapshot = Snapshot 0 initUtxo mempty
                , seenSnapshot = NoSeenSnapshot
                }
        newSn (envFor 2) params st `shouldBe` ShouldNotSnapshot (NotLeader 1)

      it "do not send ReqSn when there is a snapshot in flight" $ do
        let sn1 = Snapshot 1 initUtxo mempty :: Snapshot SimpleTx
            st =
              CoordinatedHeadState
                { seenUtxo = initUtxo
                , seenTxs = mempty
                , confirmedSnapshot = Snapshot 0 initUtxo mempty
                , seenSnapshot = SeenSnapshot sn1 (Set.fromList [])
                }
        newSn (envFor 1) params st `shouldBe` ShouldNotSnapshot (SnapshotInFlight 1)

      it "do not send ReqSn when there's no seen transactions" $ do
        let st =
              CoordinatedHeadState
                { seenUtxo = initUtxo
                , seenTxs = mempty
                , confirmedSnapshot = Snapshot 0 initUtxo mempty
                , seenSnapshot = NoSeenSnapshot
                } ::
                CoordinatedHeadState SimpleTx
        newSn (envFor 1) params st `shouldBe` ShouldNotSnapshot NoTransactionsToSnapshot

      describe "Snapshot Emission" $ do
        it "update seenSnapshot state when sending ReqSn" $ do
          let tx = aValidTx 1
              coordinatedState =
                CoordinatedHeadState
                  { seenUtxo = initUtxo
                  , seenTxs = [tx]
                  , confirmedSnapshot = Snapshot 0 initUtxo mempty
                  , seenSnapshot = NoSeenSnapshot
                  }
              st =
                inOpenState' @SimpleTx threeParties coordinatedState
              st' =
                inOpenState' @SimpleTx threeParties $
                  coordinatedState{seenSnapshot = RequestedSnapshot}

          emitSnapshot (envFor 1) [] st
            `shouldBe` (st', [NetworkEffect $ ReqSn 1 1 [tx]])

--
-- Assertion utilities
--

hasEffect :: (HasCallStack, IsTx tx) => Outcome tx -> Effect tx -> IO (HeadState tx)
hasEffect (NewState s effects) effect
  | effect `elem` effects = pure s
  | otherwise = failure $ "Missing effect " <> show effect <> " in produced effects: " <> show effects
hasEffect o _ = failure $ "Unexpected outcome: " <> show o

hasEffect_ :: (HasCallStack, IsTx tx) => Outcome tx -> Effect tx -> IO ()
hasEffect_ o e = void $ hasEffect o e

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

assertNewState :: IsTx tx => Outcome tx -> IO (HeadState tx)
assertNewState = \case
  NewState st _ -> pure st
  Error e -> failure $ "Unexpected 'Error' outcome: " <> show e
  Wait -> failure "Unexpected 'Wait' outcome"

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
