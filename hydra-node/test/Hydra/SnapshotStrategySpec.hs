{-# LANGUAGE TypeApplications #-}

module Hydra.SnapshotStrategySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (HeadParameters))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  Outcome (..),
  SeenSnapshot (NoSeenSnapshot),
  SnapshotOutcome (..),
  newSn,
  update,
 )
import Hydra.Ledger (Ledger (..), Party, Tx (..))
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger)
import Hydra.Network.Message (Message (AckSn, ReqSn, ReqTx))
import Hydra.Snapshot (Snapshot (..))

spec :: Spec
spec = do
  parallel $ do
    let threeParties = [1, 2, 3]
        ledger = simpleLedger
        envIsLeader =
          Environment
            { party = 1
            , signingKey = 1
            , otherParties = [2, 3]
            }

        envNotLeader =
          Environment
            { party = 2
            , signingKey = 2
            , otherParties = [1, 3]
            }

    it "sends ReqSn given is leader and no snapshot in flight and there's a seen tx" $ do
      let s0 = inOpenState threeParties ledger
          tx = aValidTx 1
          reqTx = NetworkEvent $ ReqTx 1 tx
      s1 <- assertNewState $ update envIsLeader ledger s0 reqTx
      newSn envIsLeader s1 `shouldBe` SendReqSn 1 [tx]

    it "do not send ReqSn when we aren't leader" $ do
      let s0 = inOpenState threeParties ledger
          tx = aValidTx 1
          reqTx = NetworkEvent $ ReqTx 1 tx
      s1 <- assertNewState $ update envNotLeader ledger s0 reqTx
      newSn envNotLeader s1 `shouldBe` NotLeader 1

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
