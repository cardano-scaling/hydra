{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | A finite schedule family for the boundary between a snapshot round and
-- settlement. Chain observations are trusted inputs; validators are not run.
module Test.Hydra.HeadLogic.DecommitSequences (
  Schedule (..),
  Event (..),
  Step (..),
  schedules,
  runSchedule,
  deliveryLimit,
) where

import Hydra.Prelude hiding (label, state)

import Data.Map.Strict qualified as Map
import Data.Secret (Secret)
import Hydra.Chain (ChainEvent (Observation), OnChainTx (OnDecrementTx))
import Hydra.Chain qualified as Chain
import Hydra.HeadLogic.Retry (RetryDecision (RetryInput), retryDecision)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (SeenSnapshot))
import Hydra.Ledger.Simple (SimpleChainState (SimpleChainState), SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.HeadLogic qualified as Head
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (HydraKey, MultiSignature, SigningKey, Verified (Verified), aggregateInOrder, sign, verifyMultiSignature)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot, InitialSnapshot), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)
import Test.Hydra.HeadLogic.Recovery (recover)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)

data Event = Acknowledge Int | Observe | Rollback | Reobserve
  deriving stock (Eq, Show)

data Schedule = Schedule
  { events :: [Event]
  , reserveLocally :: Bool
  , rollbackSlot :: Int
  , observationCarriesCertificate :: Bool
  }
  deriving stock (Show)

data Step = Step
  { recipient :: Int
  , label :: Text
  , input :: Head.Input SimpleTx
  , outcome :: Head.Outcome SimpleTx
  , after :: NodeState SimpleTx
  }
  deriving stock (Show)

-- 20 order-preserving interleavings x 6 signature orders x 2 reservations
-- x 2 rollback boundaries x 2 certificate cases = 960 schedules.
schedules :: [Schedule]
schedules =
  [ Schedule order reserve slot certificate
  | acknowledgements <- permutations [Acknowledge 0, Acknowledge 1, Acknowledge 2]
  , order <- interleave acknowledgements [Observe, Rollback, Reobserve]
  , reserve <- [False, True]
  , slot <- [1, 2]
  , certificate <- [False, True]
  ]
 where
  interleave :: [a] -> [a] -> [[a]]
  interleave [] ys = [ys]
  interleave xs [] = [xs]
  interleave (x : xs) (y : ys) = map (x :) (interleave xs (y : ys)) <> map (y :) (interleave (x : xs) ys)

deliveryLimit :: Int
deliveryLimit = 24

-- | Run the same schedule at each party. The projection holds other network
-- effects in flight; it tests safety and recovery, not eventual delivery.
-- A dependency arrives early and is retried using production eligibility.
runSchedule :: Schedule -> Either Text [Step]
runSchedule schedule = do
  steps <- concat <$> traverse runAt [0, 1, 2]
  void $ foldlM agreement mempty steps
  pure steps
 where
  agreement :: Map (SnapshotVersion, SnapshotNumber) (Snapshot SimpleTx) -> Step -> Either Text (Map (SnapshotVersion, SnapshotNumber) (Snapshot SimpleTx))
  agreement history entry = do
    let snapshot = getSnapshot (Scenario.openState entry.after).coordinatedHeadState.confirmedSnapshot
        roundId = (snapshot.version, snapshot.number)
    forM_ (Map.lookup roundId history) $ \previous ->
      unless (previous == snapshot) $ Left "parties disagree at the same version and snapshot number"
    pure $ Map.insert roundId snapshot history
  runAt recipient = first (\err -> "party " <> show recipient <> ", " <> show schedule <> ": " <> err) $ do
    reserved <- if schedule.reserveLocally then step recipient "reserve decommit" (network alice $ ReqDec decommit) (initial, []) else Right (initial, [])
    requested <- step recipient "receive candidate before ReqDec" (network alice $ ReqSn 0 1 [] (Just decommit) Nothing) reserved
    candidate <- case (Scenario.openState $ fst requested).coordinatedHeadState.seenSnapshot of
      SeenSnapshot snapshot _ _ -> Right snapshot
      _ -> Left "request did not create a candidate"
    unless (candidate.utxo == utxoRef 2 <> utxoRef 3 && candidate.utxoToDecommit == Just (utxoRef 10)) $ Left "candidate has the wrong UTxO"
    let early = network alice $ ReqTx child
    waiting <- step recipient "receive dependent transaction first" early requested
    retry <- case snd waiting of
      Step{outcome = Head.Wait reason _} : _ -> case retryDecision reason early of
        RetryInput input -> Right input
        _ -> Left "production policy did not allow the dependency retry"
      _ -> Left "dependent transaction did not wait"
    parentReceived <- step recipient "receive parent transaction" (network alice $ ReqTx parent) waiting
    applied <- step recipient "retry dependent transaction" retry parentReceived
    settled <- foldlM (scheduled recipient candidate) applied schedule.events
    let current = (Scenario.openState $ fst settled).coordinatedHeadState
    unless (current.version == 1 && (getSnapshot current.confirmedSnapshot).number == 1) $ Left "first round did not settle and confirm"
    nextRequested <- step recipient "request next round after settlement" (network bob $ ReqSn 1 2 [20, 21] Nothing Nothing) settled
    nextCandidate <- case (Scenario.openState $ fst nextRequested).coordinatedHeadState.seenSnapshot of
      SeenSnapshot snapshot _ _ -> Right snapshot
      _ -> Left "second request did not create a candidate"
    confirmed <- foldlM (\pending (party, key) -> step recipient "confirm next round" (network party $ AckSn (sign key nextCandidate) 2) pending) nextRequested signers
    rolled <- preserving recipient "rollback after the next confirmation" (rollback 1) confirmed
    final <- preserving recipient "re-observe after the next confirmation" (observation 5 candidate) rolled
    let result = (Scenario.openState $ fst final).coordinatedHeadState
        snapshot = getSnapshot result.confirmedSnapshot
    unless (snapshot.number == 2 && snapshot.version == 1 && snapshot.utxo == utxoRef 3 <> utxoRef 21 && snapshot.confirmed == [parent, child] && isNothing snapshot.utxoToDecommit) $ Left "dependency order or settled outputs differ in the next confirmation"
    unless (result.localUTxO == snapshot.utxo && null result.localTxs && isNothing result.decommitTx && Map.notMember 10 result.allTxs) $ Left "settlement resurrected work or left a reservation"
    unless (fmap (.snapshot) result.finalizedDecommit == Just (ConfirmedSnapshot candidate $ signatures candidate) && fmap (.observedAtSlot) result.finalizedDecommit == Just 5) $ Left "settlement lost its certificate or latest observation slot"
    unless (length (snd final) <= deliveryLimit) $ Left "schedule exceeds the delivery limit"
    pure $ reverse $ snd final

  scheduled recipient candidate current = \case
    Acknowledge i -> case drop i signers of
      (party, key) : _ -> step recipient ("AckSn 1 from party " <> show i) (network party $ AckSn (sign key candidate) 1) current
      _ -> Left "unknown signer"
    Observe -> step recipient "observe decrement" (observation 2 candidate) current
    Rollback -> preserving recipient "rollback around decrement" (rollback schedule.rollbackSlot) current
    Reobserve -> preserving recipient "re-observe decrement" (observation 4 candidate) current
  observation slot candidate =
    Head.ChainInput $ Observation (OnDecrementTx candidate.headId 1 (utxoRef 10) 1 (if schedule.observationCarriesCertificate then signatures candidate else mempty)) (SimpleChainState slot)
  rollback :: Int -> Head.Input SimpleTx
  rollback slot = Head.ChainInput $ Chain.Rollback{Chain.rolledBackChainState = SimpleChainState $ fromIntegral slot, Chain.chainTime = initialChainTime}

-- The checker does not call production ownership predicates or derive expected
-- state by executing the same machine a second time.
step :: Int -> Text -> Head.Input SimpleTx -> (NodeState SimpleTx, [Step]) -> Either Text (NodeState SimpleTx, [Step])
step recipient label input (before, history) = do
  let outcome = Head.update (Scenario.environment recipient) simpleLedger initialChainTime before input
  case outcome of Head.Error err -> Left $ label <> ": " <> show err; _ -> pure ()
  after <- recover before outcome
  let current = (Scenario.openState after).coordinatedHeadState
  case current.confirmedSnapshot of
    ConfirmedSnapshot snapshot certificate -> do
      unless (verifyMultiSignature (vkey <$> Scenario.scenarioParties) certificate snapshot == Verified) $ Left "invalid confirmed signatures"
      when (current.version == snapshot.version && snapshot.utxoToDecommit == Just (utxoRef 10)) $
        unless (current.decommitTx == Just decommit) $
          Left "authorised decommit lost its payload"
    InitialSnapshot{} -> Left "fixture lost its initial certificate"
  forM_ current.localTxs $ \tx ->
    unless (Map.lookup tx.txSimpleId current.allTxs == Just tx) $ Left "pending transaction lost its registry payload"
  pure (after, Step{recipient, label, input, outcome, after} : history)

preserving :: Int -> Text -> Head.Input SimpleTx -> (NodeState SimpleTx, [Step]) -> Either Text (NodeState SimpleTx, [Step])
preserving recipient label input before = do
  after <- step recipient label input before
  unless (l2 (fst before) == l2 (fst after)) $ Left $ label <> " changed retained L2 state"
  case (input, snd after) of
    (Head.ChainInput Chain.Rollback{Chain.rolledBackChainState = SimpleChainState slot}, Step{outcome = Head.Continue _ effects} : _) -> do
      let retained = (Scenario.openState $ fst before).coordinatedHeadState.finalizedDecommit
          expected = [signed.snapshot | signed <- toList retained, signed.observedAtSlot > slot]
          actual = [signed | Head.OnChainEffect{Head.postChainTx = Chain.DecrementTx{Chain.decrementingSnapshot = signed}} <- effects]
      unless (actual == expected) $ Left "rollback did not repost exactly the erased certificate"
    _ -> pure ()
  pure after
 where
  l2 node =
    let c = (Scenario.openState node).coordinatedHeadState
     in (c.version, c.confirmedSnapshot, c.seenSnapshot, c.localUTxO, c.localTxs, c.allTxs, c.decommitTx)

network :: Party -> Message SimpleTx -> Head.Input SimpleTx
network sender = Head.NetworkInput 2 . ReceivedMessage sender

decommit, parent, child :: SimpleTx
decommit = SimpleTx 10 (utxoRef 1) (utxoRef 10)
parent = SimpleTx 20 (utxoRef 2) (utxoRef 20)
child = SimpleTx 21 (utxoRef 20) (utxoRef 21)

signers :: [(Party, Secret (SigningKey HydraKey))]
signers = [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]

signatures :: Snapshot SimpleTx -> MultiSignature (Snapshot SimpleTx)
signatures snapshot = aggregateInOrder (Map.fromList [(party, sign key snapshot) | (party, key) <- signers]) Scenario.scenarioParties

initial :: NodeState SimpleTx
initial =
  let utxo = utxoRef 1 <> utxoRef 2 <> utxoRef 3
      snapshot :: Snapshot SimpleTx
      snapshot = Scenario.initialSnapshot{utxo, accumulator = Accumulator.buildFromUTxO @SimpleTx utxo, appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx utxo}
      open = Scenario.openState Scenario.initialNode
   in Scenario.initialNode{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{localUTxO = utxo, confirmedSnapshot = ConfirmedSnapshot snapshot (signatures snapshot)}}}
