{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain.Lifecycle where

import Crem.Topology (Topology (Topology))
import Data.Set qualified as Set (fromList)
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, decide, emit)
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (
    RuleEnvironment,
    contestationPeriod,
    depositPeriod,
    otherParties,
    participants,
    party
  ),
 )
import Hydra.HeadLogic.Snapshot.Verification (
  VerifiedSnapshot,
  verifiedSnapshot,
  verifySeenSnapshot,
 )
import Hydra.HeadLogic.State (
  ClosedState (
    ClosedState,
    confirmedSnapshot,
    headId,
    parameters,
    unconfirmedSnapshot,
    version
  ),
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  SeenSnapshot (SeenSnapshot, snapshot),
  mkSeenSnapshot,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  adoptSnapshot,
  start,
 )
import Hydra.HeadLogic.Workflow (Transition, Workflow, continue, finish)
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (HeadId, HeadSeed, IsTx)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot,
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  getSnapshot,
 )

data Stage
  = AdoptCandidate
  | RecordObservation
  | ContestNewer
  | Finished

type ObservationTopology =
  'Topology
    '[ '( 'AdoptCandidate, '[ 'RecordObservation])
     , '( 'RecordObservation, '[ 'ContestNewer])
     , '( 'ContestNewer, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State phase where
  Adopt :: State 'AdoptCandidate
  Record :: Bool -> State 'RecordObservation
  Contest :: Bool -> State 'ContestNewer
  Done :: State 'Finished

statePhase :: State phase -> Stage
statePhase = \case
  Adopt -> AdoptCandidate
  Record{} -> RecordObservation
  Contest{} -> ContestNewer
  Done -> Finished

-- | Observe an init transaction and initialize parameters in an 'OpenState'.
onIdleChainInitTx ::
  RuleEnvironment ->
  -- | New chain state.
  ChainStateType tx ->
  HeadId ->
  HeadSeed ->
  HeadParameters ->
  [OnChainId] ->
  Program tx ()
onIdleChainInitTx env newChainState headId headSeed headParameters participants
  | configuredParties == initializedParties
      && party `member` initializedParties
      && configuredContestationPeriod == contestationPeriod
      && configuredDepositPeriod == depositPeriod
      && Set.fromList configuredParticipants == Set.fromList participants =
      change
        HeadOpened
          { parameters = headParameters
          , chainState = newChainState
          , headId
          , headSeed
          , parties
          }
  | otherwise =
      change
        IgnoredHeadInitializing
          { headId
          , contestationPeriod
          , parties
          , participants
          }
 where
  initializedParties = Set.fromList parties

  configuredParties = Set.fromList (party : otherParties)

  HeadParameters{..} = headParameters

  RuleEnvironment
    { party
    , otherParties
    , contestationPeriod = configuredContestationPeriod
    , depositPeriod = configuredDepositPeriod
    , participants = configuredParticipants
    } = env

onOpenChainCloseTx ::
  IsChainState tx =>
  OpenState tx ->
  ChainStateType tx ->
  SnapshotNumber ->
  UTCTime ->
  MultiSignature (Snapshot tx) ->
  Program tx ()
onOpenChainCloseTx os@OpenState{..} newChainState closedNumber contestationDeadline signatures =
  let sn =
        adoptSnapshot
          parameters.parties
          closedNumber
          signatures
          candidate
          (void . Staging.stage (Coordinated.start os) . Coordinated.adoptSnapshot)
   in Workflow.run $
        maybeContestWorkflow
          sn
          (change HeadClosed{headId, snapshotNumber = closedNumber, chainState = newChainState, contestationDeadline})
          -- Close compares the confirmed snapshot from before possible adoption.
          (const $ postNewerSnapshot headId parameters version confirmedSnapshot closedNumber)
 where
  CoordinatedHeadState{..} = coordinatedHeadState
  candidate = case seenSnapshot of
    seen@SeenSnapshot{} -> Just seen
    _ -> Nothing

onClosedChainContestTx ::
  IsTx tx =>
  ClosedState tx ->
  ChainStateType tx ->
  SnapshotNumber ->
  UTCTime ->
  MultiSignature (Snapshot tx) ->
  Program tx ()
onClosedChainContestTx ClosedState{..} newChainState snapshotNumber contestationDeadline signatures =
  Workflow.run $
    maybeContestWorkflow
      ( adoptSnapshot parameters.parties snapshotNumber signatures ((`mkSeenSnapshot` mempty) <$> unconfirmedSnapshot) $ \evidence ->
          change SnapshotConfirmed{headId, snapshot = Just $ verifiedSnapshot evidence, signatures}
      )
      (change HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber})
      (\adopted -> unless adopted $ postNewerSnapshot headId parameters version confirmedSnapshot snapshotNumber)

maybeContestWorkflow ::
  forall tx.
  Program tx Bool ->
  Program tx () ->
  (Bool -> Program tx ()) ->
  Workflow tx ObservationTopology ()
maybeContestWorkflow tryAdopt recordObservation contestNewer =
  Workflow.workflow (show . statePhase) Adopt step
 where
  step :: State from -> Transition tx ObservationTopology State from ()
  step = \case
    Adopt -> continue Record tryAdopt
    Record adopted -> continue (const $ Contest adopted) recordObservation
    Contest adopted -> continue (const Done) $ contestNewer adopted
    Done -> finish Done ()

adoptSnapshot ::
  IsTx tx =>
  [Party] ->
  SnapshotNumber ->
  MultiSignature (Snapshot tx) ->
  Maybe (SeenSnapshot tx) ->
  (VerifiedSnapshot tx -> Program tx ()) ->
  Program tx Bool
adoptSnapshot parties observedNumber signatures candidate retain = case candidate of
  Just seen@SeenSnapshot{snapshot} | snapshot.number == observedNumber -> do
    void $ decide "chain observation matches the unconfirmed snapshot number" True
    verified <- verifySeenSnapshot (vkey <$> parties) seen signatures
    valid <- decide "observed snapshot signatures verify" (isRight verified)
    traverse_ retain verified
    pure valid
  _ -> decide "chain observation matches the unconfirmed snapshot number" False

postNewerSnapshot ::
  IsTx tx =>
  HeadId ->
  HeadParameters ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  SnapshotNumber ->
  Program tx ()
postNewerSnapshot headId headParameters openVersion confirmedSnapshot observedNumber = do
  newer <-
    decide
      "confirmed snapshot is newer than the on-chain snapshot"
      ((getSnapshot confirmedSnapshot).number > observedNumber)

  when newer . emit $
    OnChainEffect ContestTx{headId, headParameters, openVersion, contestingSnapshot = confirmedSnapshot}
