{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain.Settlement (
  Stage (..),
  SettlementTopology,
  Settlement (..),
  Args (..),
  workflow,
  maybeRequestSnapshotAfterVersionBump,
) where

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map (delete, lookup)
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Program (Program, traceB)
import Hydra.HeadLogic.Rules.Coordinated (sendSnapshot)
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (party, snapshotLimits),
 )
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.Snapshot.Request (
  SnapshotRequest,
  chainedRequest,
  isLeader,
  settledRequestContext,
 )
import Hydra.HeadLogic.Snapshot.Verification (verifySeenSnapshot)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    currentDepositTxId,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, parameters),
  SeenSnapshot (LastSeenSnapshot, SeenSnapshot, cachedSnapshot),
  isCollectingAcks,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  Runtime,
  adoptSnapshot,
  currentState,
  recordCommitFinalization,
  recordDecommitFinalization,
  start,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (
  workflow,
 )
import Hydra.Node.State (Deposit (deposited), PendingDeposits)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, TxIdType, UTxOType)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  getSnapshot,
 )

data Stage
  = RecordSettlement
  | ChooseCandidate
  | RequestNext
  | Finished

type SettlementTopology =
  'Topology
    '[ '( 'RecordSettlement, '[ 'ChooseCandidate])
     , '( 'ChooseCandidate, '[ 'RequestNext])
     , '( 'RequestNext, '[ 'Finished])
     ]
deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data Settlement tx = Increment (TxIdType tx) | Decrement ~(UTxOType tx)

data State tx phase where
  Record :: State tx 'RecordSettlement
  Candidate ::
    Coordinated.Runtime tx ->
    State tx 'ChooseCandidate
  Request ::
    Coordinated.Runtime tx ->
    Snapshot tx ->
    SeenSnapshot tx ->
    State tx 'RequestNext
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  Record -> RecordSettlement
  Candidate{} -> ChooseCandidate
  Request{} -> RequestNext
  Done -> Finished

data Args tx = Args
  { env :: ~RuleEnvironment
  , pendingDeposits :: ~(PendingDeposits tx)
  , openState :: ~(OpenState tx)
  , newChainState :: ~(ChainStateType tx)
  , newVersion :: ~SnapshotVersion
  , settlement :: ~(Settlement tx)
  , snapshotNumber :: ~SnapshotNumber
  , signatures :: ~(MultiSignature (Snapshot tx))
  }

workflow :: forall tx. IsChainState tx => Args tx -> Workflow tx SettlementTopology ()
workflow Args{openState = openState@OpenState{..}, ..} =
  Workflow.workflow (show . statePhase) Record step
 where
  step :: State tx from -> Transition tx SettlementTopology (State tx) from ()
  step = \case
    Record -> continue Candidate $ Staging.stage (Coordinated.start openState) $ case settlement of
      Increment depositId ->
        Coordinated.recordCommitFinalization
          newChainState
          newVersion
          depositId
          (maybe mempty (.deposited) $ Map.lookup depositId pendingDeposits)
      Decrement distributed ->
        Coordinated.recordDecommitFinalization
          newChainState
          newVersion
          distributed
    Candidate runtime -> case seenSnapshot of
      SeenSnapshot{cachedSnapshot = (Cached.snapshot -> snapshot)}
        | snapshot.number == snapshotNumber
        , snapshot.version + 1 == newVersion
        , matchesSettlement snapshot -> choose $ do
            void $ traceB "settlement matches the seen snapshot" True
            verified <- verifySeenSnapshot vkeys seenSnapshot signatures
            case verified of
              Right evidence -> do
                void $ traceB "settlement signatures verify" True
                adopted <- Staging.stage runtime $ Coordinated.adoptSnapshot evidence
                pure $ next $ Request adopted snapshot (LastSeenSnapshot snapshot.number)
              Left _ -> do
                void $ traceB "settlement signatures verify" False
                pure $ next $ Request runtime previous seenSnapshot
      _ -> continue (const $ Request runtime previous seenSnapshot) $ traceB "settlement matches the seen snapshot" False
    Request runtime snapshot seen ->
      continue (const Done) $
        maybeRequestSnapshotAfterVersionBump
          runtime
          parameters
          env.party
          (snapshot.number + 1)
          (requestOn runtime snapshot)
          version
          newVersion
          seen
    Done -> finish Done ()

  matchesSettlement :: Snapshot tx -> Bool
  matchesSettlement snapshot = case settlement of
    Increment depositTxId -> snapshot.depositTxId == Just depositTxId
    Decrement{} -> isJust snapshot.utxoToDecommit
  requestOn runtime snapshot =
    let current = Coordinated.currentState runtime
     in case settlement of
          Increment depositId -> chainedRequest env.snapshotLimits $ settledRequestContext newVersion snapshot (Map.delete depositId pendingDeposits) current{currentDepositTxId = Nothing}
          Decrement{} -> chainedRequest env.snapshotLimits $ settledRequestContext newVersion snapshot pendingDeposits current

  previous = getSnapshot confirmedSnapshot
  vkeys = vkey <$> parameters.parties
  CoordinatedHeadState{..} = coordinatedHeadState

maybeRequestSnapshotAfterVersionBump ::
  IsTx tx =>
  Coordinated.Runtime tx ->
  HeadParameters ->
  Party ->
  SnapshotNumber ->
  Maybe (SnapshotRequest tx) ->
  SnapshotVersion ->
  SnapshotVersion ->
  SeenSnapshot tx ->
  Program tx ()
maybeRequestSnapshotAfterVersionBump runtime parameters party nextSn request version newVersion seenSnapshot =
  case request of
    Nothing -> pure ()
    Just proposal -> do
      eligible <-
        traceB "leader can request at the advanced version" $
          isLeader parameters party nextSn && newVersion > version && not (isCollectingAcks seenSnapshot)
      when eligible $ do
        sendSnapshot runtime proposal
