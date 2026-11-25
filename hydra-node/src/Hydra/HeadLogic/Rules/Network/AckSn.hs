{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Network.AckSn (
  Stage (..),
  AckTopology,
  Args (..),
  workflow,
) where

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map (
  insert,
  keys,
  keysSet,
  lookup,
  notMember,
 )
import Data.Set qualified as Set (fromList)
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Cardano (assembleCertificate)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
 )
import Hydra.HeadLogic.Outcome (
  Effect (..),
  StateChanged (..),
  WaitReason (..),
 )
import Hydra.HeadLogic.Program (
  Program,
  change,
  decide,
  emit,
  reject,
  suspend,
 )
import Hydra.HeadLogic.Rules.Coordinated (sendSnapshot)
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Snapshot.Request (
  chainedSnapshotRequest,
  decommitStatus,
  isLeader,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.Snapshot.Verification (
  VerifiedCandidate,
  candidateEvidence,
  verifiedSignatures,
  verifiedSnapshot,
  verifyCandidate,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    currentDepositTxId,
    decommitTx,
    localTxs,
    seenSnapshot,
    version
  ),
  OpenState (
    OpenState,
    coordinatedHeadState,
    headId,
    headSeed,
    parameters
  ),
  SeenSnapshot (
    LastSeenSnapshot,
    SeenSnapshot,
    lastSeen,
    signatories
  ),
  seenSnapshotNumber,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  Candidate,
  Runtime,
  candidate,
  confirmSnapshot,
  currentState,
  recordSignature,
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
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  PendingDeposits,
 )
import Hydra.Prelude hiding (State, round, state)
import Hydra.Tx (txId)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
 )

data Stage
  = CheckNumber
  | FindSnapshot
  | CollectSignature
  | CheckComplete
  | ConfirmAndContinue
  | Finished

type AckTopology =
  'Topology
    '[ '( 'CheckNumber, '[ 'FindSnapshot])
     , '( 'FindSnapshot, '[ 'CollectSignature, 'Finished])
     , '( 'CollectSignature, '[ 'CheckComplete])
     , '( 'CheckComplete, '[ 'ConfirmAndContinue, 'Finished])
     , '( 'ConfirmAndContinue, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  NumberCheck :: State tx 'CheckNumber
  Find :: State tx 'FindSnapshot
  Collect ::
    Map Party (Signature (Snapshot tx)) ->
    State tx 'CollectSignature
  Collected ::
    Map Party (Signature (Snapshot tx)) ->
    Coordinated.Runtime tx ->
    State tx 'CheckComplete
  Confirm ::
    VerifiedCandidate tx ->
    Coordinated.Runtime tx ->
    State tx 'ConfirmAndContinue
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  NumberCheck -> CheckNumber
  Find -> FindSnapshot
  Collect{} -> CollectSignature
  Collected{} -> CheckComplete
  Confirm{} -> ConfirmAndContinue
  Done -> Finished

data Args tx = Args
  { env :: ~RuleEnvironment
  , chainPointTime :: ~ChainPointTime
  , pendingDeposits :: ~(PendingDeposits tx)
  , openState :: ~(OpenState tx)
  , sender :: ~Party
  , signed :: ~(Signature (Snapshot tx))
  , snapshotNumber :: ~SnapshotNumber
  }

workflow :: forall tx. IsChainState tx => Args tx -> Workflow tx AckTopology ()
workflow Args{chainPointTime = ChainPointTime{currentChainTime}, ..} =
  Workflow.workflow (show . statePhase) NumberCheck step
 where
  step :: State tx from -> Transition tx AckTopology (State tx) from ()
  step = \case
    NumberCheck -> continue (const Find) $ do
      valid <- decide "acknowledgement number is admissible" (snapshotNumber `elem` [lastSeenSn, lastSeenSn + 1])
      unless valid $ reject $ RequireFailed AckSnNumberInvalid{requestedSn = snapshotNumber, ..}
    Find -> case seenSnapshot of
      LastSeenSnapshot{lastSeen} | snapshotNumber <= lastSeen -> continue (const Done) $ pure ()
      SeenSnapshot{signatories}
        | lastSeenSn == snapshotNumber ->
            continue (const $ Collect signatories) $ pure ()
      _ -> continue (const Done) $ suspend WaitOnSeenSnapshot
    Collect signatories -> continue (Collected (Map.insert sender signed signatories)) $ do
      -- A retained invalid partial signature still blocks replacement. Recovery
      -- from that case is deferred; aggregate verification remains mandatory.
      fresh <- decide "party has not signed this snapshot" (Map.notMember sender signatories)
      unless fresh $
        reject $
          RequireFailed SnapshotAlreadySigned{knownSignatures = Map.keys signatories, receivedSignature = sender}
      let runtime = Coordinated.start openState
      owned <- requireCandidate runtime
      Staging.stage runtime $ Coordinated.recordSignature owned sender signed
    Collected allSignatures runtime ->
      if Map.keysSet allSignatures == Set.fromList parties
        then
          let signatures = assembleCertificate allSignatures parties
           in choose $ do
                void $ decide "all parties have signed" True
                owned <- requireCandidate runtime
                result <- verifyCandidate owned signatures
                case result of
                  Right verified -> pure $ next $ Confirm verified runtime
                  Left err -> reject err
        else continue (const Done) $ do
          void $ decide "all parties have signed" False
          owned <- requireCandidate runtime
          void $ Staging.stage runtime $ Coordinated.recordSignature owned sender signed
    Confirm verified runtime -> continue (const Done) $ do
      confirmedRuntime <- Staging.stage runtime $ Coordinated.confirmSnapshot verified
      let snapshot = verifiedSnapshot $ candidateEvidence verified
          signatures = verifiedSignatures $ candidateEvidence verified
          current = Coordinated.currentState confirmedRuntime
      case (snapshot.depositTxId, snapshot.utxoToCommit) of
        (Just depositTxId, Just _)
          | Just Deposit{deposited} <- Map.lookup depositTxId pendingDeposits -> do
              change CommitApproved{utxoToCommit = deposited, ..}
              emit OnChainEffect{postChainTx = IncrementTx{incrementingSnapshot = ConfirmedSnapshot{..}, ..}}
        _ -> pure ()
      case (current.decommitTx, snapshot.utxoToDecommit) of
        (Just tx, Just utxoToDecommit) -> do
          change DecommitApproved{decommitTxId = txId tx, ..}
          emit OnChainEffect{postChainTx = DecrementTx{decrementingSnapshot = ConfirmedSnapshot{..}, ..}}
        _ -> pure ()
      let nextSn = snapshot.number + 1
          req =
            chainedSnapshotRequest
              env.snapshotLimits
              current.version
              snapshot
              current.localTxs
              pendingDeposits
              current.currentDepositTxId
              (decommitStatus current.version snapshot current.decommitTx)
              (settlingCommitAtPeriod env.depositPeriod current.version currentChainTime pendingDeposits snapshot)
      case req of
        Just request | isLeader headParameters env.party nextSn -> sendSnapshot confirmedRuntime request
        _ -> pure ()
    Done -> finish Done ()

  OpenState{parameters = headParameters@HeadParameters{parties}, ..} = openState
  CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
  lastSeenSn = seenSnapshotNumber seenSnapshot

  requireCandidate :: Coordinated.Runtime tx -> Program tx (Coordinated.Candidate tx)
  requireCandidate = maybe (reject $ AssertionFailed "snapshot owner has no current candidate") pure . Coordinated.candidate
