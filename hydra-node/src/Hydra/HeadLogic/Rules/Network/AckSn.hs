{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Collect signatures and confirm a snapshot. Confirmation requires a payload constructed
-- by the successful verification branch; its effects remain an ordinary program.
module Hydra.HeadLogic.Rules.Network.AckSn (Stage (..), AckTopology, handle, workflow) where

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
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  Program,
  change,
  decide,
  emit,
  reject,
  suspend,
 )
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
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
  run,
  workflow,
 )
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  PendingDeposits,
 )
import Hydra.Prelude hiding (State, handle, round, state)
import Hydra.Tx (txId)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
 )

data Stage = CheckNumber | FindSnapshot | CollectSignature | CheckComplete | ConfirmAndContinue | Finished

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
  Collect :: Map Party (Signature (Snapshot tx)) -> State tx 'CollectSignature
  Collected :: Map Party (Signature (Snapshot tx)) -> Coordinated.Runtime tx -> State tx 'CheckComplete
  Confirm :: VerifiedCandidate tx -> Coordinated.Runtime tx -> State tx 'ConfirmAndContinue
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  NumberCheck -> CheckNumber
  Find -> FindSnapshot
  Collect{} -> CollectSignature
  Collected{} -> CheckComplete
  Confirm{} -> ConfirmAndContinue
  Done -> Finished

workflow :: forall tx. IsChainState tx => RuleEnvironment -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> Party -> Signature (Snapshot tx) -> SnapshotNumber -> Workflow tx AckTopology
workflow env ChainPointTime{currentChainTime} pendingDeposits openState otherParty signature sn =
  Workflow.workflow (show . statePhase) NumberCheck step
 where
  step :: State tx from -> Transition tx AckTopology (State tx) from
  step = \case
    NumberCheck -> continue (const Find) $ do
      valid <- Program.decide "acknowledgement number is admissible" (sn `elem` [seenSn, seenSn + 1])
      unless valid $ Program.reject $ RequireFailed AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    Find -> case seenSnapshot of
      LastSeenSnapshot{lastSeen} | sn <= lastSeen -> continue (const Done) $ pure ()
      SeenSnapshot{signatories}
        | seenSn == sn ->
            continue (const $ Collect signatories) $ pure ()
      _ -> continue (const Done) $ Program.suspend WaitOnSeenSnapshot
    Collect signatories -> continue (Collected (Map.insert otherParty signature signatories)) $ do
      -- A retained invalid partial signature still blocks replacement. Recovery
      -- from that case is deferred; aggregate verification remains mandatory.
      fresh <- Program.decide "party has not signed this snapshot" (Map.notMember otherParty signatories)
      unless fresh $ Program.reject $ RequireFailed SnapshotAlreadySigned{knownSignatures = Map.keys signatories, receivedSignature = otherParty}
      let runtime = Coordinated.start openState
      owned <- requireCandidate runtime
      Staging.stage runtime $ Coordinated.recordSignature owned otherParty signature
    Collected allSignatures runtime ->
      if Map.keysSet allSignatures == Set.fromList parties
        then
          let signatures = assembleCertificate allSignatures parties
           in choose $ do
                void $ Program.decide "all parties have signed" True
                owned <- requireCandidate runtime
                verified <- verifyCandidate owned signatures
                case verified of
                  Right signed -> pure $ next $ Confirm signed runtime
                  Left err -> Program.reject err
        else continue (const Done) $ do
          void $ Program.decide "all parties have signed" False
          owned <- requireCandidate runtime
          void $ Staging.stage runtime $ Coordinated.recordSignature owned otherParty signature
    Confirm signed runtime -> continue (const Done) $ do
      confirmedRuntime <- Staging.stage runtime $ Coordinated.confirmSnapshot signed
      let snapshot = verifiedSnapshot $ candidateEvidence signed
          signatures = verifiedSignatures $ candidateEvidence signed
          current = Coordinated.currentState confirmedRuntime
      case (snapshot.depositTxId, snapshot.utxoToCommit) of
        (Just depositTxId, Just _)
          | Just Deposit{deposited} <- Map.lookup depositTxId pendingDeposits -> do
              Program.change CommitApproved{headId, utxoToCommit = deposited}
              Program.emit OnChainEffect{postChainTx = IncrementTx{headSeed, headId, headParameters = parameters, incrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}, depositTxId}}
        _ -> pure ()
      case (current.decommitTx, snapshot.utxoToDecommit) of
        (Just tx, Just utxoToDecommit) -> do
          Program.change DecommitApproved{headId, decommitTxId = txId tx, utxoToDecommit}
          Program.emit OnChainEffect{postChainTx = DecrementTx{headSeed, headId, headParameters = parameters, decrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}}}
        _ -> pure ()
      let nextSn = snapshot.number + 1
      case chainedSnapshotRequest env.snapshotLimits current.version snapshot current.localTxs pendingDeposits current.currentDepositTxId (decommitStatus current.version snapshot current.decommitTx) (settlingCommitAtPeriod env.depositPeriod current.version currentChainTime pendingDeposits snapshot) of
        Just request | isLeader parameters env.party nextSn -> sendSnapshot confirmedRuntime request
        _ -> pure ()
    Done -> finish Done

  OpenState{parameters = parameters@HeadParameters{parties}, coordinatedHeadState, headId, headSeed} = openState
  CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
  seenSn = seenSnapshotNumber seenSnapshot
  requireCandidate :: Coordinated.Runtime tx -> Program.Program tx (Coordinated.Candidate tx)
  requireCandidate = maybe (Program.reject $ AssertionFailed "snapshot owner has no current candidate") pure . Coordinated.candidate

-- | Handle one input through the inspectable workflow.
handle :: forall tx. IsChainState tx => RuleEnvironment -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> Party -> Signature (Snapshot tx) -> SnapshotNumber -> Program tx ()
handle env chainPoint pendingDeposits openState otherParty signature sn = Workflow.run $ workflow env chainPoint pendingDeposits openState otherParty signature sn
