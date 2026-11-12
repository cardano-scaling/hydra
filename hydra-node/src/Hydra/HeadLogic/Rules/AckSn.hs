{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Collect signatures and confirm a snapshot. Confirmation requires a payload constructed
-- by the successful verification branch; its effects remain an ordinary program.
module Hydra.HeadLogic.Rules.AckSn (Phase (..), AckTopology, machine) where

import Hydra.HeadLogic.Cardano (assembleCertificate)
import Hydra.Prelude hiding (State, round, state)

import Crem.Topology (
  Topology (Topology),
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (LogicError (..), RequirementFailure (..))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.SnapshotRequest (chainedSnapshotRequest, decommitStatus, isLeader, settlingCommitAtPeriod)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedCandidate, candidateEvidence, verifiedSignatures, verifiedSnapshot, verifyCandidate)
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
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
  workflow,
 )
import Hydra.Node.State (ChainPointTime (..), Deposit (..), PendingDeposits)
import Hydra.Tx (txId)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber)

data Phase = CheckNumber | FindSnapshot | CollectSignature | CheckComplete | ConfirmAndContinue | Finished

type AckTopology =
  'Topology
    '[ '( 'CheckNumber, '[ 'FindSnapshot])
     , '( 'FindSnapshot, '[ 'CollectSignature, 'Finished])
     , '( 'CollectSignature, '[ 'CheckComplete])
     , '( 'CheckComplete, '[ 'ConfirmAndContinue, 'Finished])
     , '( 'ConfirmAndContinue, '[ 'Finished])
     ]

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase

data State tx phase where
  NumberCheck :: State tx 'CheckNumber
  Find :: State tx 'FindSnapshot
  Collect :: Map Party (Signature (Snapshot tx)) -> State tx 'CollectSignature
  Collected :: Map Party (Signature (Snapshot tx)) -> Coordinated.Runtime tx -> State tx 'CheckComplete
  Confirm :: VerifiedCandidate tx -> Coordinated.Runtime tx -> State tx 'ConfirmAndContinue
  Done :: State tx 'Finished

statePhase :: State tx phase -> Phase
statePhase = \case
  NumberCheck -> CheckNumber
  Find -> FindSnapshot
  Collect{} -> CollectSignature
  Collected{} -> CheckComplete
  Confirm{} -> ConfirmAndContinue
  Done -> Finished

machine :: forall tx. IsChainState tx => RuleEnvironment -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> Party -> Signature (Snapshot tx) -> SnapshotNumber -> Workflow tx AckTopology
machine env ChainPointTime{currentChainTime} pendingDeposits openState otherParty signature sn =
  workflow (show . statePhase) NumberCheck step
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
      Coordinated.stage runtime $ Coordinated.recordSignature owned otherParty signature
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
          void $ Coordinated.stage runtime $ Coordinated.recordSignature owned otherParty signature
    Confirm signed runtime -> continue (const Done) $ do
      confirmedRuntime <- Coordinated.stage runtime $ Coordinated.confirmSnapshot signed
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
