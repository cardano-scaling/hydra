{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Collect signatures and confirm a snapshot. Confirmation requires a payload constructed
-- by the successful verification branch; its effects remain an ordinary program.
module Hydra.HeadLogic.Rules.AckSn (Phase (..), AckTopology, ackTopology, sAckTopology, machine) where

import Hydra.Prelude hiding (State, round, state)

import Crem.BaseMachine (BaseMachineT (..), InitialState (..))
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.Chain (PostChainTx (..))
import Hydra.HeadLogic.Error (LogicError (..), RequirementFailure (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.SnapshotRequest (SnapshotRequest (..), chainedSnapshotRequest, isLeader, settlingCommitAtPeriod)
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
    signableBytes,
    signatories,
    snapshot
  ),
  seenSnapshotNumber,
 )
import Hydra.HeadLogic.State.Operations (settledUTxO)
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), PendingDeposits)
import Hydra.Tx (IsTx, txId)
import Hydra.Tx.Crypto (MultiSignature, Signature, Verified (..), aggregateInOrder)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber)

$( singletons
    [d|
      data Phase = CheckNumber | FindSnapshot | CollectSignature | CheckComplete | CheckVerification | ConfirmAndContinue | Finished

      ackTopology :: Topology Phase
      ackTopology =
        Topology
          [ (CheckNumber, [FindSnapshot])
          , (FindSnapshot, [CollectSignature, Finished])
          , (CollectSignature, [CheckComplete])
          , (CheckComplete, [CheckVerification, Finished])
          , (CheckVerification, [ConfirmAndContinue, Finished])
          , (ConfirmAndContinue, [Finished])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data Candidate tx = Candidate (Snapshot tx) (MultiSignature (Snapshot tx))

-- This constructor is private to the rule. The topology alone cannot prove
-- that the signatures are valid or that a supplied primitive reply is honest.
data VerifiedRound tx = VerifiedRound (Snapshot tx) (MultiSignature (Snapshot tx))

data State tx phase where
  NumberCheck :: State tx 'CheckNumber
  Find :: State tx 'FindSnapshot
  Collect :: Snapshot tx -> Map Party (Signature (Snapshot tx)) -> ~ByteString -> State tx 'CollectSignature
  Collected :: Snapshot tx -> Map Party (Signature (Snapshot tx)) -> ~ByteString -> State tx 'CheckComplete
  VerificationResult :: Candidate tx -> Verified -> State tx 'CheckVerification
  Confirm :: VerifiedRound tx -> State tx 'ConfirmAndContinue
  Done :: State tx 'Finished

machine :: forall tx ledger signing. IsTx tx => RuleEnvironment -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> Party -> Signature (Snapshot tx) -> SnapshotNumber -> Workflow tx ledger signing AckTopology
machine env ChainPointTime{currentChainTime} pendingDeposits openState otherParty signature sn =
  BaseMachineT (InitialState NumberCheck) $ \state () -> step state
 where
  step :: State tx from -> Transition tx ledger signing AckTopology (State tx) from
  step = \case
    NumberCheck -> continue (const Find) $ do
      valid <- Program.decide "acknowledgement number is admissible" (sn `elem` [seenSn, seenSn + 1])
      unless valid $ Program.reject $ RequireFailed AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    Find -> case seenSnapshot of
      LastSeenSnapshot{lastSeen} | sn <= lastSeen -> continue (const Done) $ pure ()
      SeenSnapshot{snapshot, signatories, signableBytes}
        | seenSn == sn ->
            continue (const $ Collect snapshot signatories signableBytes) $ pure ()
      _ -> continue (const Done) $ Program.suspend WaitOnSeenSnapshot
    Collect snapshot signatories bytes -> continue (const $ Collected snapshot (Map.insert otherParty signature signatories) bytes) $ do
      fresh <- Program.decide "party has not signed this snapshot" (Map.notMember otherParty signatories)
      unless fresh $ Program.reject $ RequireFailed SnapshotAlreadySigned{knownSignatures = Map.keys signatories, receivedSignature = otherParty}
      Program.change $ signed snapshot
    Collected snapshot allSignatures bytes ->
      if Map.keysSet allSignatures == Set.fromList parties
        then
          let signatures = aggregateInOrder allSignatures parties
           in continue (VerificationResult $ Candidate snapshot signatures) $ do
                void $ Program.decide "all parties have signed" True
                Program.verifySnapshot vkeys signatures bytes
        else continue (const Done) $ do
          void $ Program.decide "all parties have signed" False
          -- The existing partial-ack path records this event twice.
          Program.change $ signed snapshot
    VerificationResult (Candidate snapshot signatures) verified -> case verified of
      Verified -> continue (const $ Confirm $ VerifiedRound snapshot signatures) $ pure ()
      FailedKeys failures -> continue (const Done) $ Program.reject $ RequireFailed InvalidMultisignature{multisig = show signatures, vkeys = failures}
      KeyNumberMismatch -> continue (const Done) $ Program.reject $ RequireFailed InvalidMultisignature{multisig = show signatures, vkeys}
    Confirm (VerifiedRound snapshot signatures) -> continue (const Done) $ do
      Program.change SnapshotConfirmed{headId, snapshot = Nothing, signatures}
      case (snapshot.depositTxId, snapshot.utxoToCommit) of
        (Just depositTxId, Just _)
          | Just Deposit{deposited} <- Map.lookup depositTxId pendingDeposits -> do
              Program.change CommitApproved{headId, utxoToCommit = deposited}
              Program.emit OnChainEffect{postChainTx = IncrementTx{headSeed, headId, headParameters = parameters, incrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}, depositTxId}}
        _ -> pure ()
      case (decommitTx, snapshot.utxoToDecommit) of
        (Just tx, Just utxoToDecommit) -> do
          Program.change DecommitApproved{headId, decommitTxId = txId tx, utxoToDecommit}
          Program.emit OnChainEffect{postChainTx = DecrementTx{headSeed, headId, headParameters = parameters, decrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}}}
        _ -> pure ()
      let nextSn = snapshot.number + 1
      case chainedSnapshotRequest (settledUTxO version snapshot) localTxs pendingDeposits currentDepositTxId decommitTx (settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits snapshot) of
        Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} | isLeader parameters env.party nextSn -> do
          Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
          Program.emit $ NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId
        _ -> pure ()
    Done -> finish Done

  signed :: Snapshot tx -> StateChanged tx
  signed snapshot = PartySignedSnapshot{snapshotNumber = snapshot.number, party = otherParty, signature}
  OpenState{parameters = parameters@HeadParameters{parties}, coordinatedHeadState, headId, headSeed} = openState
  CoordinatedHeadState{seenSnapshot, localTxs, decommitTx, currentDepositTxId, version} = coordinatedHeadState
  seenSn = seenSnapshotNumber seenSnapshot
  vkeys = vkey <$> parties
