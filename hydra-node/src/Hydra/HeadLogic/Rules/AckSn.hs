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
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (LogicError (..), RequirementFailure (..))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.SnapshotRequest (chainedSnapshotRequest, decommitStatus, isLeader, settlingCommitAtPeriod)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot, verifiedSignatures, verifiedSnapshot, verifyCandidate)
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
    signatories,
    snapshot
  ),
  seenSnapshotNumber,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
 )
import Hydra.Node.State (ChainPointTime (..), Deposit (..), PendingDeposits)
import Hydra.Tx (txId)
import Hydra.Tx.Crypto (Signature, aggregateInOrder)
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

data State tx phase where
  NumberCheck :: State tx 'CheckNumber
  Find :: State tx 'FindSnapshot
  Collect :: Snapshot tx -> Map Party (Signature (Snapshot tx)) -> State tx 'CollectSignature
  Collected :: Snapshot tx -> Map Party (Signature (Snapshot tx)) -> Coordinated.Runtime tx -> State tx 'CheckComplete
  VerificationResult :: Coordinated.Runtime tx -> Either (LogicError tx) (VerifiedSnapshot tx) -> State tx 'CheckVerification
  Confirm :: VerifiedSnapshot tx -> Coordinated.Runtime tx -> State tx 'ConfirmAndContinue
  Done :: State tx 'Finished

machine :: forall tx ledger signing. IsChainState tx => RuleEnvironment -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> Party -> Signature (Snapshot tx) -> SnapshotNumber -> Workflow tx ledger signing AckTopology
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
      SeenSnapshot{snapshot, signatories}
        | seenSn == sn ->
            continue (const $ Collect snapshot signatories) $ pure ()
      _ -> continue (const Done) $ Program.suspend WaitOnSeenSnapshot
    Collect snapshot signatories -> continue (Collected snapshot (Map.insert otherParty signature signatories)) $ do
      -- A retained invalid partial signature still blocks replacement. Recovery
      -- from that case is deferred; aggregate verification remains mandatory.
      fresh <- Program.decide "party has not signed this snapshot" (Map.notMember otherParty signatories)
      unless fresh $ Program.reject $ RequireFailed SnapshotAlreadySigned{knownSignatures = Map.keys signatories, receivedSignature = otherParty}
      Coordinated.stage (Coordinated.start openState) $ Coordinated.recordSignature snapshot.number otherParty signature
    Collected snapshot allSignatures runtime ->
      if Map.keysSet allSignatures == Set.fromList parties
        then
          let signatures = aggregateInOrder allSignatures parties
           in continue (VerificationResult runtime) $ do
                void $ Program.decide "all parties have signed" True
                verifyCandidate vkeys (Coordinated.currentState runtime).seenSnapshot signatures
        else continue (const Done) $ do
          void $ Program.decide "all parties have signed" False
          -- The existing partial-ack path records this event twice.
          void $ Coordinated.stage runtime $ Coordinated.recordSignature snapshot.number otherParty signature
    VerificationResult runtime verified -> case verified of
      Right signed -> continue (const $ Confirm signed runtime) $ pure ()
      Left err -> continue (const Done) $ Program.reject err
    Confirm signed runtime -> continue (const Done) $ do
      confirmedRuntime <- Coordinated.stage runtime $ Coordinated.confirmSnapshot signed
      let snapshot = verifiedSnapshot signed
          signatures = verifiedSignatures signed
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
      case chainedSnapshotRequest version snapshot localTxs pendingDeposits currentDepositTxId (decommitStatus version snapshot decommitTx) (settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits snapshot) of
        Just request | isLeader parameters env.party nextSn -> sendSnapshot confirmedRuntime request
        _ -> pure ()
    Done -> finish Done

  OpenState{parameters = parameters@HeadParameters{parties}, coordinatedHeadState, headId, headSeed} = openState
  CoordinatedHeadState{seenSnapshot, localTxs, decommitTx, currentDepositTxId, version} = coordinatedHeadState
  seenSn = seenSnapshotNumber seenSnapshot
  vkeys = vkey <$> parties
