module Hydra.HeadLogic.Snapshot.Verification (
  VerifiedSnapshot,
  VerifiedCandidate,
  verifiedSnapshot,
  verifiedSignatures,
  verifiedFor,
  verifiedCandidate,
  candidateEvidence,
  verifyCandidate,
  verifySeenSnapshot,
  verifySignedSnapshot,
) where

import Hydra.HeadLogic.Cardano (snapshotSigningBytes)
import Hydra.HeadLogic.Error (
  LogicError (RequireFailed),
  RequirementFailure (InvalidMultisignature),
 )
import Hydra.HeadLogic.Outcome (WaitReason (WaitOnSeenSnapshot))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  suspend,
  verifySnapshot,
 )
import Hydra.HeadLogic.Snapshot.Candidate (Candidate (..))
import Hydra.HeadLogic.State (
  SeenSnapshot (SeenSnapshot, snapshot),
 )
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx)
import Hydra.Tx.Crypto (
  HydraKey,
  MultiSignature,
  VerificationError (..),
  VerificationKey,
 )
import Hydra.Tx.Snapshot (Snapshot (Snapshot, headId))

data VerifiedSnapshot tx = VerifiedSnapshot (Snapshot tx) (MultiSignature (Snapshot tx)) [VerificationKey HydraKey]

data VerifiedCandidate tx = VerifiedCandidate (Candidate tx) (VerifiedSnapshot tx)

verifiedCandidate :: VerifiedCandidate tx -> Candidate tx
verifiedCandidate (VerifiedCandidate candidate _) = candidate

candidateEvidence :: VerifiedCandidate tx -> VerifiedSnapshot tx
candidateEvidence (VerifiedCandidate _ evidence) = evidence

verifiedSnapshot :: VerifiedSnapshot tx -> Snapshot tx
verifiedSnapshot (VerifiedSnapshot snapshot _ _) = snapshot

verifiedSignatures :: VerifiedSnapshot tx -> MultiSignature (Snapshot tx)
verifiedSignatures (VerifiedSnapshot _ signatures _) = signatures

verifiedFor ::
  HeadId ->
  [VerificationKey HydraKey] ->
  VerifiedSnapshot tx ->
  Bool
verifiedFor expectedHead expectedKeys (VerifiedSnapshot Snapshot{headId} _ verifiedKeys) =
  headId == expectedHead && verifiedKeys == expectedKeys

verifySignedSnapshot ::
  IsTx tx =>
  [VerificationKey HydraKey] ->
  Snapshot tx ->
  MultiSignature (Snapshot tx) ->
  Program tx (Either VerificationError (VerifiedSnapshot tx))
verifySignedSnapshot vkeys snapshot = verify vkeys snapshot (snapshotSigningBytes snapshot)

verify ::
  [VerificationKey HydraKey] ->
  Snapshot tx ->
  ByteString ->
  MultiSignature (Snapshot tx) ->
  Program tx (Either VerificationError (VerifiedSnapshot tx))
verify vkeys snapshot bytes signatures = do
  result <- Program.verifySnapshot vkeys signatures bytes
  pure $ VerifiedSnapshot snapshot signatures vkeys <$ result

verifyCandidate ::
  Candidate tx ->
  MultiSignature (Snapshot tx) ->
  Program tx (Either (LogicError tx) (VerifiedCandidate tx))
verifyCandidate candidate@Candidate{snapshot, parties, representation} signatures = do
  result <- verify parties snapshot representation signatures
  pure $ VerifiedCandidate candidate <$> first (verificationError parties signatures) result

verifySeenSnapshot ::
  IsTx tx =>
  [VerificationKey HydraKey] ->
  SeenSnapshot tx ->
  MultiSignature (Snapshot tx) ->
  Program tx (Either (LogicError tx) (VerifiedSnapshot tx))
verifySeenSnapshot parties seen signatures = case seen of
  SeenSnapshot{snapshot} -> first (verificationError parties signatures) <$> verifySignedSnapshot parties snapshot signatures
  _ -> Program.suspend WaitOnSeenSnapshot

verificationError ::
  [VerificationKey HydraKey] ->
  MultiSignature (Snapshot tx) ->
  VerificationError ->
  LogicError tx
verificationError parties signatures failure =
  RequireFailed $ InvalidMultisignature (show signatures) $ case failure of
    InvalidSignatures failures -> toList failures
    SignatureCountMismatch -> parties
