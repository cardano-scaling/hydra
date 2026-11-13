-- | Evidence for live confirmation. Only the successful primitive reply creates
-- this value; replay has a separate trusted path for already retained events.
module Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot, VerifiedCandidate, verifiedSnapshot, verifiedSignatures, verifiedFor, verifiedCandidate, candidateEvidence, verifyCandidate, verifySeenSnapshot, verifySignedSnapshot) where

import Hydra.HeadLogic.Cardano (snapshotSigningBytes)
import Hydra.Prelude

import Hydra.HeadLogic.Error (LogicError (RequireFailed), RequirementFailure (InvalidMultisignature))
import Hydra.HeadLogic.Machine.Coordinated.Candidate (Candidate (..))
import Hydra.HeadLogic.Outcome (WaitReason (WaitOnSeenSnapshot))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.State (SeenSnapshot (SeenSnapshot, snapshot))
import Hydra.Tx (HeadId, IsTx)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, VerificationKey, Verified (..))
import Hydra.Tx.Snapshot (Snapshot (Snapshot, headId))

data VerifiedSnapshot tx = VerifiedSnapshot (Snapshot tx) (MultiSignature (Snapshot tx)) [VerificationKey HydraKey]

-- A generally valid certificate is insufficient to confirm the current round.
-- Confirmation also carries the handle issued by that round's owner.
data VerifiedCandidate tx = VerifiedCandidate (Candidate tx) (VerifiedSnapshot tx)

verifiedCandidate :: VerifiedCandidate tx -> Candidate tx
verifiedCandidate (VerifiedCandidate candidate _) = candidate

candidateEvidence :: VerifiedCandidate tx -> VerifiedSnapshot tx
candidateEvidence (VerifiedCandidate _ evidence) = evidence

verifiedSnapshot :: VerifiedSnapshot tx -> Snapshot tx
verifiedSnapshot (VerifiedSnapshot snapshot _ _) = snapshot

verifiedSignatures :: VerifiedSnapshot tx -> MultiSignature (Snapshot tx)
verifiedSignatures (VerifiedSnapshot _ signatures _) = signatures

-- | Verification authorises one Head and its ordered parties, not just bytes.
verifiedFor :: HeadId -> [VerificationKey HydraKey] -> VerifiedSnapshot tx -> Bool
verifiedFor expectedHead expectedKeys (VerifiedSnapshot Snapshot{headId} _ verifiedKeys) =
  headId == expectedHead && verifiedKeys == expectedKeys

verifySignedSnapshot :: IsTx tx => [VerificationKey HydraKey] -> Snapshot tx -> MultiSignature (Snapshot tx) -> Program tx (Either Verified (VerifiedSnapshot tx))
verifySignedSnapshot vkeys snapshot = verify vkeys snapshot (snapshotSigningBytes snapshot)

verify :: [VerificationKey HydraKey] -> Snapshot tx -> ByteString -> MultiSignature (Snapshot tx) -> Program tx (Either Verified (VerifiedSnapshot tx))
verify vkeys snapshot bytes signatures = do
  result <- Program.verifySnapshot vkeys signatures bytes
  pure $ case result of
    Verified -> Right $ VerifiedSnapshot snapshot signatures vkeys
    failure -> Left failure

verifyCandidate :: Candidate tx -> MultiSignature (Snapshot tx) -> Program tx (Either (LogicError tx) (VerifiedCandidate tx))
verifyCandidate candidate@Candidate{snapshot, parties, representation} signatures = do
  result <- verify parties snapshot representation signatures
  pure $ VerifiedCandidate candidate <$> first (verificationError parties signatures) result

-- | Chain adoption verifies snapshot contents without issuing a live handle.
verifySeenSnapshot :: IsTx tx => [VerificationKey HydraKey] -> SeenSnapshot tx -> MultiSignature (Snapshot tx) -> Program tx (Either (LogicError tx) (VerifiedSnapshot tx))
verifySeenSnapshot parties seen signatures = case seen of
  SeenSnapshot{snapshot} -> first (verificationError parties signatures) <$> verifySignedSnapshot parties snapshot signatures
  _ -> Program.suspend WaitOnSeenSnapshot

verificationError :: [VerificationKey HydraKey] -> MultiSignature (Snapshot tx) -> Verified -> LogicError tx
verificationError parties signatures failure =
  RequireFailed $ InvalidMultisignature (show signatures) $ case failure of
    FailedKeys failures -> failures
    _ -> parties
