-- | Evidence for live confirmation. Only the successful primitive reply creates
-- this value; replay has a separate trusted path for already retained events.
module Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot, verifiedSnapshot, verifiedSignatures, verifiedRepresentation, verifyCandidate, verifySignedSnapshot) where

import Hydra.Prelude

import Hydra.HeadLogic.Error (LogicError (RequireFailed), RequirementFailure (InvalidMultisignature))
import Hydra.HeadLogic.Outcome (WaitReason (WaitOnSeenSnapshot))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.State (SeenSnapshot (..))
import Hydra.Tx (IsTx)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, VerificationKey, Verified (..), getSignableRepresentation)
import Hydra.Tx.Snapshot (Snapshot)

data VerifiedSnapshot tx = VerifiedSnapshot (Snapshot tx) (MultiSignature (Snapshot tx)) ByteString

verifiedSnapshot :: VerifiedSnapshot tx -> Snapshot tx
verifiedSnapshot (VerifiedSnapshot snapshot _ _) = snapshot

verifiedSignatures :: VerifiedSnapshot tx -> MultiSignature (Snapshot tx)
verifiedSignatures (VerifiedSnapshot _ signatures _) = signatures

verifiedRepresentation :: VerifiedSnapshot tx -> ByteString
verifiedRepresentation (VerifiedSnapshot _ _ bytes) = bytes

verifySignedSnapshot :: IsTx tx => [VerificationKey HydraKey] -> Snapshot tx -> MultiSignature (Snapshot tx) -> Program tx ledger signing (Either Verified (VerifiedSnapshot tx))
verifySignedSnapshot vkeys snapshot = verify vkeys snapshot (getSignableRepresentation snapshot)

verify :: [VerificationKey HydraKey] -> Snapshot tx -> ByteString -> MultiSignature (Snapshot tx) -> Program tx ledger signing (Either Verified (VerifiedSnapshot tx))
verify vkeys snapshot bytes signatures = do
  result <- Program.verifySnapshot vkeys signatures bytes
  pure $ case result of
    Verified -> Right $ VerifiedSnapshot snapshot signatures bytes
    failure -> Left failure

verifyCandidate :: [VerificationKey HydraKey] -> SeenSnapshot tx -> MultiSignature (Snapshot tx) -> Program tx ledger signing (Either (LogicError tx) (VerifiedSnapshot tx))
verifyCandidate vkeys candidate signatures = case candidate of
  SeenSnapshot{snapshot, signableBytes} -> do
    result <- verify vkeys snapshot signableBytes signatures
    pure $ case result of
      Right evidence -> Right evidence
      Left (FailedKeys failures) -> Left $ RequireFailed $ InvalidMultisignature (show signatures) failures
      Left _ -> Left $ RequireFailed $ InvalidMultisignature (show signatures) vkeys
  _ -> Program.suspend WaitOnSeenSnapshot
