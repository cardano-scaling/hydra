module CertificateAsCandidate where

import Hydra.HeadLogic.Machine.Coordinated (Command, confirmSnapshot)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot)

-- A valid certificate may be adopted, but is not a current candidate handle.
confirm :: VerifiedSnapshot tx -> Command tx
confirm = confirmSnapshot
