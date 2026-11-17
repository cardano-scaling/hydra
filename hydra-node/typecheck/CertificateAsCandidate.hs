module CertificateAsCandidate where

import Hydra.HeadLogic.Snapshot.Verification (VerifiedSnapshot)
import Hydra.HeadLogic.State.Coordinated (Command, confirmSnapshot)

-- A valid certificate may be adopted, but is not a current candidate handle.
confirm :: VerifiedSnapshot tx -> Command tx
confirm = confirmSnapshot
