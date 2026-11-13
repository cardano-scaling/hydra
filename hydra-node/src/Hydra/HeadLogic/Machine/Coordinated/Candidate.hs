{-# LANGUAGE OverloadedRecordDot #-}

-- | Internal candidate identity. Only the snapshot owner issues handles; public
-- callers receive the abstract type through Coordinated.
module Hydra.HeadLogic.Machine.Coordinated.Candidate (Candidate (..), sameCandidate) where

import Hydra.Prelude

import Hydra.Tx.Crypto (HydraKey, VerificationKey)
import Hydra.Tx.Snapshot (Snapshot (..))

data Candidate tx = Candidate
  { snapshot :: Snapshot tx
  , parties :: [VerificationKey HydraKey]
  , representation :: ~ByteString
  }

-- | Check whether the candidate snapshot and the expected snapshot agree.
sameCandidate :: Candidate tx -> Candidate tx -> Bool
sameCandidate expected supplied =
  expected.snapshot.headId == supplied.snapshot.headId
    && expected.snapshot.version == supplied.snapshot.version
    && expected.snapshot.number == supplied.snapshot.number
    && expected.parties == supplied.parties
    && expected.representation == supplied.representation
