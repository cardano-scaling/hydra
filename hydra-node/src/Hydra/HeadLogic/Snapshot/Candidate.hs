{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.Snapshot.Candidate (Candidate (..), sameCandidate) where

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
