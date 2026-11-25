{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.Snapshot.Candidate (Candidate (..), sameCandidate) where

import Hydra.HeadLogic.Snapshot.Cached (CachedSnapshot)
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.Prelude
import Hydra.Tx.Crypto (HydraKey, VerificationKey)
import Hydra.Tx.Snapshot (Snapshot (..))

data Candidate tx = Candidate
  { cachedSnapshot :: CachedSnapshot tx
  , parties :: [VerificationKey HydraKey]
  }

-- | Check whether the candidate snapshot and the expected snapshot agree.
sameCandidate :: Candidate tx -> Candidate tx -> Bool
sameCandidate expected supplied =
  expectedSnapshot.headId == suppliedSnapshot.headId
    && expectedSnapshot.version == suppliedSnapshot.version
    && expectedSnapshot.number == suppliedSnapshot.number
    && expected.parties == supplied.parties
    && Cached.signingBytes expected.cachedSnapshot == Cached.signingBytes supplied.cachedSnapshot
 where
  expectedSnapshot = Cached.snapshot expected.cachedSnapshot
  suppliedSnapshot = Cached.snapshot supplied.cachedSnapshot
