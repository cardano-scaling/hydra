module Hydra.HeadLogic.SnapshotOutcome where

import Data.List (elemIndex)
import Hydra.Chain (HeadParameters (..))
import Hydra.Party (Party)
import Hydra.Prelude
import Hydra.Snapshot (SnapshotNumber)

-- * Snapshot helper functions

data SnapshotOutcome tx
  = ShouldSnapshot SnapshotNumber [tx] -- TODO(AB) : should really be a Set (TxId tx)
  | ShouldNotSnapshot NoSnapshotReason
  deriving stock (Eq, Show, Generic)

data NoSnapshotReason
  = NotLeader SnapshotNumber
  | SnapshotInFlight SnapshotNumber
  | NoTransactionsToSnapshot
  deriving stock (Eq, Show, Generic)

-- REVIEW
-- Spec: leader(s+1) = i
isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False
