{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Machine.Coordinated.Topology (
  SnapshotPhase (..),
  SnapshotTopology,
  snapshotTopology,
  sSnapshotTopology,
  DecommitPhase (..),
  DecommitTopology,
  decommitTopology,
  sDecommitTopology,
  CommitPhase (..),
  CommitTopology,
  commitTopology,
  sCommitTopology,
) where

import Hydra.Prelude

import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (STopology (STopology), Topology (Topology), TopologySym0)
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )

$( singletons
    [d|
      -- \| A snapshot can only be awaiting for a snapshot, requesting a
      -- snapshot, or collecting signatures.
      data SnapshotPhase = AwaitingRequest | RequestSent | CollectingSignatures

      snapshotTopology :: Topology SnapshotPhase
      snapshotTopology =
        Topology
          [ (AwaitingRequest, [RequestSent, CollectingSignatures])
          , (RequestSent, [AwaitingRequest, CollectingSignatures])
          , (CollectingSignatures, [AwaitingRequest])
          ]

      -- \| Local reservation and current-version authority to decrement are
      -- distinct: a certificate can arrive without the local transaction.
      data DecommitPhase = NoDecommit | DecommitPending | AwaitingDecommitSettlement

      decommitTopology :: Topology DecommitPhase
      decommitTopology =
        Topology
          [ (NoDecommit, [DecommitPending, AwaitingDecommitSettlement])
          , (DecommitPending, [NoDecommit, AwaitingDecommitSettlement])
          , (AwaitingDecommitSettlement, [NoDecommit])
          ]

      -- \| Selection is local intent; a current-version signed claim awaits
      -- settlement. Expiry and recovery affect eligibility, not the certificate.
      data CommitPhase = NoCommit | CommitSelected | AwaitingCommitSettlement

      commitTopology :: Topology CommitPhase
      commitTopology =
        Topology
          [ (NoCommit, [CommitSelected, AwaitingCommitSettlement])
          , (CommitSelected, [NoCommit, AwaitingCommitSettlement])
          , -- A replaced certificate or newer version can leave a local selection.
            (AwaitingCommitSettlement, [NoCommit, CommitSelected])
          ]
      |]
 )

deriving stock instance Eq SnapshotPhase
deriving stock instance Show SnapshotPhase
deriving stock instance Enum SnapshotPhase
deriving stock instance Bounded SnapshotPhase
deriving via AllVertices SnapshotPhase instance RenderableVertices SnapshotPhase

deriving stock instance Eq DecommitPhase
deriving stock instance Show DecommitPhase
deriving stock instance Enum DecommitPhase
deriving stock instance Bounded DecommitPhase
deriving via AllVertices DecommitPhase instance RenderableVertices DecommitPhase

deriving stock instance Eq CommitPhase
deriving stock instance Show CommitPhase
deriving stock instance Enum CommitPhase
deriving stock instance Bounded CommitPhase
deriving via AllVertices CommitPhase instance RenderableVertices CommitPhase
