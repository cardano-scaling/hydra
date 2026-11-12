{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Live ownership transitions. Runtime machines, inspection and compile-fail
-- checks import these definitions; trusted replay does not widen their edges.
module Hydra.HeadLogic.Machine.Coordinated.Topology (
  SnapshotPhase (..),
  SnapshotTopology,
  snapshotTopology,
  sSnapshotTopology,
  TransactionPhase (..),
  TransactionTopology,
  transactionTopology,
  sTransactionTopology,
) where

import Hydra.Prelude

import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (STopology (STopology), Topology (Topology), TopologySym0)
import Data.Singletons.Base.TH (NilSym0, SList (SCons, SNil), STuple2 (STuple2), Tuple2Sym0, singletons, type (:@#@$))

$( singletons
    [d|
      data SnapshotPhase = AwaitingRequest | RequestSent | CollectingSignatures

      snapshotTopology :: Topology SnapshotPhase
      snapshotTopology =
        Topology
          [ (AwaitingRequest, [RequestSent, CollectingSignatures])
          , (RequestSent, [AwaitingRequest, CollectingSignatures])
          , (CollectingSignatures, [AwaitingRequest])
          ]

      data TransactionPhase = NoDecommit | DecommitPending | AwaitingSettlement

      transactionTopology :: Topology TransactionPhase
      transactionTopology =
        Topology
          [ (NoDecommit, [DecommitPending, AwaitingSettlement])
          , (DecommitPending, [NoDecommit, AwaitingSettlement])
          , (AwaitingSettlement, [NoDecommit])
          ]
      |]
 )

deriving stock instance Eq SnapshotPhase
deriving stock instance Show SnapshotPhase
deriving stock instance Enum SnapshotPhase
deriving stock instance Bounded SnapshotPhase
deriving via AllVertices SnapshotPhase instance RenderableVertices SnapshotPhase

deriving stock instance Eq TransactionPhase
deriving stock instance Show TransactionPhase
deriving stock instance Enum TransactionPhase
deriving stock instance Bounded TransactionPhase
deriving via AllVertices TransactionPhase instance RenderableVertices TransactionPhase
