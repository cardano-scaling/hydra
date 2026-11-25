{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.State.Coordinated.Snapshots.Topology where

import Crem.Topology (
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.Prelude

$( singletons
    [d|
      data SnapshotPhase
        = AwaitingRequest
        | RequestSent
        | CollectingSignatures

      snapshotTopology :: Topology SnapshotPhase
      snapshotTopology =
        Topology
          [ (AwaitingRequest, [RequestSent, CollectingSignatures])
          , (RequestSent, [AwaitingRequest, CollectingSignatures])
          , (CollectingSignatures, [AwaitingRequest])
          ]
      |]
 )

deriving stock instance Eq SnapshotPhase
deriving stock instance Show SnapshotPhase
deriving stock instance Enum SnapshotPhase
deriving stock instance Bounded SnapshotPhase

-- | These edges include administrative recovery, which can discard a candidate.
transitionLabels :: [((SnapshotPhase, SnapshotPhase), String)]
transitionLabels =
  [ ((AwaitingRequest, RequestSent), "Request")
  , ((AwaitingRequest, CollectingSignatures), "Accept candidate")
  , ((RequestSent, CollectingSignatures), "Accept candidate")
  , ((RequestSent, AwaitingRequest), "Settlement / adoption / administrative reset")
  , ((CollectingSignatures, AwaitingRequest), "Confirmation / adoption / administrative reset")
  ]
