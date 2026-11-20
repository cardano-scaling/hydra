{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.State.Fanout where

import Hydra.Chain.ChainState (IsChainState (ChainStateType))
import Hydra.HeadLogic.State (
  ClosedState (..),
  FanoutMode,
  HeadState (Closed, FanoutProgress),
  OnChainFanoutDatum (..),
  PartialFanoutState (..),
 )
import Hydra.Prelude
import Hydra.Tx (UTxOType)

closedToPartialFanout :: ClosedState tx -> ChainStateType tx -> UTxOType tx -> UTxOType tx -> FanoutMode tx -> OnChainFanoutDatum -> PartialFanoutState tx
closedToPartialFanout closedState chainState remaining distributed mode fanoutDatum =
  PartialFanoutState
    { parameters
    , confirmedSnapshot
    , contestationDeadline
    , chainState
    , headId
    , headSeed
    , version
    , remainingOutputs = remaining
    , distributedOutputs = distributed
    , mode
    , fanoutDatum
    , readyToFanoutSent
    , unconfirmedSnapshot
    }
 where
  ClosedState{parameters, confirmedSnapshot, contestationDeadline, headId, headSeed, version, readyToFanoutSent, unconfirmedSnapshot} = closedState

-- | Rebuild the 'ClosedState' from a 'PartialFanoutState' when clearing a
-- local fanout request (see 'HeadFanoutReverted'). The Head stays Closed.
-- The head owner only applies this to pending fanout, before any distribution.
-- The candidate and notification state remain available before the deadline,
-- including after a checkpoint or a failed early request.
fanoutProgressToClosed :: PartialFanoutState tx -> ClosedState tx
fanoutProgressToClosed pfs =
  ClosedState
    { parameters
    , confirmedSnapshot
    , contestationDeadline
    , readyToFanoutSent
    , chainState
    , headId
    , headSeed
    , version
    , unconfirmedSnapshot
    }
 where
  PartialFanoutState{parameters, confirmedSnapshot, contestationDeadline, chainState, headId, headSeed, version, readyToFanoutSent, unconfirmedSnapshot} = pfs

-- | Local fanout intent does not stop the on-chain Head being Closed.
getClosedState :: HeadState tx -> Maybe (ClosedState tx)
getClosedState = \case
  Closed st -> Just st
  FanoutProgress st@PartialFanoutState{fanoutDatum = DatumClosed} -> Just $ fanoutProgressToClosed st
  _ -> Nothing
