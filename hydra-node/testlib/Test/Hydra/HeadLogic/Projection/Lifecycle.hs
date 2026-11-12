{-# LANGUAGE OverloadedRecordDot #-}

-- | Refine the stored head state with readiness and distribution observations.
-- These data projections refine the five-phase Crem owner with readiness.
module Test.Hydra.HeadLogic.Projection.Lifecycle where

import Hydra.Prelude
import Hydra.HeadLogic.Fanout.Outputs (nullOutputs)
import Hydra.HeadLogic.State qualified as Head
import Hydra.Tx (IsTx)

data Phase = Idle | Open | Closed | ReadyToFanout | PendingFanout | FanoutProgress
  deriving stock (Eq, Show)

phase :: IsTx tx => Head.HeadState tx -> Phase
phase = \case
  Head.Idle{} -> Idle
  Head.Open{} -> Open
  Head.Closed st -> if st.readyToFanoutSent then ReadyToFanout else Closed
  Head.FanoutProgress st -> if nullOutputs st.distributedOutputs then PendingFanout else FanoutProgress
