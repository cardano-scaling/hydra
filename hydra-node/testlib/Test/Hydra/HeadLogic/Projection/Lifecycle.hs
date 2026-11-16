{-# LANGUAGE OverloadedRecordDot #-}

-- | Project the protocol phase from stored state. Readiness and pending local
-- requests are fields within Closed; only observed distribution is PartialFanout.
module Test.Hydra.HeadLogic.Projection.Lifecycle where

import Hydra.HeadLogic.Fanout.Outputs (nullOutputs)
import Hydra.HeadLogic.State qualified as Head
import Hydra.Prelude
import Hydra.Tx (IsTx)

data Phase = Idle | Open | Closed | FanoutProgress | Finalized
  deriving stock (Eq, Show)

phase :: IsTx tx => Head.HeadState tx -> Phase
phase = \case
  Head.Idle{} -> Idle
  Head.Open{} -> Open
  Head.Closed{} -> Closed
  Head.FanoutProgress st -> if nullOutputs st.distributedOutputs then Closed else FanoutProgress
  Head.Finalized{} -> Finalized
