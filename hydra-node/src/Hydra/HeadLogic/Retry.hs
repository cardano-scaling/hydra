-- | Queue eligibility shared by the node and the bounded development scheduler.
module Hydra.HeadLogic.Retry where

import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (WaitReason (..))
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Prelude

data RetryDecision tx = ParkUntilSynced | RetryInput (Input tx) | DropInput

retryDecision :: WaitReason tx -> Input tx -> RetryDecision tx
retryDecision reason input = case input of
  NetworkInput _ _ | WaitOnNodeInSync{} <- reason -> ParkUntilSynced
  NetworkInput _ ReceivedMessage{msg = ReqSn{}}
    | WaitOnUnresolvedCommit{} <- reason -> RetryInput input
  NetworkInput ttl msg | ttl > 0 -> RetryInput $ NetworkInput (ttl - 1) msg
  _ -> DropInput
