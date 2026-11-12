-- | Check every retained prefix, including the state from which execution resumes.
module Test.Hydra.HeadLogic.Recovery (recover) where

import Hydra.Prelude hiding (state)

import Cardano.Binary (decodeFull', serialize')
import Hydra.HeadLogic (Outcome (..), StateChanged, aggregateNodeState, aggregateState)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node.State (NodeState)

recover :: NodeState SimpleTx -> Outcome SimpleTx -> Either Text (NodeState SimpleTx)
recover before outcome = do
  restored <- foldlM event before $ case outcome of
    Continue events _ -> events
    Wait _ events -> events
    Error{} -> []
  unless (restored == aggregateState before outcome) $ Left "recovery differs from live aggregation"
  pure restored
 where
  event :: NodeState SimpleTx -> StateChanged SimpleTx -> Either Text (NodeState SimpleTx)
  event state change = do
    decoded <- first show $ decodeFull' $ serialize' change
    unless (decoded == change) $ Left "event CBOR round trip differs"
    let next = aggregateNodeState state decoded
    checkpoint <- first show $ decodeFull' $ serialize' next
    unless (checkpoint == next) $ Left "checkpoint CBOR round trip differs"
    pure checkpoint
