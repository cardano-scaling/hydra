{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.StateEvent where

import Hydra.Chain.ChainState (ChainStateType, IsChainState)
import Hydra.Events (EventId, HasEventId (..))
import Hydra.HeadLogic.Outcome (StateChanged (Checkpoint))
import Hydra.Node.State (NodeState)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (ArbitraryIsTx)

-- * State change events as used by Hydra.Node

-- | A state change event with an event id that is the common entity to be
-- loaded from an 'EventSource' and sent to 'EventSink's.
data StateEvent tx = StateEvent
  { eventId :: EventId
  , stateChanged :: StateChanged tx
  , time :: UTCTime
  }
  deriving (Generic)

instance HasEventId (StateEvent tx) where
  getEventId = eventId

deriving instance IsChainState tx => Show (StateEvent tx)
deriving instance IsChainState tx => Eq (StateEvent tx)
deriving instance IsChainState tx => ToJSON (StateEvent tx)
deriving instance IsChainState tx => FromJSON (StateEvent tx)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx), IsChainState tx) => Arbitrary (StateEvent tx) where
  arbitrary = arbitrary >>= genStateEvent
  shrink = genericShrink

genStateEvent :: StateChanged tx -> Gen (StateEvent tx)
genStateEvent sc = StateEvent <$> arbitrary <*> pure sc <*> arbitrary

mkCheckpoint :: NodeState tx -> EventId -> UTCTime -> StateEvent tx
mkCheckpoint nodeState eventId time =
  StateEvent
    { eventId
    , stateChanged = Checkpoint nodeState
    , time
    }
