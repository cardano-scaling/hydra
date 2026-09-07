{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.StateEvent where

import Data.EventSource (EventId, HasEventId (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged (Checkpoint))
import Hydra.Node.State (NodeState)
import Hydra.Prelude

-- * State change events as used by Hydra.Node

-- | A state change event with an event id that is the common entity to be
-- loaded from an 'EventSource' and sent to 'EventSink's.
data StateEvent tx = StateEvent
  { eventId :: EventId
  , stateChanged :: StateChanged tx
  , time :: UTCTime
  }
  deriving stock (Generic)

instance HasEventId (StateEvent tx) where
  getEventId = eventId

deriving stock instance IsChainState tx => Show (StateEvent tx)
deriving stock instance IsChainState tx => Eq (StateEvent tx)
deriving anyclass instance IsChainState tx => ToJSON (StateEvent tx)
deriving anyclass instance IsChainState tx => FromJSON (StateEvent tx)

-- NOTE: This codec defines the row format persisted in the hydra.db events
-- table (see "Data.EventSource.SQLite"). Changing it breaks decoding of
-- existing databases and requires a schema migration.
instance IsChainState tx => ToCBOR (StateEvent tx) where
  toCBOR = genericToCBOR

instance IsChainState tx => FromCBOR (StateEvent tx) where
  fromCBOR = genericFromCBOR

mkCheckpoint :: NodeState tx -> EventId -> UTCTime -> StateEvent tx
mkCheckpoint nodeState eventId time =
  StateEvent
    { eventId
    , stateChanged = Checkpoint nodeState
    , time
    }
