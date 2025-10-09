module Hydra.Chain.SyncedStatus where

import Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..))

data SyncedStatus
  = SyncedStatus
  { point :: Maybe ChainPoint
  , tip :: ChainPoint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary SyncedStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

status :: SyncedStatus -> Bool
status SyncedStatus{point, tip} =
  case point of
    Just p -> p == tip
    Nothing -> False

unSynced :: ChainPoint -> SyncedStatus
unSynced tip = SyncedStatus{point = Nothing, tip}
