module Hydra.Cardano.Api where

import Hydra.Prelude

data ByronAddr

type Address a = Text
type PolicyId = Text
type PolicyAssets = Text
newtype Coin = Coin Integer
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
