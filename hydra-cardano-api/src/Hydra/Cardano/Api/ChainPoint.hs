{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

-- * Orphans

-- XXX: Incomplete arbitrary instance
instance Arbitrary ChainPoint where
  arbitrary =
    pure ChainPointAtGenesis
