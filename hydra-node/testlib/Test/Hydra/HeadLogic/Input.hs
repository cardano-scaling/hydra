{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.Input where

import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.HeadLogic.Input (Input)
import "hydra-tx" Hydra.Chain.ChainState (ChainStateType (..), IsChainState)

import "hydra-node" Test.Hydra.API.ClientInput ()
import "hydra-node" Test.Hydra.Chain ()
import "hydra-node" Test.Hydra.Network ()
import "hydra-node" Test.Hydra.Network.Message ()
import "hydra-tx" Test.Hydra.Tx.Gen (ArbitraryIsTx)

instance
  ( ArbitraryIsTx tx
  , Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  , IsChainState tx
  ) =>
  Arbitrary (Input tx)
  where
  arbitrary = genericArbitrary
  shrink = genericShrink
