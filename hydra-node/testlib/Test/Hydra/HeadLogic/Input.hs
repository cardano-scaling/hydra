{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.Input where

import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Chain.ChainState (ChainStateType (..), IsChainState)
import "hydra-tx" Test.Hydra.Tx.Gen (ArbitraryIsTx)

import Hydra.HeadLogic.Input (Input)
import Test.Hydra.API.ClientInput ()
import Test.Hydra.Chain ()
import Test.Hydra.Network ()
import Test.Hydra.Network.Message ()

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
