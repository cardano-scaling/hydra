{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.Input where

import Test.Hydra.Prelude

import Hydra.Tx.ChainState (ChainStateType (..), IsChainState)
import Hydra.HeadLogic.Input (Input)

import Test.Hydra.API.ClientInput ()
import Test.Hydra.Chain ()
import Test.Hydra.Network ()
import Test.Hydra.Network.Message ()
import Test.Hydra.Tx.Gen (ArbitraryIsTx)

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
