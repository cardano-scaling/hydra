{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.StateEvent where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Chain.ChainState (ChainPointType, ChainStateType, IsChainState)
import "hydra-tx" Test.Hydra.Tx.Gen (ArbitraryIsTx)

import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Test.Hydra.HeadLogic.Outcome ()

instance
  ( ArbitraryIsTx tx
  , Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  , IsChainState tx
  ) =>
  Arbitrary (StateEvent tx)
  where
  arbitrary = arbitrary >>= genStateEvent
  shrink = genericShrink

genStateEvent :: StateChanged tx -> Gen (StateEvent tx)
genStateEvent sc = StateEvent <$> arbitrary <*> pure sc <*> arbitrary
