{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.StateEvent where

import Hydra.Tx.ChainState (ChainPointType, ChainStateType, IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Prelude
import Test.Hydra.HeadLogic.Outcome ()
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (ArbitraryIsTx)

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
