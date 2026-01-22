{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.API.ServerOutput where

import Hydra.API.ClientInput (ClientInput)
import Hydra.API.ServerOutput (ClientMessage, DecommitInvalidReason, Greetings, HeadStatus, NetworkInfo, ServerOutput (..), TimedServerOutput)
import Hydra.Chain (PostChainTx)
import Hydra.Chain.ChainState (ChainPointType, ChainStateType, IsChainState)
import Hydra.HeadLogic.Error (SideLoadRequirementFailure)
import Test.Hydra.Chain ()
import Test.Hydra.Ledger ()
import Test.Hydra.Network ()
import Test.Hydra.Node.Environment ()
import Test.Hydra.Node.State ()
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (ArbitraryIsTx)
import Test.QuickCheck (recursivelyShrink)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

instance Arbitrary (ServerOutput tx) => Arbitrary (TimedServerOutput tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (DecommitInvalidReason tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (SideLoadRequirementFailure tx) where
  arbitrary = genericArbitrary

instance (IsChainState tx, Arbitrary (ChainStateType tx), Arbitrary (ClientInput tx), Arbitrary (PostChainTx tx), ArbitraryIsTx tx) => Arbitrary (ClientMessage tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (Greetings tx) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, IsChainState tx) => ToADTArbitrary (Greetings tx)

data InvalidInput = InvalidInput

instance (ArbitraryIsTx tx, Arbitrary (ChainPointType tx), Arbitrary (ChainStateType tx)) => Arbitrary (ServerOutput tx) where
  arbitrary = genericArbitrary
  shrink = recursivelyShrink

instance (ArbitraryIsTx tx, Arbitrary (ChainPointType tx), Arbitrary (ChainStateType tx), IsChainState tx) => ToADTArbitrary (ServerOutput tx)

instance Arbitrary HeadStatus where
  arbitrary = genericArbitrary

-- | All information needed to distinguish behavior of the commit endpoint.
instance Arbitrary NetworkInfo where
  arbitrary = genericArbitrary
