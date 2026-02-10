{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.API.ServerOutput where

import "QuickCheck" Test.QuickCheck (recursivelyShrink)
import "hydra-node" Hydra.API.ClientInput (ClientInput)
import "hydra-node" Hydra.API.ServerOutput (ClientMessage, DecommitInvalidReason, Greetings, HeadStatus, NetworkInfo, ServerOutput (..), TimedServerOutput)
import "hydra-node" Hydra.Chain (PostChainTx)
import "hydra-node" Hydra.HeadLogic.Error (SideLoadRequirementFailure)
import "hydra-node" Test.Hydra.Chain ()
import "hydra-node" Test.Hydra.Ledger ()
import "hydra-node" Test.Hydra.Network ()
import "hydra-node" Test.Hydra.Node.Environment ()
import "hydra-node" Test.Hydra.Node.State ()
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Chain.ChainState (ChainStateType, IsChainState)
import "hydra-tx" Test.Hydra.Tx.Gen (ArbitraryIsTx)
import "quickcheck-arbitrary-adt" Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

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

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (ServerOutput tx) where
  arbitrary = genericArbitrary
  shrink = recursivelyShrink

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx), IsChainState tx) => ToADTArbitrary (ServerOutput tx)

instance Arbitrary HeadStatus where
  arbitrary = genericArbitrary

-- | All information needed to distinguish behavior of the commit endpoint.
instance Arbitrary NetworkInfo where
  arbitrary = genericArbitrary
