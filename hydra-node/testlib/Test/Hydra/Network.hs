{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Asynchronous messaging interface to the Hydra Network, e.g to other Hydra nodes.
--
-- Concrete implementations are provided by submodules. Import those instead of
-- this one if interested in actually configuring and running a real network
-- layer.
--
-- Incoming and outgoing messages are modelled as 'Message' data type.
module Test.Hydra.Network where

import "hydra-prelude" Hydra.Prelude hiding (show)
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (elements, listOf, suchThat)
import "base" Text.Show (Show (show))
import "cardano-ledger-core" Cardano.Ledger.Orphans ()
import "iproute" Data.IP (toIPv4w)
import "quickcheck-instances" Test.QuickCheck.Instances.Natural ()
import "quickcheck-instances" Test.QuickCheck.Instances.Text ()
import "text" Data.Text (pack)

import Hydra.Network

instance Arbitrary WhichEtcd where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance Arbitrary PortNumber where
  arbitrary = fromIntegral @Word16 <$> arbitrary

instance Arbitrary NodeId where
  arbitrary =
    NodeId . pack <$> suchThat (listOf (elements ['a' .. 'z'])) (not . null)

instance Arbitrary Host where
  arbitrary = do
    ip <- toIPv4w <$> arbitrary
    Host (toText $ show ip) <$> arbitrary

instance Arbitrary Connectivity where
  arbitrary = genericArbitrary

instance Arbitrary ProtocolVersion where
  arbitrary = genericArbitrary
