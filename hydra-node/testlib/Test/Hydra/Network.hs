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

import Hydra.Network
import Hydra.Prelude hiding (show)
import Test.Hydra.Prelude

import Cardano.Ledger.Orphans ()
import Data.IP (toIPv4w)
import Data.Text (pack)
import Test.QuickCheck (elements, listOf, suchThat)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Text.Show (Show (show))

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
