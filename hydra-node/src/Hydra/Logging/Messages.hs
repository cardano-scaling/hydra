{-# LANGUAGE UndecidableInstances #-}

-- | Aggregates all tracing messages in a single type.
--
-- This module provides a central point where top-level traced messages are
-- grouped. This is useful for traces consumers that will need to do something
-- specific depending on various tracing messages, eg. monitoring and metrics
-- collection.
module Hydra.Logging.Messages where

import Hydra.Prelude

import Hydra.API.APIServerLog (APIServerLog)
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Network.Authenticate (AuthLog)
import Hydra.Network.Reliability (ReliabilityLog)
import Hydra.Node (HydraNodeLog)
import Hydra.Options (RunOptions, RunOfflineOptions)
import Hydra.Options.Offline ()

data HydraLog tx net
  = DirectChain {directChain :: DirectChainLog}
  | APIServer {api :: APIServerLog}
  | Network {network :: net}
  | Node {node :: HydraNodeLog tx}
  | NodeOptions {runOptions :: RunOptions}
  | NodeOfflineOptions {runOfflineOptions :: RunOfflineOptions}
  | Authentication {authentication :: AuthLog}
  | Reliability {reliability :: ReliabilityLog}
  deriving stock (Generic)

deriving stock instance (Eq net, Eq (HydraNodeLog tx)) => Eq (HydraLog tx net)
deriving stock instance (Show net, Show (HydraNodeLog tx)) => Show (HydraLog tx net)
deriving anyclass instance (ToJSON net, ToJSON (HydraNodeLog tx)) => ToJSON (HydraLog tx net)
deriving anyclass instance (FromJSON net, FromJSON (HydraNodeLog tx)) => FromJSON (HydraLog tx net)

instance (Arbitrary net, Arbitrary (HydraNodeLog tx)) => Arbitrary (HydraLog tx net) where
  arbitrary = genericArbitrary
  shrink = genericShrink
