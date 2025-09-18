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
import Hydra.Chain.Direct.Handlers (CardanoChainLog)
import Hydra.Node (HydraNodeLog)
import Hydra.Node.Network (NetworkLog)
import Hydra.Options (RunOptions)
import Hydra.PersistenceLog (PersistenceLog)

data HydraLog tx
  = DirectChain {directChain :: CardanoChainLog}
  | APIServer {api :: APIServerLog}
  | Network {network :: NetworkLog}
  | Node {node :: HydraNodeLog tx}
  | NodeOptions {runOptions :: RunOptions}
  | Persistence {persistenceLog :: PersistenceLog}
  | EnteringMainloop
  deriving stock (Generic)

deriving stock instance Eq (HydraNodeLog tx) => Eq (HydraLog tx)
deriving stock instance Show (HydraNodeLog tx) => Show (HydraLog tx)
deriving anyclass instance ToJSON (HydraNodeLog tx) => ToJSON (HydraLog tx)
deriving anyclass instance FromJSON (HydraNodeLog tx) => FromJSON (HydraLog tx)
