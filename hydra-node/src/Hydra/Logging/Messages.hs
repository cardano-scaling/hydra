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
import Hydra.Events.SQLiteBased (SQLiteLog)
import Hydra.Logging.PrettyError (PrettyError (..), Severity (..), genericFlatten)
import Hydra.Node (HydraNodeLog)
import Hydra.Node.Network (NetworkLog)
import Hydra.Options (RunOptions)

data HydraLog tx
  = DirectChain {directChain :: CardanoChainLog}
  | APIServer {api :: APIServerLog}
  | Network {network :: NetworkLog}
  | Node {node :: HydraNodeLog tx}
  | NodeOptions {runOptions :: RunOptions}
  | SQLite {sqlite :: SQLiteLog}
  | EnteringMainloop
  | NodeHydrated
  | ChainBackendStarted
  | NetworkStarted
  deriving stock (Generic)

deriving stock instance Eq (HydraNodeLog tx) => Eq (HydraLog tx)
deriving stock instance Show (HydraNodeLog tx) => Show (HydraLog tx)
deriving anyclass instance ToJSON (HydraNodeLog tx) => ToJSON (HydraLog tx)
deriving anyclass instance FromJSON (HydraNodeLog tx) => FromJSON (HydraLog tx)

instance PrettyError (HydraNodeLog tx) => PrettyError (HydraLog tx) where
  severity = \case
    DirectChain l -> severity l
    APIServer l -> severity l
    Network l -> severity l
    Node l -> severity l
    SQLite l -> severity l
    NodeOptions{} -> Info
    EnteringMainloop -> Info
    NodeHydrated -> Info
    ChainBackendStarted -> Info
    NetworkStarted -> Info
  showPretty = \case
    DirectChain l -> showPretty l
    APIServer l -> showPretty l
    Network l -> showPretty l
    Node l -> showPretty l
    SQLite l -> showPretty l
    NodeOptions{runOptions} -> genericFlatten runOptions
    EnteringMainloop -> []
    NodeHydrated -> []
    ChainBackendStarted -> []
    NetworkStarted -> []
