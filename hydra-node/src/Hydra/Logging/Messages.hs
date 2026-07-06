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

instance (Typeable tx, ToCBOR (HydraNodeLog tx)) => ToCBOR (HydraLog tx) where
  toCBOR = \case
    DirectChain{directChain} -> toCBOR ("DirectChain" :: Text) <> toCBOR directChain
    APIServer{api} -> toCBOR ("APIServer" :: Text) <> toCBOR api
    Network{network} -> toCBOR ("Network" :: Text) <> toCBOR network
    Node{node} -> toCBOR ("Node" :: Text) <> toCBOR node
    NodeOptions{runOptions} -> toCBOR ("NodeOptions" :: Text) <> toCBOR runOptions
    SQLite{sqlite} -> toCBOR ("SQLite" :: Text) <> toCBOR sqlite
    EnteringMainloop -> toCBOR ("EnteringMainloop" :: Text)
    NodeHydrated -> toCBOR ("NodeHydrated" :: Text)
    ChainBackendStarted -> toCBOR ("ChainBackendStarted" :: Text)
    NetworkStarted -> toCBOR ("NetworkStarted" :: Text)

instance (Typeable tx, FromCBOR (HydraNodeLog tx)) => FromCBOR (HydraLog tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("DirectChain" :: Text) -> DirectChain <$> fromCBOR
      "APIServer" -> APIServer <$> fromCBOR
      "Network" -> Network <$> fromCBOR
      "Node" -> Node <$> fromCBOR
      "NodeOptions" -> NodeOptions <$> fromCBOR
      "SQLite" -> SQLite <$> fromCBOR
      "EnteringMainloop" -> pure EnteringMainloop
      "NodeHydrated" -> pure NodeHydrated
      "ChainBackendStarted" -> pure ChainBackendStarted
      "NetworkStarted" -> pure NetworkStarted
      tag -> fail $ show tag <> " is not a proper CBOR-encoded HydraLog"
