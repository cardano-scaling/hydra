{-# LANGUAGE UndecidableInstances #-}

-- | Aggregates all tracing messages in a single type.
--
-- This module provides a central point where top-level traced messages are
-- grouped. This is useful for traces consumers that will need to do something
-- specific depending on various tracing messages, eg. monitoring and metrics
-- collection.
module Hydra.Logging.Messages where

import Cardano.Prelude (Show)
import Hydra.API.Server (APIServerLog)
import Hydra.Chain.ZeroMQ (MockChainLog)
import Hydra.Network.ZeroMQ (NetworkLog)
import Hydra.Node (HydraNodeLog)

data HydraLog tx
  = MockChain MockChainLog
  | APIServer APIServerLog
  | Network NetworkLog
  | Node (HydraNodeLog tx)
  deriving (Show)
