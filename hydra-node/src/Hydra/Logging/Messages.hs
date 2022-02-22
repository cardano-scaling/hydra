{-# LANGUAGE UndecidableInstances #-}

-- | Aggregates all tracing messages in a single type.
--
-- This module provides a central point where top-level traced messages are
-- grouped. This is useful for traces consumers that will need to do something
-- specific depending on various tracing messages, eg. monitoring and metrics
-- collection.
module Hydra.Logging.Messages where

import Hydra.Prelude

import Hydra.API.Server (APIServerLog)
import Hydra.Chain.Direct (DirectChainLog)
import Hydra.Chain.ZeroMQ (MockChainLog)
import Hydra.Ledger (TxIdType, UTxOType)
import Hydra.Node (HydraNodeLog)
import Test.QuickCheck (oneof)

data HydraLog tx net
  = DirectChain {directChain :: DirectChainLog}
  | MockChain {chain :: MockChainLog tx}
  | APIServer {api :: APIServerLog}
  | Network {network :: net}
  | Node {node :: HydraNodeLog tx}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance
  ( Arbitrary net
  , Arbitrary tx
  , Arbitrary DirectChainLog
  , Arbitrary (UTxOType tx)
  , Arbitrary (TxIdType tx)
  , Arbitrary (MockChainLog tx)
  , Arbitrary APIServerLog
  ) =>
  Arbitrary (HydraLog tx net)
  where
  arbitrary =
    oneof
      [ DirectChain <$> arbitrary
      , APIServer <$> arbitrary
      , Network <$> arbitrary
      , Node <$> arbitrary
      ]
