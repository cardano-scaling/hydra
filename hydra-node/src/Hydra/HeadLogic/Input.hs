{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Input (
  Input (..),
  TTL,
  MessagePriority (..),
  inputPriority,
) where

import Hydra.Prelude

import Hydra.API.ClientInput (ClientInput)
import Hydra.Chain (ChainEvent)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Tx.IsTx (ArbitraryIsTx)

type TTL = Natural

-- | Priority level for input messages. Protocol messages (ReqSn, AckSn) get
-- high priority to prevent them from being delayed by transaction messages
-- under high load.
data MessagePriority = HighPriority | LowPriority
  deriving stock (Eq, Show, Generic)

-- | Classify an input by its priority. Protocol messages that are critical
-- for snapshot progress get high priority, while transaction submissions
-- get low priority.
inputPriority :: Input tx -> MessagePriority
inputPriority = \case
  -- Protocol messages: high priority to ensure snapshot progress
  NetworkInput{networkEvent = ReceivedMessage{msg = ReqSn{}}} -> HighPriority
  NetworkInput{networkEvent = ReceivedMessage{msg = AckSn{}}} -> HighPriority
  -- Connectivity events: high priority for protocol health
  NetworkInput{networkEvent = ConnectivityEvent{}} -> HighPriority
  -- Transaction requests: low priority (can be delayed under load)
  NetworkInput{networkEvent = ReceivedMessage{msg = ReqTx{}}} -> LowPriority
  NetworkInput{networkEvent = ReceivedMessage{msg = ReqDec{}}} -> LowPriority
  -- Client inputs: high priority (user-initiated actions)
  ClientInput{} -> HighPriority
  -- Chain events: high priority (must be processed promptly)
  ChainInput{} -> HighPriority

-- | Inputs that are processed by the head logic (the "core"). Corresponding to
-- each of the "shell" layers, we distinguish between inputs from the client,
-- the network and the chain.
data Input tx
  = -- | Input received from clients via the "Hydra.API".
    ClientInput {clientInput :: ClientInput tx}
  | -- | Input received from peers via a "Hydra.Network".
    --
    --  * `ttl` is a simple counter that's decreased every time the event is
    --    reenqueued due to a wait. It's default value is `defaultTTL`
    NetworkInput {ttl :: TTL, networkEvent :: NetworkEvent (Message tx)}
  | -- | Input received from the chain via a "Hydra.Chain".
    ChainInput {chainEvent :: ChainEvent tx}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (Input tx)
deriving stock instance IsChainState tx => Show (Input tx)
deriving anyclass instance IsChainState tx => ToJSON (Input tx)
deriving anyclass instance IsChainState tx => FromJSON (Input tx)

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (Input tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink
