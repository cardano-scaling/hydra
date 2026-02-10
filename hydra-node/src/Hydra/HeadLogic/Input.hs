module Hydra.HeadLogic.Input where

import "hydra-prelude" Hydra.Prelude

import "hydra-node" Hydra.API.ClientInput (ClientInput)
import "hydra-node" Hydra.Chain (ChainEvent)
import "hydra-node" Hydra.Network.Message (Message, NetworkEvent)
import "hydra-tx" Hydra.Chain.ChainState (IsChainState)

type TTL = Natural

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
