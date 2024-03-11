{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Event where

import Hydra.Prelude

import Hydra.API.ClientInput (ClientInput)
import Hydra.Chain (ChainEvent, IsChainState)
import Hydra.Ledger (IsTx)
import Hydra.Network.Message (Message)
import Hydra.Party (Party)

type TTL = Natural

-- TODO: Move logic up and types down or re-organize using explicit exports

-- | The different events which are processed by the head logic (the "core").
-- Corresponding to each of the "shell" layers, we distinguish between events
-- from the client, the network and the chain.
data Event tx
  = -- | Event received from clients via the "Hydra.API".
    ClientEvent {clientInput :: ClientInput tx}
  | -- | Event received from peers via a "Hydra.Network".
    --
    --  * `ttl` is a simple counter that's decreased every time the event is
    --    reenqueued due to a wait. It's default value is `defaultTTL`
    NetworkEvent {ttl :: TTL, party :: Party, message :: Message tx}
  | -- | Event received from the chain via a "Hydra.Chain".
    OnChainEvent {chainEvent :: ChainEvent tx}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (Event tx)
deriving stock instance IsChainState tx => Show (Event tx)
deriving anyclass instance IsChainState tx => ToJSON (Event tx)
deriving anyclass instance IsChainState tx => FromJSON (Event tx)

instance (IsTx tx, IsChainState tx) => Arbitrary (Event tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink
