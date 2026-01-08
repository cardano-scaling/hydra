{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Input where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.ClientInput (ClientInput)
import Hydra.Chain (ChainEvent)
import Hydra.Chain.ChainState (ChainStateType, IsChainState)
import Hydra.Network.Message (Message, NetworkEvent)
import Test.Hydra.Tx.Gen (ArbitraryIsTx)

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

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx), IsChainState tx) => Arbitrary (Input tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink
