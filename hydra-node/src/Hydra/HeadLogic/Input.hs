{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Input where

import Hydra.Prelude

import Hydra.API.ClientInput (ClientInput)
import Hydra.Chain (ChainEvent)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Network.Message (Message, NetworkEvent)
import Hydra.Tx.IsTx (ArbitraryIsTx)

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

instance IsChainState tx => ToCBOR (Input tx) where
  toCBOR = \case
    ClientInput input -> toCBOR ("ClientInput" :: Text) <> toCBOR input
    NetworkInput ttl networkEvent -> toCBOR ("NetworkInput" :: Text) <> toCBOR ttl <> toCBOR networkEvent
    ChainInput chainEvent -> toCBOR ("ChainInput" :: Text) <> toCBOR chainEvent

instance IsChainState tx => FromCBOR (Input tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ClientInput" :: Text) -> ClientInput <$> fromCBOR
      "NetworkInput" -> NetworkInput <$> fromCBOR <*> fromCBOR
      "ChainInput" -> ChainInput <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (Input tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink
