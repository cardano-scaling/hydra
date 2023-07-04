module Hydra.Network.Authenticate where

import Cardano.Crypto.Util (SignableRepresentation)
import Hydra.Crypto (HydraKey, Key (SigningKey), Signature, sign, verify)
import Hydra.Network (Network (Network, broadcast), NetworkComponent)
import Hydra.Party (Party (Party, vkey), deriveParty)
import Hydra.Prelude

-- | Represents a signed message over the network.
-- Becomes valid once its receivers verify it against its other peers
-- verification keys.
-- Messages are signed and turned into authenticated messages before
-- broadcasting them to other peers.
data Authenticated msg = Authenticated
  { payload :: msg
  , signature :: Signature msg
  , party :: Party
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (Arbitrary msg, SignableRepresentation msg) => Arbitrary (Authenticated msg) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ToCBOR msg => ToCBOR (Authenticated msg) where
  toCBOR (Authenticated msg sig party) = toCBOR msg <> toCBOR sig <> toCBOR party

instance FromCBOR msg => FromCBOR (Authenticated msg) where
  fromCBOR = Authenticated <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Middleware used to sign messages before broadcasting them to other peers
-- and verify signed messages upon receiving.
-- Only verified messages are pushed downstream to the internal network for the
-- node to consume and process. Non-verified messages get discarded.
withAuthentication ::
  ( MonadAsync m
  , SignableRepresentation msg
  ) =>
  -- The party signing key
  SigningKey HydraKey ->
  -- Other party members
  [Party] ->
  -- The underlying raw network.
  NetworkComponent m (Authenticated msg) a ->
  -- The node internal authenticated network.
  NetworkComponent m msg a
withAuthentication signingKey parties withRawNetwork callback action = do
  withRawNetwork checkSignature authenticate
 where
  checkSignature (Authenticated msg sig party@Party{vkey = partyVkey}) = do
    when (verify partyVkey sig msg && elem party parties) $
      callback msg
  authenticate = \Network{broadcast} ->
    action $ Network{broadcast = \msg -> broadcast (Authenticated msg (sign signingKey msg) (deriveParty signingKey))}
