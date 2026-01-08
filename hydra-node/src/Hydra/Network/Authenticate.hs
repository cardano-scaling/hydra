{-# LANGUAGE DuplicateRecordFields #-}

-- | A `NetworkComponent` that handles authentication of sent and received messages.
--
-- This "middleware" uses `HydraKey` keys for signing own messages and verifying
-- others', providing `Authenticated` messages to consumers.
module Hydra.Network.Authenticate where

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Tracer (Tracer)
import Data.Aeson (Options (tagSingleConstructors), defaultOptions, genericToJSON)
import Data.Aeson qualified as Aeson
import Hydra.Logging (traceWith)
import Hydra.Network (Network (Network, broadcast), NetworkCallback (..), NetworkComponent)
import Hydra.Prelude
import Hydra.Tx (Party (Party, vkey), deriveParty)
import Hydra.Tx.Crypto (HydraKey, Key (SigningKey), Signature, sign, verify)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen ()

-- | Represents a signed message over the network.
-- Becomes valid once its receivers verify it against its other peers
-- verification keys.
-- Messages are signed and turned into authenticated messages before
-- broadcasting them to other peers.
data Signed msg = Signed
  { payload :: msg
  , signature :: Signature msg
  , party :: Party
  }
  deriving stock (Eq, Show, Generic)

data Authenticated msg = Authenticated
  { payload :: msg
  , party :: Party
  }
  deriving stock (Eq, Show, Generic)

instance (Arbitrary msg, SignableRepresentation msg) => Arbitrary (Signed msg) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ToCBOR msg => ToCBOR (Signed msg) where
  toCBOR (Signed msg sig party) = toCBOR msg <> toCBOR sig <> toCBOR party

instance FromCBOR msg => FromCBOR (Signed msg) where
  fromCBOR = Signed <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Middleware used to sign messages before broadcasting them to other peers
-- and verify signed messages upon receiving.
-- Only verified messages are pushed downstream to the internal network for the
-- node to consume and process. Non-verified messages get discarded.
withAuthentication ::
  ( SignableRepresentation inbound
  , ToJSON inbound
  , SignableRepresentation outbound
  ) =>
  Tracer m AuthLog ->
  -- The party signing key
  SigningKey HydraKey ->
  -- Other party members
  [Party] ->
  -- The underlying raw network.
  NetworkComponent m (Signed inbound) (Signed outbound) a ->
  -- The node internal authenticated network.
  NetworkComponent m (Authenticated inbound) outbound a
withAuthentication tracer signingKey parties withRawNetwork NetworkCallback{deliver, onConnectivity} action = do
  withRawNetwork NetworkCallback{deliver = checkSignature, onConnectivity} authenticate
 where
  checkSignature (Signed msg sig party@Party{vkey = partyVkey}) =
    if verify partyVkey sig msg && party `elem` allParties
      then deliver $ Authenticated msg party
      else traceWith tracer (mkAuthLog msg sig party)

  me = deriveParty signingKey

  allParties = me : parties

  authenticate Network{broadcast} =
    action $
      Network
        { broadcast = \msg ->
            broadcast (Signed msg (sign signingKey msg) me)
        }

-- | Smart constructor for 'MessageDropped'
mkAuthLog :: (ToJSON msg, Show signature) => msg -> signature -> Party -> AuthLog
mkAuthLog message signature party =
  MessageDropped
    { message = decodeUtf8 $ Aeson.encode message
    , signature = show signature
    , party
    }

data AuthLog = MessageDropped {message :: Text, signature :: Text, party :: Party}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- NOTE: we make an explicit instance here because the default derivation
-- from Generic does not add a tag for single constructor data types or newtypes.
-- Without the tag, the message is pretty cryptic in the logs
instance ToJSON AuthLog where
  toJSON = genericToJSON defaultOptions{tagSingleConstructors = True}

instance Arbitrary AuthLog where
  arbitrary = genericArbitrary
  shrink = genericShrink
