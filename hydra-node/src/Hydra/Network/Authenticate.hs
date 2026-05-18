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

instance ToCBOR msg => ToCBOR (Signed msg) where
  toCBOR (Signed msg sig party) = toCBOR msg <> toCBOR sig <> toCBOR party

instance FromCBOR msg => FromCBOR (Signed msg) where
  fromCBOR = Signed <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Middleware used to sign messages before broadcasting them to other peers
-- and verify signed messages upon receiving.
-- Only verified messages are pushed downstream to the internal network for the
-- node to consume and process. Non-verified messages get discarded.
--
-- The accepted-parties set is provided as an 'STM' action rather than a static
-- list so that a node can shrink (or grow) the live party set at runtime, e.g.
-- after observing a 'ParametersChanged' on chain (issue #1813,
-- dynamic-head-participants). Each inbound message is checked against a fresh
-- snapshot of the accepted set.
--
-- The 'readJoiningParty' accessor provides a narrowly-scoped escape valve for
-- Phase 2 of the dynamic-head-participants feature (issue #1813): when a join
-- is in flight, the multi-signed snapshot that authorizes the join includes
-- the joining party's own 'AckSn', and that message must be accepted before
-- the on-chain 'UpdateParametersTx' is observed (i.e. before the joiner is
-- added to the live party set). The accessor returns 'Just' the joining party
-- exactly when there's a pending 'AddParty' parameter update; the auth
-- middleware accepts any signed message from that party even while they
-- aren't yet in the regular set.
withAuthentication ::
  ( SignableRepresentation inbound
  , ToJSON inbound
  , SignableRepresentation outbound
  , MonadSTM m
  ) =>
  Tracer m AuthLog ->
  -- The party signing key
  SigningKey HydraKey ->
  -- Accepted other party members. Read once per inbound message.
  STM m [Party] ->
  -- Currently-joining party (Phase 2). Pass @pure Nothing@ when no join is in
  -- flight or to disable the speculative-accept exception.
  STM m (Maybe Party) ->
  -- The underlying raw network.
  NetworkComponent m (Signed inbound) (Signed outbound) a ->
  -- The node internal authenticated network.
  NetworkComponent m (Authenticated inbound) outbound a
withAuthentication tracer signingKey readOtherParties readJoiningParty withRawNetwork NetworkCallback{deliver, onConnectivity} action = do
  withRawNetwork NetworkCallback{deliver = checkSignature, onConnectivity} authenticate
 where
  checkSignature (Signed msg sig party@Party{vkey = partyVkey}) = do
    (parties, mJoining) <- atomically $ (,) <$> readOtherParties <*> readJoiningParty
    let allParties = me : parties
        knownSender = party `elem` allParties
        joiningSender = Just party == mJoining
        partyAllowed = knownSender || joiningSender
    if verify partyVkey sig msg && partyAllowed
      then deliver $ Authenticated msg party
      else traceWith tracer (mkAuthLog msg sig party)

  me = deriveParty signingKey

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
