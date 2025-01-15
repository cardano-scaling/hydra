module Hydra.Network.Ouroboros.Type where

import Hydra.Prelude

import Cardano.Binary qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import GHC.Show (Show (show))
import Network.TypedProtocol.Core (ReflRelativeAgency (ReflClientAgency))
import Network.TypedProtocol (Protocol (..))
import Network.TypedProtocol.Codec (Codec)
import Network.TypedProtocol.Codec.CBOR (mkCodecCborLazyBS)
import Network.TypedProtocol.Core (PeerRole)
import Network.TypedProtocol.Driver (SomeMessage (SomeMessage))
import Ouroboros.Consensus.Util (ShowProxy (..))

-- | TODO explain Protocol
--
-- It is used both as a type level tag for the protocol and as the kind of the
-- types of the states in the protocol state machine. That is @FireForget@ is a
-- kind, and @StIdle@ is a type of that kind.
data FireForget msg where
  StIdle :: FireForget msg
  StDone :: FireForget msg

instance ShowProxy (FireForget msg) where
  showProxy _ = "FireForget"

instance Protocol (FireForget msg) where
  -- The actual messages in our protocol.
  --
  -- Messages define the possible transitions between the protocol's state as defined
  -- by `FireForget`. In this particular case things are extremely simple: The protocol
  -- handles `Msg` containing some payload until terminated by `MsgDone`.
  data Message (FireForget msg) from to where
    MsgSend :: msg -> Message (FireForget msg) 'StIdle 'StIdle
    MsgDone :: Message (FireForget msg) 'StIdle 'StDone

  -- We have to explain to the framework what our states mean, in terms of
  -- who is expected to send and receive in the different states.
  --
  -- Idle states are where it is for the client to send a message.
  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle

  -- In our protocol the server is always receiving, thus in no state the server
  -- has agency.
  data ServerHasAgency st

  -- In the done state neither client nor server can send messages.
  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

deriving stock instance Show msg => Show (Message (FireForget msg) from to)

deriving stock instance Eq msg => Eq (Message (FireForget msg) from to)

instance Show (ClientHasAgency (st :: FireForget msg)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: FireForget msg)) where
  show _ = error "absurd"

codecFireForget ::
  forall a m.
  ( MonadST m
  , ToCBOR a
  , FromCBOR a
  ) =>
  Codec (FireForget a) CBOR.DeserialiseFailure m LByteString
codecFireForget = mkCodecCborLazyBS encodeMsg decodeMsg
 where
  encodeMsg ::
    forall (pr :: PeerRole) st st'.
    PeerHasAgency pr st ->
    Message (FireForget a) st st' ->
    CBOR.Encoding
  encodeMsg (ReflClientAgency TokIdle) MsgDone = CBOR.encodeWord 0
  encodeMsg (ReflClientAgency TokIdle) (MsgSend msg) = CBOR.encodeWord 1 <> toCBOR msg

  decodeMsg ::
    forall (pr :: PeerRole) s (st :: FireForget a).
    PeerHasAgency pr st ->
    CBOR.Decoder s (SomeMessage st)
  decodeMsg stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (ReflClientAgency TokIdle, 0) ->
        return $ SomeMessage MsgDone
      (ReflClientAgency TokIdle, 1) -> do
        SomeMessage . MsgSend <$> fromCBOR
      (ReflClientAgency TokIdle, _) ->
        fail "codecFireForget.StIdle: unexpected key"
