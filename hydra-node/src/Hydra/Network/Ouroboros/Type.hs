module Hydra.Network.Ouroboros.Type where

import Hydra.Prelude

import Cardano.Binary qualified as CBOR
import Network.TypedProtocol.Codec
import Codec.CBOR.Read qualified as CBOR
import GHC.Show (Show (show))
import Network.TypedProtocol.Core (ReflRelativeAgency (ReflClientAgency))
import Network.TypedProtocol.Core 
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

data SFireForget (st :: FireForget msg) where
  SingIdle :: SFireForget StIdle
  SingDone :: SFireForget StDone

deriving instance Show (SFireForget st)

instance StateTokenI StIdle where
    stateToken = SingIdle
instance StateTokenI StDone where
    stateToken = SingDone

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

  type StateAgency StIdle = ClientAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SFireForget

deriving stock instance Show msg => Show (Message (FireForget msg) from to)

deriving stock instance Eq msg => Eq (Message (FireForget msg) from to)

codecFireForget
  :: forall m msg. Monad m
  => Codec (FireForget msg) CodecFailure m String
codecFireForget = undefined
{--
    Codec{encode, decode}
  where
    encode :: forall (st :: FireForget msg) (st' :: FireForget msg).
              Message (FireForget msg) st st'
           -> String
    encode MsgSend = "ping\n"
    encode MsgDone = "done\n"

    decode :: forall (st :: FireForget msg).
              ActiveState st
           => StateToken st
           -> m (DecodeStep String CodecFailure m (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, str) of
          (SingIdle, "idle") ->
            DecodeDone (SomeMessage MsgPong) trailing
          (SingDone, "done") ->
            DecodeDone (SomeMessage MsgDone) trailing

          (_       , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)
--}
{--
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
--}
