module Hydra.Network.Ouroboros.Codec where

import Hydra.Prelude

import Cardano.Binary qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Hydra.Network.Ouroboros.Type (FireForget (..), Message (..), SFireForget (..))
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.CBOR (mkCodecCborLazyBS)
import Network.TypedProtocol.Core

codecFireForget ::
  forall m msg.
  (MonadST m, FromCBOR msg, ToCBOR msg) =>
  Codec (FireForget msg) CBOR.DeserialiseFailure m LByteString
codecFireForget =
  mkCodecCborLazyBS encode decode
 where
  encode ::
    forall msg' (st :: FireForget msg') (st' :: FireForget msg').
    ToCBOR msg' =>
    Message (FireForget msg') st st' ->
    CBOR.Encoding
  encode MsgDone = CBOR.encodeWord 0
  encode (MsgSend msg) = CBOR.encodeWord 1 <> toCBOR msg

  decode ::
    forall msg' s (st :: FireForget msg').
    (FromCBOR msg', ActiveState st) =>
    StateToken st ->
    CBOR.Decoder s (SomeMessage st)
  decode stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (SingIdle, 0) -> pure $ SomeMessage MsgDone
      (SingIdle, 1) -> SomeMessage . MsgSend <$> fromCBOR
      (_, _) -> fail "codedFireForget.StIdle: unexpected"
