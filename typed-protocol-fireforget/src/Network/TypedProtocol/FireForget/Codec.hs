module Network.TypedProtocol.FireForget.Codec where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Codec.CBOR.Decoding qualified as CBOR (
  Decoder,
  decodeWord,
 )
import Codec.CBOR.Encoding qualified as CBOR (
  Encoding,
  encodeWord,
 )
import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Class.MonadST
import Data.ByteString.Lazy (ByteString)
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.CBOR
import Network.TypedProtocol.Core
import Network.TypedProtocol.FireForget.Type (FireForget (..), Message (..), SFireForget (..))

codecFireForget ::
  forall m msg.
  (MonadST m, FromCBOR msg, ToCBOR msg) =>
  Codec (FireForget msg) CBOR.DeserialiseFailure m ByteString
codecFireForget =
  mkCodecCborLazyBS encode decode
 where
  encode ::
    forall st st'.
    Message (FireForget msg) st st' ->
    CBOR.Encoding
  encode MsgDone = CBOR.encodeWord 0
  encode (MsgSend msg) = CBOR.encodeWord 1 <> toCBOR msg

  decode ::
    forall s (st :: FireForget msg).
    ActiveState st =>
    StateToken st ->
    CBOR.Decoder s (SomeMessage st)
  decode stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (SingIdle, 0) -> pure $ SomeMessage MsgDone
      (SingIdle, 1) -> SomeMessage . MsgSend <$> fromCBOR
      (_, _) -> fail "codecFireForget.StIdle: unexpected key"
