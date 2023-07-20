-- | Shamelessly stolen from cardan-api 'Cardano.Api.SerialiseUsing' module.
--
-- With some minor modifications of also using encodeUtf8 in 'IsString' instance.
--
-- XXX: Expose / contribute upstream
module Hydra.Cardano.Api.UsingRawBytesHex where

import Hydra.Cardano.Api.Prelude

import Data.Aeson (FromJSONKey, ToJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Base16
import Data.String (IsString (..))
import qualified Data.Text as Text
import Data.Typeable (tyConName, typeRep, typeRepTyCon)

-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a hex
-- encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
-- > deriving (ToJSON, FromJSON) via (UsingRawBytesHex Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingRawBytesHex Blah)
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
  show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
  fromString = either error id . deserialiseFromRawBytesBase16 . encodeUtf8 . Text.pack

instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
  toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

instance (SerialiseAsRawBytes a) => FromJSON (UsingRawBytesHex a) where
  parseJSON =
    Aeson.withText tname $
      either fail pure . deserialiseFromRawBytesBase16 . encodeUtf8
   where
    tname = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a) where
  toJSONKey =
    Aeson.toJSONKeyText $ \(UsingRawBytesHex x) -> serialiseToRawBytesHexText x

instance (SerialiseAsRawBytes a) => FromJSONKey (UsingRawBytesHex a) where
  fromJSONKey =
    Aeson.FromJSONKeyTextParser $
      either fail pure . deserialiseFromRawBytesBase16 . encodeUtf8

-- * Internal

-- | Don't use this, use 'deserialiseFromRawBytesHex' instead.
deserialiseFromRawBytesBase16 ::
  SerialiseAsRawBytes a => ByteString -> Either String (UsingRawBytesHex a)
deserialiseFromRawBytesBase16 str =
  case Base16.decode str of
    Right raw -> case deserialiseFromRawBytes ttoken raw of
      Right x -> Right (UsingRawBytesHex x)
      -- XXX: cannot user inner value because SerialiseAsRawBytesError data
      -- constructor not exposed
      Left e -> Left ("cannot deserialise " ++ show str ++ ".  The error was: " <> show e)
    Left msg -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
 where
  ttoken = proxyToAsType (Proxy :: Proxy a)

unsafeDeserialiseFromRawBytesBase16 ::
  (HasCallStack, SerialiseAsRawBytes a) =>
  ByteString ->
  a
unsafeDeserialiseFromRawBytesBase16 =
  either error (\(UsingRawBytesHex a) -> a) . deserialiseFromRawBytesBase16
