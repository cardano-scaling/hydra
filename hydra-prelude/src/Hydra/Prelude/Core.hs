module Hydra.Prelude.Core where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Aeson.Types (FromJSONKey (..), FromJSONKeyFunction (FromJSONKeyTextParser), ToJSONKey (..), toJSONKeyText)
import Data.ByteString.Base16 qualified as Base16
import Relude

newtype HexBytes = HexBytes ByteString

instance ToJSON HexBytes where
  toJSON (HexBytes bytes) = toJSON $ encodeBase16 bytes

instance FromJSON HexBytes where
  parseJSON = withText "HexBytes" $ fmap HexBytes . decodeBase16

instance ToJSONKey HexBytes where
  toJSONKey = toJSONKeyText $ \(HexBytes bytes) -> encodeBase16 bytes

instance FromJSONKey HexBytes where
  fromJSONKey = FromJSONKeyTextParser $ fmap HexBytes . decodeBase16

-- | Encode some bytes to hex-encoded text.
--
-- >>> encodeBase16 "ab"
-- "4142"
encodeBase16 :: ByteString -> Text
encodeBase16 =
  decodeUtf8 . Base16.encode

-- | Decode some hex-encoded text string to raw bytes.
--
-- >>> decodeBase16 "dflkgjdjgdh"
-- Left "Not base 16"
decodeBase16 :: MonadFail f => Text -> f ByteString
decodeBase16 =
  either fail pure . Base16.decode . encodeUtf8

infixl 4 ?>

-- | If 'Nothing' use given 'e' as 'Left'. Infix version of `maybeToEither`.
(?>) :: Maybe a -> e -> Either e a
(?>) m e =
  case m of
    Nothing -> Left e
    Just a -> Right a
