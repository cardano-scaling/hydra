{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans instances partly copied from Plutus, partly coming from us for test
-- purpose.
module Hydra.Plutus.Orphans where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "aeson" Data.Aeson (object, withObject, (.:), (.=))
import "aeson" Data.Aeson qualified as Aeson
import "base16-bytestring" Data.ByteString.Base16 qualified as Base16
import "plutus-ledger-api" PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime (..), PubKeyHash (..))
import "plutus-tx" PlutusTx.Prelude (BuiltinByteString, fromBuiltin, toBuiltin)
import "quickcheck-instances" Test.QuickCheck.Instances.ByteString ()

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin <$> (arbitrary :: Gen ByteString)

instance Arbitrary CurrencySymbol where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary POSIXTime where
  arbitrary = POSIXTime <$> arbitrary

instance ToJSON PubKeyHash where
  toJSON kh =
    object
      [ "tag" .= Aeson.String "PubKeyHash"
      , "keyHash" .= Aeson.String (decodeUtf8 $ Base16.encode $ fromBuiltin $ getPubKeyHash kh)
      ]

instance FromJSON PubKeyHash where
  parseJSON = withObject "PubKeyHash" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "PubKeyHash" -> do
        hexText :: Text <- o .: "keyHash"
        case Base16.decode $ encodeUtf8 hexText of
          Left e -> fail e
          Right bs -> pure $ PubKeyHash (toBuiltin bs)
      _ -> fail "Expected tag to be PubKeyHash"

instance Arbitrary PubKeyHash where
  arbitrary = genericArbitrary
