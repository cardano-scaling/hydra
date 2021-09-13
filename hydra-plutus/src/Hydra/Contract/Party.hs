{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Contract.Party where

import Hydra.Prelude hiding (init)

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (rawDeserialiseVerKeyDSIGN, rawSerialiseVerKeyDSIGN), MockDSIGN, VerKeyDSIGN (VerKeyMockDSIGN))
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import qualified PlutusTx
import PlutusTx.IsData
import Schema (FormSchema (..), ToSchema (..))

-- TODO(SN): Copied party + json instances for deserializing in 'init' endpoint
-- and we were struggling to define 'Lift' and 'IsData'

newtype Party = UnsafeParty Integer -- (VerKeyDSIGN MockDSIGN)
  deriving stock (Eq, Generic)
  deriving newtype (Show, Num)

PlutusTx.makeLift ''Party

instance Arbitrary Party where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance ToJSON Party where
  toJSON (UnsafeParty i) =
    object ["vkey" .= (String . decodeUtf8 . Base16.encode $ rawSerialiseVerKeyDSIGN vkey)]
   where
    vkey = fromInteger i :: VerKeyDSIGN MockDSIGN

instance FromJSON Party where
  parseJSON = withObject "Party" $ \o -> do
    vkeyHex :: Text <- o .: "vkey"
    vkeyBytes <- either fail pure . Base16.decode $ encodeUtf8 vkeyHex
    (VerKeyMockDSIGN w) <- maybe (fail "deserialize verification key") pure $ rawDeserialiseVerKeyDSIGN vkeyBytes
    pure $ UnsafeParty $ fromIntegral w

instance ToSchema Party where
  toSchema = FormSchemaUnsupported "Party"

instance PlutusTx.ToData Party where
  toBuiltinData (UnsafeParty k) = toBuiltinData k

instance PlutusTx.FromData Party where
  fromBuiltinData = fmap fromInteger . fromBuiltinData

instance PlutusTx.UnsafeFromData Party where
  unsafeFromBuiltinData = fromInteger . unsafeFromBuiltinData
