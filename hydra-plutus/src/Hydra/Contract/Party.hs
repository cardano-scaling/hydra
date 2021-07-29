{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hydra.Contract.Party where

import Hydra.Prelude hiding (init)

import qualified PlutusTx
import PlutusTx.IsData
import Cardano.Crypto.DSIGN (VerKeyDSIGN (VerKeyMockDSIGN), MockDSIGN, DSIGNAlgorithm (rawSerialiseVerKeyDSIGN, SignKeyDSIGN, rawDeserialiseVerKeyDSIGN), deriveVerKeyDSIGN)
import Data.Aeson (ToJSONKey, FromJSONKey)

type SigningKey = SignKeyDSIGN MockDSIGN

-- | Identifies a party in a Hydra head.
newtype Party = UnsafeParty (VerKeyDSIGN MockDSIGN)
  deriving stock (Eq, Generic)
  deriving newtype (Show, Read, Num)

deriving instance Read (VerKeyDSIGN MockDSIGN)

instance Ord Party where
  (UnsafeParty a) <= (UnsafeParty b) =
    rawSerialiseVerKeyDSIGN a <= rawSerialiseVerKeyDSIGN b

instance Arbitrary Party where
  arbitrary = deriveParty . generateKey <$> arbitrary

instance ToJSONKey Party
instance ToJSON Party where
  toJSON (UnsafeParty (VerKeyMockDSIGN i)) = toJSON i

instance FromJSONKey Party
instance FromJSON Party where
  parseJSON = fmap fromInteger . parseJSON

instance FromCBOR Party where
  fromCBOR = UnsafeParty <$> fromCBOR

instance ToCBOR Party where
  toCBOR (UnsafeParty vk) = toCBOR vk

instance PlutusTx.ToData Party where
  toBuiltinData (UnsafeParty k) =
    toBuiltinData $ rawSerialiseVerKeyDSIGN k

instance PlutusTx.FromData Party where
  fromBuiltinData =
    fromBuiltinData >=> fmap UnsafeParty . rawDeserialiseVerKeyDSIGN

instance PlutusTx.UnsafeFromData Party where
  unsafeFromBuiltinData d =
    case rawDeserialiseVerKeyDSIGN $ unsafeFromBuiltinData d of
      Just k -> UnsafeParty k
      Nothing -> error "not a VerKeyDSIGNKey"

deriveParty :: SigningKey -> Party
deriveParty = coerce . deriveVerKeyDSIGN

generateKey :: Integer -> SigningKey
generateKey = fromInteger
