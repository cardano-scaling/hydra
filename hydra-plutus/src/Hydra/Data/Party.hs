{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- TODO(SN): rename to /= Data but something which hints at "OnChain"
module Hydra.Data.Party where

import Hydra.Prelude hiding (init)

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (rawDeserialiseVerKeyDSIGN, rawSerialiseVerKeyDSIGN), MockDSIGN, VerKeyDSIGN (VerKeyMockDSIGN))
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import qualified PlutusTx
import PlutusTx.IsData

-- TODO(SN): Copied party + json instances for deserializing in 'init' endpoint
-- and we were struggling to define 'Lift' and 'IsData'. Ideally we would be
-- able to define all necessary instances on 'Hydra.Party' directly

newtype Party = UnsafeParty Integer
  deriving stock (Eq, Generic)
  deriving newtype (Show, Num)

PlutusTx.makeLift ''Party

instance Arbitrary Party where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance ToJSON Party where
  toJSON p =
    object ["vkey" .= (String . decodeUtf8 . Base16.encode $ rawSerialiseVerKeyDSIGN vkey)]
   where
    vkey = partyToVerKey p

instance FromJSON Party where
  parseJSON = withObject "Party" $ \o -> do
    vkeyHex :: Text <- o .: "vkey"
    vkeyBytes <- either fail pure . Base16.decode $ encodeUtf8 vkeyHex
    verKey <- maybe (fail "deserialize verification key") pure $ rawDeserialiseVerKeyDSIGN vkeyBytes
    pure $ partyFromVerKey verKey

instance PlutusTx.ToData Party where
  toBuiltinData (UnsafeParty k) = toBuiltinData k

instance PlutusTx.FromData Party where
  fromBuiltinData = fmap fromInteger . fromBuiltinData

instance PlutusTx.UnsafeFromData Party where
  unsafeFromBuiltinData = fromInteger . unsafeFromBuiltinData

partyFromVerKey :: VerKeyDSIGN MockDSIGN -> Party
partyFromVerKey (VerKeyMockDSIGN w) = UnsafeParty $ fromIntegral w

partyToVerKey :: Party -> VerKeyDSIGN MockDSIGN
partyToVerKey (UnsafeParty i) = fromInteger i
