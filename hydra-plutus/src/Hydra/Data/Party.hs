{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- TODO(SN): rename to /= Data but something which hints at "OnChain"
module Hydra.Data.Party where

import Hydra.Prelude hiding (init)

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN, VerKeyDSIGN (..))
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Maybe (fromJust)
import qualified PlutusTx
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin, toBuiltin)
import PlutusTx.IsData
import Test.QuickCheck (vector)

-- TODO(SN): Copied party + json instances for deserializing in 'init' endpoint
-- and we were struggling to define 'Lift' and 'IsData'. Ideally we would be
-- able to define all necessary instances on 'Hydra.Party' directly

newtype Party = UnsafeParty BuiltinByteString
  deriving stock (Eq, Generic)
  deriving newtype (Show)

PlutusTx.makeLift ''Party

instance Arbitrary Party where
  arbitrary = UnsafeParty . toBuiltin . BS.pack <$> vector 32

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
  toBuiltinData (UnsafeParty bytes) = toBuiltinData bytes

instance PlutusTx.FromData Party where
  fromBuiltinData = fmap UnsafeParty . fromBuiltinData

instance PlutusTx.UnsafeFromData Party where
  unsafeFromBuiltinData = UnsafeParty . unsafeFromBuiltinData

partyFromVerKey :: VerKeyDSIGN Ed25519DSIGN -> Party
partyFromVerKey =
  UnsafeParty . toBuiltin . rawSerialiseVerKeyDSIGN

partyToVerKey :: HasCallStack => Party -> VerKeyDSIGN Ed25519DSIGN
partyToVerKey (UnsafeParty bytes) =
  fromJust $ rawDeserialiseVerKeyDSIGN (fromBuiltin bytes)
