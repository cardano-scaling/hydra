{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- TODO(SN): rename to /= Data but something which hints at "OnChain"
module Hydra.Data.Party where

import Hydra.Prelude hiding (init)

import qualified Data.ByteString as BS
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

instance PlutusTx.ToData Party where
  toBuiltinData (UnsafeParty bytes) = toBuiltinData bytes

instance PlutusTx.FromData Party where
  fromBuiltinData = fmap UnsafeParty . fromBuiltinData

instance PlutusTx.UnsafeFromData Party where
  unsafeFromBuiltinData = UnsafeParty . unsafeFromBuiltinData

partyFromVerificationKeyBytes :: ByteString -> Party
partyFromVerificationKeyBytes =
  UnsafeParty . toBuiltin

partyToVerficationKeyBytes :: Party -> ByteString
partyToVerficationKeyBytes (UnsafeParty bytes) =
  fromBuiltin bytes
