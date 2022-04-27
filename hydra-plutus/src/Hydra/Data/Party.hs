module Hydra.Data.Party where

import Hydra.Prelude hiding (init)

import qualified Data.ByteString as BS
import qualified PlutusTx
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin, toBuiltin)
import PlutusTx.IsData
import Test.QuickCheck (vector)

-- | On-chain representation of a Hydra party.
--
-- NOTE: This roughly corresponds to the 'Hydra.Party.Party', but is greatly
-- simplified to allow usage of this type in plutus-tx. If we would use the
-- complex type directly, which is based on `Cardano.Crypto.DSIGN`, we would get
-- errors like "Error: Unsupported feature: Kind: GHC.Types.Nat".
newtype Party = UnsafeParty BuiltinByteString
  deriving stock (Eq, Generic)
  deriving newtype (Show)

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
