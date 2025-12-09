module Hydra.Data.Party where

import Hydra.Prelude hiding (init)

import Data.ByteString qualified as BS
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin, toBuiltin)
import PlutusTx.IsData
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (Arbitrary (..), vector)

-- | On-chain representation of a Hydra party.
--
-- NOTE: This roughly corresponds to the 'Party' in 'hydra-node', but is
-- simplified to allow usage of this type in plutus-tx. If we would use the
-- complex type directly, which is based on 'cardano-crypto-class', we would get
-- errors like "Error: Unsupported feature: Kind: GHC.Types.Nat".
--
-- The data constructor should not be used to construct this value as it would
-- always come from off-chain code via 'partyFromVerificationKeyBytes'.
newtype Party = UnsafeParty {vkey :: BuiltinByteString}
  deriving stock (Eq, Generic)
  deriving newtype (Show, PlutusTx.Eq)

instance Arbitrary Party where
  arbitrary = partyFromVerificationKeyBytes . BS.pack <$> vector 32

instance PlutusTx.ToData Party where
  toBuiltinData (UnsafeParty bytes) = toBuiltinData bytes

instance PlutusTx.FromData Party where
  fromBuiltinData = fmap UnsafeParty . fromBuiltinData

instance PlutusTx.UnsafeFromData Party where
  unsafeFromBuiltinData = UnsafeParty . unsafeFromBuiltinData

-- | Create an on-chain 'Party' from some verification key bytes.
partyFromVerificationKeyBytes :: ByteString -> Party
partyFromVerificationKeyBytes =
  UnsafeParty . toBuiltin

-- | Get the verification key bytes contained from an on-chain 'Party'.
partyToVerificationKeyBytes :: Party -> ByteString
partyToVerificationKeyBytes (UnsafeParty bytes) =
  fromBuiltin bytes
