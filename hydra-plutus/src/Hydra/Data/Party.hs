module Hydra.Data.Party where

import Hydra.Prelude hiding (init)

import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified PlutusTx
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin, toBuiltin)
import PlutusTx.IsData
import qualified PlutusTx.Prelude as PlutusTx
import Test.QuickCheck (vector)

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

instance ToJSON Party where
  toJSON (UnsafeParty bytes) =
    object ["vkey" .= String (decodeUtf8 $ Base16.encode $ fromBuiltin bytes)]

instance FromJSON Party where
  parseJSON =
    withObject "Party" $ \o -> do
      hexText :: Text <- o .: "vkey"
      case Base16.decode $ encodeUtf8 hexText of
        Left e -> fail e
        Right bs -> pure UnsafeParty{vkey = toBuiltin bs}

-- | Create an on-chain 'Party' from some verification key bytes.
partyFromVerificationKeyBytes :: ByteString -> Party
partyFromVerificationKeyBytes =
  UnsafeParty . toBuiltin

-- | Get the verification key bytes contained from an on-chain 'Party'.
partyToVerficationKeyBytes :: Party -> ByteString
partyToVerficationKeyBytes (UnsafeParty bytes) =
  fromBuiltin bytes
