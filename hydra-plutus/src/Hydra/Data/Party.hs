module Hydra.Data.Party where

import Hydra.Prelude hiding (init)

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import qualified PlutusTx
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin, toBuiltin)
import PlutusTx.IsData
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
  deriving newtype (Show)

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
    let cbor = serialise bytes
     in object ["vkey" .= String (decodeUtf8 cbor)]

instance FromJSON Party where
  parseJSON =
    withObject "Party" $ \o -> do
      cborText :: Text <- o .: "vkey"
      case deserialiseOrFail $ encodeUtf8 cborText of
        Left e -> fail $ show e
        Right vkey -> pure UnsafeParty{vkey}

-- | Create an on-chain 'Party' from some verification key bytes.
partyFromVerificationKeyBytes :: ByteString -> Party
partyFromVerificationKeyBytes =
  UnsafeParty . toBuiltin

-- | Get the verification key bytes contained from an on-chain 'Party'.
partyToVerficationKeyBytes :: Party -> ByteString
partyToVerficationKeyBytes (UnsafeParty bytes) =
  fromBuiltin bytes
