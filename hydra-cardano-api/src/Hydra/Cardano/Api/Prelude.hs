module Hydra.Cardano.Api.Prelude (
  module Cardano.Api,
  module Data.Aeson,
  HasCallStack,
  Proxy (..),
  Typeable,
  UTxO (UTxO),
  Era,
  LedgerEra,
  ledgerEraVersion,
  Text,
  decodeUtf8,
  encodeUtf8,
  toStrict,
  fromStrict,
  ByteString,
  Map,
  Set,
  unsafeHashFromBytes,
) where

import "aeson" Data.Aeson (FromJSON (..), ToJSON (..))
import "base" Data.Typeable (Typeable)
import "base" GHC.Stack (HasCallStack)
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString.Lazy (fromStrict, toStrict)
import "cardano-api" Cardano.Api hiding (
  UTxO,
  scriptLanguageSupportedInEra,
  toLedgerUTxO,
 )
import "cardano-api" Cardano.Api.UTxO (UTxO (..))
import "cardano-crypto-class" Cardano.Crypto.Hash.Class qualified as CC
import "cardano-ledger-binary" Cardano.Ledger.Binary qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Core qualified as Ledger
import "containers" Data.Map (Map)
import "containers" Data.Set (Set)
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)

type Era = ConwayEra

-- | Currently supported ledger era.
type LedgerEra = ShelleyLedgerEra Era

-- | Associated version for the fixed 'LedgerEra'.
ledgerEraVersion :: Ledger.Version
ledgerEraVersion = Ledger.eraProtVerLow @LedgerEra

-- | Interpret some raw 'ByteString' as a particular 'Hash'.
--
-- NOTE: This throws if byte string has a length different that the expected
-- target digest length.
unsafeHashFromBytes ::
  (HasCallStack, CC.HashAlgorithm hash) =>
  ByteString ->
  CC.Hash hash a
unsafeHashFromBytes bytes =
  case CC.hashFromBytes bytes of
    Nothing ->
      error $ "unsafeHashFromBytes: failed to convert hash: " <> show bytes
    Just h ->
      h
