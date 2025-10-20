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
  Arbitrary (..),
  Gen,
) where

import Cardano.Api hiding (
  UTxO,
  scriptLanguageSupportedInEra,
  toLedgerUTxO,
 )
import Cardano.Api.UTxO (UTxO (..))
import Cardano.Crypto.Hash.Class qualified as CC
import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Test.QuickCheck (Arbitrary (..), Gen)

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
