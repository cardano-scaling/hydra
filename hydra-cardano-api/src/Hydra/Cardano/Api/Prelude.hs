module Hydra.Cardano.Api.Prelude (
  module Cardano.Api,
  module Cardano.Api.Shelley,
  module Data.Aeson,
  module Relude,
  Proxy (..),
  UTxO,
  UTxO' (UTxO),
  StandardCrypto,
  Era,
  LedgerEra,
  UsesStandardCrypto,
  unsafeHashFromBytes,
  Arbitrary (..),
  Gen,
) where

import Cardano.Api hiding (
  UTxO,
  multiAssetSupportedInEra,
  scriptDataSupportedInEra,
  scriptLanguageSupportedInEra,
  toLedgerUTxO,
 )
import Cardano.Api.Shelley hiding (
  UTxO,
  multiAssetSupportedInEra,
  scriptDataSupportedInEra,
  scriptLanguageSupportedInEra,
  toLedgerUTxO,
 )
import Cardano.Api.UTxO (UTxO, UTxO' (..))
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Babbage as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger.Era
import Data.Aeson (FromJSON (..), ToJSON (..))
import Relude
import Test.QuickCheck (Arbitrary (..), Gen)
import Prelude hiding (error, show)

type Era = BabbageEra

type LedgerEra = Ledger.BabbageEra StandardCrypto

type UsesStandardCrypto era =
  (Ledger.Era.Crypto (ShelleyLedgerEra era) ~ StandardCrypto)

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
