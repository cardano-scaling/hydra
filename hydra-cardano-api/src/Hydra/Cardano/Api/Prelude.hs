module Hydra.Cardano.Api.Prelude (
  module Hydra.Prelude,
  module Cardano.Api,
  module Cardano.Api.Shelley,
  UTxO,
  UTxO' (UTxO),
  StandardCrypto,
  Era,
  LedgerEra,
  UsesStandardCrypto,
  unsafeHashFromBytes,
) where

import Hydra.Prelude hiding (Key)

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

type Era =
  BabbageEra

type LedgerEra =
  Ledger.BabbageEra StandardCrypto

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
