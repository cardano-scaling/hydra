module Hydra.Cardano.Api.Hash where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import qualified Data.ByteString as BS
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Convert a cardano-api 'Hash' into a plutus 'PubKeyHash'
toPlutusKeyHash :: Hash PaymentKey -> Plutus.PubKeyHash
toPlutusKeyHash (PaymentKeyHash vkh) =
  Ledger.transKeyHash vkh

-- | Convert a cardano-api 'Hash' into a cardano-ledger 'KeyHash'
toLedgerKeyHash :: Hash PaymentKey -> Ledger.KeyHash 'Ledger.Witness StandardCrypto
toLedgerKeyHash (PaymentKeyHash (Ledger.KeyHash vkh)) =
  Ledger.KeyHash vkh

-- | Unsafe wrap some bytes as a 'Hash PaymentKey'.
--
-- Pre-condition: the input bytestring MUST be of length 28.
unsafePaymentKeyHashFromBytes ::
  HasCallStack =>
  ByteString ->
  Hash PaymentKey
unsafePaymentKeyHashFromBytes bytes
  | BS.length bytes /= 28 =
    error $ "unsafePaymentKeyHashFromBytes: pre-condition failed: " <> show (BS.length bytes) <> " bytes."
  | otherwise =
    PaymentKeyHash $ Ledger.KeyHash $ unsafeHashFromBytes bytes

-- | Unsafe wrap some bytes as a 'Hash ScriptData', relying on the fact that
-- Plutus is using Blake2b_256 for hashing data.
--
-- Pre-condition: the input bytestring MUST be of length 32.
unsafeScriptDataHashFromBytes ::
  HasCallStack =>
  ByteString ->
  Hash ScriptData
unsafeScriptDataHashFromBytes bytes
  | BS.length bytes /= 32 =
    error $ "unsafeScriptDataHashFromBytes: pre-condition failed: " <> show (BS.length bytes) <> " bytes."
  | otherwise =
    ScriptDataHash
      . unsafeMakeSafeHash
      $ unsafeHashFromBytes bytes
