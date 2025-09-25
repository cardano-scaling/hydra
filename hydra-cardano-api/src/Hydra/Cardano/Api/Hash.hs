module Hydra.Cardano.Api.Hash where

import Hydra.Cardano.Api.Prelude

import Cardano.Api (PaymentKey, ScriptData, SerialiseAsCBOR, deserialiseFromCBOR, proxyToAsType, serialiseToCBOR)
import Cardano.Api.Shelley (Hash (..))
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.Plutus.TxInfo (transKeyHash)
import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 qualified as Plutus

-- * Type conversions

-- | Convert a cardano-api 'Hash' into a plutus 'PubKeyHash'
toPlutusKeyHash :: Hash PaymentKey -> Plutus.PubKeyHash
toPlutusKeyHash (PaymentKeyHash vkh) = transKeyHash vkh

-- | Convert a cardano-api 'Hash' into a cardano-ledger 'KeyHash'
toLedgerKeyHash :: Hash PaymentKey -> Ledger.KeyHash 'Ledger.Witness
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
-- Plutus is using Blake2b_256 for hashing data (according to 'cardano-ledger').
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

-- NOTE: The constructor for Hash isn't exposed in the cardano-api. Although
-- there's a 'CastHash' type-class, there are not instances for everything, so
-- we have to resort to binary serialisation/deserialisation to cast hashes.
unsafeCastHash ::
  (SerialiseAsCBOR (Hash a), SerialiseAsCBOR (Hash b), HasCallStack) =>
  Hash a ->
  Hash b
unsafeCastHash a =
  either
    (\e -> error $ "unsafeCastHash: incompatible hash: " <> show e)
    id
    (deserialiseFromCBOR (proxyToAsType Proxy) (serialiseToCBOR a))
