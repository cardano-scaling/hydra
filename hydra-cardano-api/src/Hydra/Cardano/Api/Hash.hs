module Hydra.Cardano.Api.Hash where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Data.ByteString as BS
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Convert a cardano-api's 'Hash' into a plutus' 'PubKeyHash'
toPlutusKeyHash :: Hash PaymentKey -> Plutus.PubKeyHash
toPlutusKeyHash (PaymentKeyHash vkh) =
  Ledger.transKeyHash vkh

-- | Convert a cardano-api's 'Hash' into a cardano-ledger's 'KeyHash'
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
    identity
    (deserialiseFromCBOR (proxyToAsType Proxy) (serialiseToCBOR a))
