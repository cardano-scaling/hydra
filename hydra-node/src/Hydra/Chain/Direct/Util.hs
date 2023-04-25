{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Util where

import Hydra.Prelude

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Ledger.Crypto (DSIGN)
import Hydra.Cardano.Api hiding (Block, SigningKey, VerificationKey)
import qualified Hydra.Cardano.Api as Shelley
import Ouroboros.Consensus.Cardano (CardanoBlock)
import PlutusCore.Data (Data)
import PlutusLedgerApi.V2 (BuiltinByteString, builtinDataToData, toBuiltinData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

type Block = CardanoBlock StandardCrypto
type VerificationKey = Crypto.VerKeyDSIGN (DSIGN StandardCrypto)
type SigningKey = Crypto.SignKeyDSIGN (DSIGN StandardCrypto)

readKeyPair :: FilePath -> IO (Shelley.VerificationKey PaymentKey, Shelley.SigningKey PaymentKey)
readKeyPair keyPath = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) keyPath
  pure (getVerificationKey sk, sk)

readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO a
readFileTextEnvelopeThrow asType =
  either (fail . show) pure <=< readFileTextEnvelope asType

readVerificationKey :: FilePath -> IO (Shelley.VerificationKey PaymentKey)
readVerificationKey = readFileTextEnvelopeThrow (Shelley.AsVerificationKey Shelley.AsPaymentKey)

-- | A simple retrying function with a constant delay. Retries only if the given
-- predicate evaluates to 'True'.
--
-- Better coupled with a 'timeout' function.
retry ::
  forall e m a.
  (MonadCatch m, MonadDelay m, Exception e) =>
  (e -> Bool) ->
  m a ->
  m a
retry predicate action =
  catchIf predicate action $ \_ ->
    threadDelay 0.5 >> retry predicate action
 where
  catchIf f a b = a `catch` \e -> if f e then b e else throwIO e

-- | Marker datum used to identify payment UTXO
markerDatum :: Data
markerDatum = builtinDataToData $ toBuiltinData ("Hydra Head Payment" :: BuiltinByteString)

-- | Hash of the markerDatum
markerDatumHash :: Hash ScriptData
markerDatumHash =
  hashScriptDataBytes . unsafeHashableScriptData $ fromPlutusData markerDatum

-- | Determine whether a 'TxOut' is marked to be used for paying Hydra Head transactions
isMarkedOutput :: TxOut CtxUTxO -> Bool
isMarkedOutput = \case
  (TxOut _ _ (TxOutDatumHash ha) _) -> ha == markerDatumHash
  _ -> False
