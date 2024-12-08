{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Chain.Direct.Util where

import Hydra.Prelude

import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Ledger.Crypto (DSIGN)
import Hydra.Cardano.Api hiding (Block, SigningKey, VerificationKey)
import Hydra.Cardano.Api qualified as Shelley
import Ouroboros.Consensus.Cardano (CardanoBlock)

type Block = CardanoBlock StandardCrypto
type VerificationKey = Crypto.VerKeyDSIGN (DSIGN StandardCrypto)
type SigningKey = Crypto.SignKeyDSIGN (DSIGN StandardCrypto)

readKeyPair :: FilePath -> IO (Shelley.VerificationKey PaymentKey, Shelley.SigningKey PaymentKey)
readKeyPair keyPath = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) keyPath
  pure (getVerificationKey sk, sk)

-- XXX: Should accept a 'File' path
readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO a
readFileTextEnvelopeThrow asType fileContents =
  either (fail . show) pure =<< readFileTextEnvelope asType (File fileContents)
