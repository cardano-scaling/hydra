{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Node.Util where

import Hydra.Prelude

import Hydra.Cardano.Api (
  AsType (..),
  File (..),
  HasTextEnvelope,
  PaymentKey,
  SigningKey,
  VerificationKey,
  getVerificationKey,
  readFileTextEnvelope,
 )

readKeyPair :: FilePath -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
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
