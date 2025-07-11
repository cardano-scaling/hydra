{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Node.Util where

import Hydra.Prelude
    ( MonadFail(fail),
      Applicative(pure),
      IO,
      either,
      (.),
      (=<<),
      show,
      FilePath )

import Hydra.Cardano.Api (
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
  sk <- readFileTextEnvelopeThrow keyPath
  pure (getVerificationKey sk, sk)

-- XXX: Should accept a 'File' path
readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  FilePath ->
  IO a
readFileTextEnvelopeThrow fileContents =
  either (fail . show) pure =<< readFileTextEnvelope (File fileContents)
