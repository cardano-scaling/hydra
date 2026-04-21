{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Node.Util where

import Hydra.Prelude

import Cardano.Api.UTxO (totalValue)
import Hydra.Cardano.Api (
  AssetId (..),
  CardanoSigningKey (..),
  File (..),
  HasTextEnvelope,
  PaymentExtendedKey,
  PaymentKey,
  SigningKey,
  UTxO,
  Value,
  VerificationKey,
  castVerificationKey,
  filterValue,
  getCardanoPaymentVerificationKey,
  readFileTextEnvelope,
 )

-- | Read a cardano signing key pair from a text envelope file, accepting both
-- normal ('PaymentKey') and extended ('PaymentExtendedKey') key formats.
-- Returns the verification key (always as 'VerificationKey PaymentKey') and
-- the signing key wrapped in 'CardanoSigningKey'.
readKeyPair :: FilePath -> IO (VerificationKey PaymentKey, CardanoSigningKey)
readKeyPair keyPath = do
  sk <- readSigningKey keyPath
  pure (getCardanoPaymentVerificationKey sk, sk)

-- | Read a 'CardanoSigningKey' from a text envelope file, accepting both
-- normal ('PaymentKey') and extended ('PaymentExtendedKey') key formats.
-- Extended keys are kept native (not converted) to preserve correct signing.
readSigningKey :: FilePath -> IO CardanoSigningKey
readSigningKey path = do
  result <- readFileTextEnvelope (File path)
  case result of
    Right sk -> pure (CardanoSigningKey sk)
    Left _normalError -> do
      extResult <- readFileTextEnvelope @(SigningKey PaymentExtendedKey) (File path)
      case extResult of
        Right extSk -> pure (CardanoExtendedSigningKey extSk)
        Left extError -> fail $ show extError

-- | Read a 'VerificationKey PaymentKey' from a text envelope file, accepting
-- both normal ('PaymentKey') and extended ('PaymentExtendedKey') key formats.
-- Extended keys are converted using 'castVerificationKey'.
readVerificationKey :: FilePath -> IO (VerificationKey PaymentKey)
readVerificationKey path = do
  result <- readFileTextEnvelope (File path)
  case result of
    Right vk -> pure vk
    Left _normalError -> do
      extResult <- readFileTextEnvelope @(VerificationKey PaymentExtendedKey) (File path)
      case extResult of
        Right extVk -> pure $ castVerificationKey extVk
        Left extError -> fail $ show extError

-- XXX: Should accept a 'File' path
readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  FilePath ->
  IO a
readFileTextEnvelopeThrow fileContents =
  either (fail . show) pure =<< readFileTextEnvelope (File fileContents)

-- | Filter and return any non-ADA assets as 'Left' if they are present in the 'UTxO' value.
checkNonADAAssetsUTxO :: UTxO -> Either Value ()
checkNonADAAssetsUTxO utxo =
  let nonADA = filterValue (/= AdaAssetId) $ totalValue utxo
   in if nonADA == mempty
        then Right ()
        else Left nonADA
