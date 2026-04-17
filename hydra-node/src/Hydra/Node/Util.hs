{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Node.Util where

import Hydra.Prelude

import Cardano.Api.UTxO (totalValue)
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  AsType (..),
  AssetId (..),
  File (..),
  HasTextEnvelope,
  PaymentExtendedKey,
  PaymentKey,
  SerialiseAsRawBytes (..),
  SigningKey,
  UTxO,
  Value,
  VerificationKey,
  castVerificationKey,
  filterValue,
  getVerificationKey,
  readFileTextEnvelope,
 )

readKeyPair :: FilePath -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
readKeyPair keyPath = do
  sk <- readSigningKey keyPath
  pure (getVerificationKey sk, sk)

-- | Read a 'SigningKey PaymentKey' from a text envelope file, accepting both
-- normal ('PaymentKey') and extended ('PaymentExtendedKey') key formats.
-- Extended keys are converted to normal keys by extracting the first 32 bytes
-- of the 64-byte BIP32-Ed25519 extended signing key.
readSigningKey :: FilePath -> IO (SigningKey PaymentKey)
readSigningKey path = do
  result <- readFileTextEnvelope (File path)
  case result of
    Right sk -> pure sk
    Left _normalError -> do
      extResult <- readFileTextEnvelope @(SigningKey PaymentExtendedKey) (File path)
      case extResult of
        Right extSk -> do
          let extRawBytes = serialiseToRawBytes extSk
              normalRawBytes = BS.take 32 extRawBytes
          case deserialiseFromRawBytes (AsSigningKey AsPaymentKey) normalRawBytes of
            Right sk -> pure sk
            Left err -> fail $ "Failed to convert extended signing key: " <> show err
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
