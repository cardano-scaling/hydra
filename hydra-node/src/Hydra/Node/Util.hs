{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Node.Util where

import Hydra.Prelude

import Cardano.Api.UTxO (totalValue)
import Data.Secret (Secret, mkSecret)
import Hydra.Cardano.Api (
  AsType (AsPaymentExtendedKey, AsPaymentKey, AsSigningKey, AsVerificationKey),
  AssetId (..),
  CardanoSigningKey (..),
  File (..),
  FileError,
  FromSomeType (..),
  HasTextEnvelope,
  PaymentKey,
  TextEnvelopeError,
  UTxO,
  Value,
  VerificationKey,
  castVerificationKey,
  filterValue,
  getCardanoPaymentVerificationKey,
  readFileTextEnvelope,
  readFileTextEnvelopeAnyOf,
 )

-- | Thrown when a cardano key file could not be read as any of the accepted
-- text envelope formats.
newtype KeyFileError = KeyFileError (FileError TextEnvelopeError)
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Read a cardano signing key pair from a text envelope file, accepting both
-- normal ('PaymentKey') and extended ('PaymentExtendedKey') key formats.
-- Returns the verification key (always as 'VerificationKey PaymentKey') and
-- the signing key wrapped in 'CardanoSigningKey' inside 'Secret'.
readKeyPair :: FilePath -> IO (VerificationKey PaymentKey, Secret CardanoSigningKey)
readKeyPair keyPath = do
  sk <- readSigningKey keyPath
  pure (getCardanoPaymentVerificationKey sk, mkSecret sk)

-- | Read a 'CardanoSigningKey' from a text envelope file, accepting both
-- normal ('PaymentKey') and extended ('PaymentExtendedKey') key formats.
-- Extended keys are kept native (not converted) to preserve correct signing.
-- Throws 'KeyFileError' listing both accepted formats if neither matches.
readSigningKey :: FilePath -> IO CardanoSigningKey
readSigningKey path =
  readFileTextEnvelopeAnyOf
    [ FromSomeType (AsSigningKey AsPaymentKey) CardanoSigningKey
    , FromSomeType (AsSigningKey AsPaymentExtendedKey) CardanoExtendedSigningKey
    ]
    (File path)
    >>= either (throwIO . KeyFileError) pure

-- | Read a 'VerificationKey PaymentKey' from a text envelope file, accepting
-- both normal ('PaymentKey') and extended ('PaymentExtendedKey') key formats.
-- Extended keys are converted using 'castVerificationKey'.
-- Throws 'KeyFileError' listing both accepted formats if neither matches.
readVerificationKey :: FilePath -> IO (VerificationKey PaymentKey)
readVerificationKey path =
  readFileTextEnvelopeAnyOf
    [ FromSomeType (AsVerificationKey AsPaymentKey) id
    , FromSomeType (AsVerificationKey AsPaymentExtendedKey) castVerificationKey
    ]
    (File path)
    >>= either (throwIO . KeyFileError) pure

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
