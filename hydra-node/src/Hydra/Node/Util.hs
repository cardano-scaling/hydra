{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Node.Util where

import Hydra.Prelude

import Hydra.Cardano.Api (
  AssetId (..),
  File (..),
  HasTextEnvelope,
  PaymentKey,
  SigningKey,
  UTxO,
  Value,
  VerificationKey,
  filterValue,
  getVerificationKey,
  readFileTextEnvelope,
 )
import "cardano-api" Cardano.Api.UTxO (totalValue)

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

-- | Filter and return any non-ADA assets as 'Left' if they are present in the 'UTxO' value.
checkNonADAAssetsUTxO :: UTxO -> Either Value ()
checkNonADAAssetsUTxO utxo =
  let nonADA = filterValue (/= AdaAssetId) $ totalValue utxo
   in if nonADA == mempty
        then Right ()
        else Left nonADA
