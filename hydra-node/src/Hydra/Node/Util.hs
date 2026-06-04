{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Node.Util where

import Hydra.Prelude

import Cardano.Api.UTxO (totalValue)
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
  readFileTextEnvelope,
 )
import Hydra.Tx.Crypto (getVerificationKey)
import Hydra.Tx.Secret (Secret, mkSecret)

-- | Read a 'SigningKey PaymentKey' from a text-envelope file and return it
-- wrapped in 'Secret'. The verification key is the public projection so
-- stays unwrapped.
readKeyPair :: FilePath -> IO (VerificationKey PaymentKey, Secret (SigningKey PaymentKey))
readKeyPair keyPath = do
  sk <- readFileTextEnvelopeThrow keyPath
  pure (getVerificationKey sk, mkSecret sk)

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
