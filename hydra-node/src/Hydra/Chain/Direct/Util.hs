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
