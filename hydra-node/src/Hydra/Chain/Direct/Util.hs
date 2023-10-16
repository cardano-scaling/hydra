{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Chain.Direct.Util where

import Hydra.Prelude

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Ledger.Crypto (DSIGN)
import Hydra.Cardano.Api hiding (Block, SigningKey, VerificationKey)
import qualified Hydra.Cardano.Api as Shelley
import Hydra.Chain.CardanoClient (CardanoExtendedKeys, CardanoKeys, CardanoVKey)
import Ouroboros.Consensus.Cardano (CardanoBlock)

type Block = CardanoBlock StandardCrypto
type VerificationKey = Crypto.VerKeyDSIGN (DSIGN StandardCrypto)
type SigningKey = Crypto.SignKeyDSIGN (DSIGN StandardCrypto)

readKeyPair :: FilePath -> IO (Either CardanoKeys CardanoExtendedKeys)
readKeyPair keyPath = do
  sk <-
    asum
      [ Left <$> readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) keyPath
      , Right <$> readFileTextEnvelopeThrow (AsSigningKey AsPaymentExtendedKey) keyPath
      ]
  pure $ bimap (\a -> (getVerificationKey a, a)) (\a -> (getVerificationKey a, a)) sk

-- XXX: Should accept a 'File' path
readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO a
readFileTextEnvelopeThrow asType fileContents =
  either (fail . show) pure =<< readFileTextEnvelope asType (File fileContents)

readVerificationKey :: FilePath -> IO CardanoVKey
readVerificationKey fp =
  asum
    [ Left <$> readFileTextEnvelopeThrow (Shelley.AsVerificationKey Shelley.AsPaymentKey) fp
    , Right <$> readFileTextEnvelopeThrow (Shelley.AsVerificationKey Shelley.AsPaymentExtendedKey) fp
    ]

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
