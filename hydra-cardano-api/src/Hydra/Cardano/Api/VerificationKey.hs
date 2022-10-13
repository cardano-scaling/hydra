{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.VerificationKey where

import Hydra.Cardano.Api.Prelude

-- * Orphans

-- XXX: This is quite specific to payment keys

instance ToJSON (VerificationKey PaymentKey) where
  toJSON = toJSON . serialiseToTextEnvelope Nothing

instance FromJSON (VerificationKey PaymentKey) where
  parseJSON v = do
    env <- parseJSON v
    case deserialiseFromTextEnvelope (AsVerificationKey AsPaymentKey) env of
      Left e -> fail $ show e
      Right a -> pure a
