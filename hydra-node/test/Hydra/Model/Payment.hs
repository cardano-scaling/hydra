{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simplistic type of transactions useful for modelling purpose.
-- a `Payment` is a simple transaction type that moves some amount of ADAs between
-- to `CardanoSigningKey`.
module Hydra.Model.Payment where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label, toList)

import Data.List qualified as List
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (choose)
import Prelude qualified

-- NOTE: New type wrapper to add Ord and Eq instances to signing keys
newtype CardanoSigningKey = CardanoSigningKey {signingKey :: SigningKey PaymentKey}

instance Show CardanoSigningKey where
  show CardanoSigningKey{signingKey} =
    show . mkVkAddress @Era testNetworkId . getVerificationKey $ signingKey

instance Eq CardanoSigningKey where
  CardanoSigningKey ska == CardanoSigningKey skb =
    verificationKeyHash (getVerificationKey ska) == verificationKeyHash (getVerificationKey skb)

instance Ord CardanoSigningKey where
  CardanoSigningKey a <= CardanoSigningKey b = hashOf a <= hashOf b
   where
    hashOf = verificationKeyHash . getVerificationKey

instance Arbitrary CardanoSigningKey where
  arbitrary = CardanoSigningKey . snd <$> genKeyPair

-- | A single Ada-payment only transaction in our model.
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
  deriving stock (Eq, Generic)

instance Show Payment where
  -- NOTE: We display derived addresses instead of raw signing keys in order to help troubleshooting
  -- tests failures or errors.
  show Payment{from, to, value} =
    "Payment { from = "
      <> show from
      <> ", to = "
      <> show to
      <> ", value = "
      <> show value
      <> " }"

type PaymentUTxO = [(CardanoSigningKey, Value)]

applyTx :: PaymentUTxO -> Payment -> PaymentUTxO
applyTx utxo Payment{from, to, value} =
  (to, value) : List.delete (from, value) utxo

genAdaValue :: Gen Value
genAdaValue = lovelaceToValue . fromInteger <$> choose (minimumUTxOAda, 10000000000)
 where
  -- NOTE: this should probably be retrieved from some authoritative source?
  minimumUTxOAda = 1000000

-- * Orphans
instance Arbitrary Value where
  arbitrary = genAdaValue
