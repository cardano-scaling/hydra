{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simplistic type of transactions useful for modelling purpose.
-- a `Payment` is a simple transaction type that moves some amount of ADAs between
-- to `CardanoSigningKey`.
module Hydra.Model.Payment where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Data.List qualified as List
import Data.Set ((\\))
import Data.Set qualified as Set
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (genKeyPair)
import Test.QuickCheck (choose)
import Test.QuickCheck.StateModel (HasVariables)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))
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

instance ToJSON CardanoSigningKey where
  toJSON = error "don't use"

instance FromJSON CardanoSigningKey where
  parseJSON = error "don't use"

instance ToCBOR CardanoSigningKey where
  toCBOR = error "don't use"

instance FromCBOR CardanoSigningKey where
  fromCBOR = error "don't use"

instance Arbitrary CardanoSigningKey where
  arbitrary = CardanoSigningKey . snd <$> genKeyPair

-- | A single Ada-payment only transaction in our model.
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

instance Arbitrary Payment where
  arbitrary = error "don't use"

instance ToCBOR Payment where
  toCBOR = error "don't use"

instance FromCBOR Payment where
  fromCBOR = error "don't use"

instance HasVariables Payment where
  getAllVariables _ = mempty

-- | Making `Payment` an instance of `IsTx` allows us to use it with `HeadLogic'`s messages.
instance IsTx Payment where
  type TxIdType Payment = Int
  type UTxOType Payment = [(CardanoSigningKey, Value)]
  type ValueType Payment = Value
  txId = error "undefined"
  balance = foldMap snd
  hashUTxO = encodeUtf8 . show @Text
  txSpendingUTxO = \case
    [] -> error "nothing to spend spending"
    [(from, value)] -> Payment{from, to = from, value}
    _ -> error "cant spend from multiple utxo in one payment"
  utxoFromTx Payment{to, value} = [(to, value)]
  withoutUTxO a b =
    let as = bimap id valueToList <$> a
        bs = bimap id valueToList <$> b
        result = Set.toList $ Set.fromList as \\ Set.fromList bs
     in bimap id valueFromList <$> result

applyTx :: UTxOType Payment -> Payment -> UTxOType Payment
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

instance ToCBOR Value where
  toCBOR = error "don't use"

instance FromCBOR Value where
  fromCBOR = error "don't use"
