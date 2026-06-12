{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simplistic type of transactions useful for modelling purpose.
-- a `Payment` is a simple transaction type that moves some amount of ADAs between
-- to `CardanoSigningKey`.
module Hydra.Model.Payment where

import Hydra.Cardano.Api hiding (getVerificationKey)
import Hydra.Prelude hiding (Any, label, toList)
import Test.Hydra.Prelude
import Text.Show qualified

import Data.List qualified as List
import Data.Set ((\\))
import Data.Set qualified as Set
import GHC.IsList (IsList (..))
import Hydra.Tx.Crypto (getVerificationKey)
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Secret (Secret, mkSecret)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (choose)

-- | New type wrapper to add 'Ord' and 'Eq' instances to signing keys. The
-- inner 'SigningKey PaymentKey' is wrapped in 'Secret': the @signingKey@
-- field never exposes the raw key, mirroring the way Hydra signing keys
-- are handled. The accompanying 'ToJSON' / 'FromJSON' / 'ToCBOR' /
-- 'FromCBOR' bans are inherited from 'Secret'.
newtype CardanoSigningKey = CardanoSigningKey {signingKey :: Secret (SigningKey PaymentKey)}

instance Show CardanoSigningKey where
  show (CardanoSigningKey sk) =
    "CardanoSigningKey " <> show (verificationKeyHash (getVerificationKey sk))

instance Eq CardanoSigningKey where
  CardanoSigningKey ska == CardanoSigningKey skb =
    verificationKeyHash (getVerificationKey ska)
      == verificationKeyHash (getVerificationKey skb)

instance Ord CardanoSigningKey where
  CardanoSigningKey a <= CardanoSigningKey b = hashOf a <= hashOf b
   where
    hashOf = verificationKeyHash . getVerificationKey

instance Arbitrary CardanoSigningKey where
  arbitrary = CardanoSigningKey . mkSecret . snd <$> genKeyPair

-- | 'CardanoSigningKey' wraps a 'Secret' internally, which makes any
-- JSON / CBOR access via the 'Secret' value a compile-time 'TypeError'.
-- 'IsTx Payment' below pulls in 'ToJSON' / 'FromJSON' / 'ToCBOR' /
-- 'FromCBOR' as superclasses of its transaction type, so we provide
-- placebo instances that error at runtime if accidentally called.
-- 'Payment' is purely a test-model type and is never serialised in
-- production.
instance ToJSON CardanoSigningKey where
  toJSON = error "don't use"

instance FromJSON CardanoSigningKey where
  parseJSON = error "don't use"

instance ToCBOR CardanoSigningKey where
  toCBOR = error "don't use"

instance FromCBOR CardanoSigningKey where
  fromCBOR = error "don't use"

-- | A single Ada-payment only transaction in our model.
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR Payment where
  toCBOR = error "don't use"

instance FromCBOR Payment where
  fromCBOR = error "don't use"

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

-- | Making `Payment` an instance of `IsTx` allows us to use it with `HeadLogic'`s messages.
instance IsTx Payment where
  type TxIdType Payment = Int
  type TxOutType Payment = (CardanoSigningKey, Value)
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
  outputsOfUTxO = id
  withoutUTxO a b =
    let as = second toList <$> a
        bs = second toList <$> b
        result = Set.toList $ Set.fromList as \\ Set.fromList bs
     in second fromList <$> result
  applyTxTo tx utxo = applyTx utxo tx
  filterUTxOByOutputs utxo outputs = filter (`Set.member` outputs) utxo
  removeOneOutputFromUTxO = List.delete
  utxoToElement = encodeUtf8 . show @Text

applyTx :: UTxOType Payment -> Payment -> UTxOType Payment
applyTx utxo Payment{from, to, value} =
  (to, value) : List.delete (from, value) utxo

genAdaValue :: Gen Value
genAdaValue = lovelaceToValue . fromInteger <$> choose (minimumUTxOAda, 10000000000)
 where
  -- NOTE: this should probably be retrieved from some authoritative source?
  minimumUTxOAda = 1000000

-- * Orphans

-- | Orphan 'Ord' instance for Cardano 'Value' using JSON encoding for comparison.
-- Needed to use '(CardanoSigningKey, Value)' as 'TxOutType Payment' in 'Set'.
instance Ord Value where
  compare x y = compare (toJSON x) (toJSON y)

instance Arbitrary Value where
  arbitrary = genAdaValue

instance ToCBOR Value where
  toCBOR = error "don't use"

instance FromCBOR Value where
  fromCBOR = error "don't use"
