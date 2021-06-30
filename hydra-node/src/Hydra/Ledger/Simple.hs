-- | A mock implementation of a ledger slightly less dumb than 'Mock'.
--
-- This implementation of a 'Ledger' adds a bit more logic in order to:
--
-- * Be able to have a representation of 'UTxO' closer to what a real-life eUTxO would be,
--   so that we can distinguish it from other components of the ledger,
-- * Be able to have transactions validated against the current state of the ledger, so that
--   one transaction could at some point be invalid, then becomes valid because some inputs it
--   consumes is now available.
module Hydra.Ledger.Simple where

import Hydra.Prelude

import Data.Aeson (
  object,
  withObject,
  (.:),
  (.=),
 )
import qualified Data.Set as Set
import Hydra.Ledger

instance Tx SimpleTx where
  type UTxO SimpleTx = Set TxIn
  type TxId SimpleTx = SimpleId

  txId (SimpleTx tid _ _) = tid

-- | Simple transaction.
-- A transaction is a 'SimpleId', a list of inputs and a list of outputs.
data SimpleTx = SimpleTx
  { txSimpleId :: SimpleId
  , txInputs :: UTxO SimpleTx
  , txOutputs :: UTxO SimpleTx
  }
  deriving stock (Eq, Ord, Generic, Read, Show)

instance Arbitrary SimpleTx where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance ToJSON SimpleTx where
  toJSON tx =
    object
      [ "id" .= txId tx
      , "inputs" .= txInputs tx
      , "outputs" .= txOutputs tx
      ]

instance FromJSON SimpleTx where
  parseJSON = withObject "SimpleTx" $ \obj ->
    SimpleTx
      <$> (obj .: "id")
      <*> (obj .: "inputs")
      <*> (obj .: "outputs")

instance ToCBOR SimpleTx where
  toCBOR (SimpleTx txid inputs outputs) =
    toCBOR txid <> toCBOR inputs <> toCBOR outputs

instance FromCBOR SimpleTx where
  fromCBOR = SimpleTx <$> fromCBOR <*> fromCBOR <*> fromCBOR

type SimpleId = Integer

-- |An identifier for a single output of a 'SimpleTx'.
newtype TxIn = TxIn {unTxIn :: Integer}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Read, Show, ToJSON, FromJSON)

instance Arbitrary TxIn where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance ToCBOR TxIn where
  toCBOR (TxIn inId) = toCBOR inId

instance FromCBOR TxIn where
  fromCBOR = TxIn <$> fromCBOR

simpleLedger :: Ledger SimpleTx
simpleLedger =
  Ledger
    { applyTransactions =
        foldlM $ \utxo (SimpleTx _ ins outs) ->
          if ins `Set.isSubsetOf` utxo && utxo `Set.disjoint` outs
            then Right $ (utxo Set.\\ ins) `Set.union` outs
            else Left ValidationError
    , initUTxO = mempty
    }
