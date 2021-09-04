{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Ledger where

import Hydra.Prelude

class
  ( Eq tx
  , Eq (TxId tx)
  , Eq (Utxo tx)
  , FromCBOR tx
  , FromJSON (TxId tx)
  , FromJSON (Utxo tx)
  , FromJSON tx
  , Monoid (Utxo tx)
  , Ord (TxId tx)
  , Show (TxId tx)
  , Show (Utxo tx)
  , Show tx
  , ToCBOR tx
  , ToJSON (TxId tx)
  , ToJSON (Utxo tx)
  , ToJSON tx
  , Typeable (TxId tx)
  , Typeable tx
  ) =>
  Tx tx
  where
  type Utxo tx
  type TxId tx
  type AssetId tx

  txId :: tx -> TxId tx

  balance :: Utxo tx -> Balance tx

data Balance tx = Balance
  { lovelace :: Natural
  , assets :: Map (AssetId tx) Integer
  }

data Ledger tx = Ledger
  { applyTransactions :: Utxo tx -> [tx] -> Either ValidationError (Utxo tx)
  , initUtxo :: Utxo tx
  }

canApply :: Ledger tx -> Utxo tx -> tx -> ValidationResult
canApply ledger utxo tx =
  either Invalid (const Valid) $ applyTransactions ledger utxo (pure tx)

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ValidationError = ValidationError {reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary
