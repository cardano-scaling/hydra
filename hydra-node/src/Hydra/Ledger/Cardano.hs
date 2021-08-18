{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano where

import Hydra.Prelude hiding (id)

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary (MaryEra)
import Hydra.Ledger (Tx (..))
import qualified Shelley.Spec.Ledger.API as Cardano
import Text.Read (readPrec)

type CardanoEra = MaryEra StandardCrypto

data CardanoTx = CardanoTx
  { id :: Cardano.TxId StandardCrypto
  , body :: CardanoTxBody
  , witnesses :: CardanoTxWitnesses
  }
  deriving stock (Eq, Show, Generic)

instance Read CardanoTx where
  readPrec = error "Read: CardanoTx"

instance Read (Cardano.UTxO era) where
  readPrec = error "Read: Cardano.UTxO"

instance Monoid (Cardano.UTxO era)
instance Semigroup (Cardano.UTxO era)

instance ToJSON CardanoTx where
  toJSON = error "toJSON: CardanoTx"

instance FromJSON CardanoTx where
  parseJSON = error "parseJSON: CardanoTx"

instance ToJSON (Cardano.UTxO era) where
  toJSON = error "toJSON: Cardano.UTxO"

instance FromJSON (Cardano.UTxO era) where
  parseJSON = error "parseJSON: Cardano.UTxO"

instance Read (Cardano.TxId era) where
  readPrec = error "Read: Cardano.TxId"

instance ToJSON (Cardano.TxId era) where
  toJSON = error "toJSON: Cardano.TxId"

instance FromJSON (Cardano.TxId era) where
  parseJSON = error "parseJSON: Ledger.TxId"

type CardanoTxBody = Cardano.TxBody CardanoEra

type CardanoTxWitnesses = Cardano.WitnessSet CardanoEra

instance Tx CardanoTx where
  type Utxo CardanoTx = Cardano.UTxO CardanoEra
  type TxId CardanoTx = Cardano.TxId StandardCrypto

  txId = id
