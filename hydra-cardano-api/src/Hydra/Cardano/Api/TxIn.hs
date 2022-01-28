module Hydra.Cardano.Api.TxIn where

import Cardano.Api
import Hydra.Prelude

import Cardano.Api.Shelley (fromShelleyTxIn, toShelleyTxIn)
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.TxIn as Ledger

-- * Type Conversions

-- | Convert a cardano-ledger 'TxIn' to a cardano-api 'TxIn'
fromLedgerTxIn :: Ledger.TxIn StandardCrypto -> TxIn
fromLedgerTxIn = fromShelleyTxIn

-- | Convert a cardano-api 'TxIn' to a cardano-ledger 'TxIn'
toLedgerTxIn :: TxIn -> Ledger.TxIn StandardCrypto
toLedgerTxIn = toShelleyTxIn

-- * Extras

-- | Create a 'TxIn' from a transaction body and index.
mkTxIn :: TxBody era -> Word -> TxIn
mkTxIn txBody index = TxIn (getTxId txBody) (TxIx index)
