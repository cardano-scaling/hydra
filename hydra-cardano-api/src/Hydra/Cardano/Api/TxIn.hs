module Hydra.Cardano.Api.TxIn where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.TxIn as Ledger

-- * Extras

-- | Create a 'TxIn' (a.k.a UTXO) from a transaction and output index.
mkTxIn :: Tx era -> Word -> TxIn
mkTxIn (getTxId . getTxBody -> txId) index =
  TxIn txId (TxIx index)

-- | Access inputs of a transaction, as an ordered list.
txIns' :: Tx era -> [TxIn]
txIns' (getTxBody -> txBody) =
  let TxBody TxBodyContent{txIns} = txBody
   in fst <$> txIns

-- * Type Conversions

-- | Convert a cardano-ledger 'TxIn' to a cardano-api 'TxIn'
fromLedgerTxIn :: Ledger.TxIn StandardCrypto -> TxIn
fromLedgerTxIn = fromShelleyTxIn

-- | Convert a cardano-api 'TxIn' to a cardano-ledger 'TxIn'
toLedgerTxIn :: TxIn -> Ledger.TxIn StandardCrypto
toLedgerTxIn = toShelleyTxIn
