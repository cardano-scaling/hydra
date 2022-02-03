module Hydra.Cardano.Api.TxIn where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Plutus.V1.Ledger.Api as Plutus

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

-- | Convert a cardano-ledger 'TxIn' into a cardano-api 'TxIn'
fromLedgerTxIn :: Ledger.TxIn StandardCrypto -> TxIn
fromLedgerTxIn = fromShelleyTxIn

-- | Convert a cardano-api 'TxIn' into a cardano-ledger 'TxIn'
toLedgerTxIn :: TxIn -> Ledger.TxIn StandardCrypto
toLedgerTxIn = toShelleyTxIn

-- | Convert a plutus' 'TxOutRef' into a cardano-api 'TxIn'
fromPlutusTxOutRef :: Plutus.TxOutRef -> TxIn
fromPlutusTxOutRef (Plutus.TxOutRef (Plutus.TxId bytes) ix) =
  TxIn
    (TxId $ unsafeHashFromBytes $ Plutus.fromBuiltin bytes)
    (TxIx $ fromIntegral ix)

-- | Convert a cardano-api's 'TxIn' into a plutus' 'TxOutRef'.
toPlutusTxOutRef :: TxIn -> Plutus.TxOutRef
toPlutusTxOutRef = Ledger.txInfoIn' . toLedgerTxIn
