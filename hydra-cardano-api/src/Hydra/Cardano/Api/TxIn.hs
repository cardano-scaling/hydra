{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.TxIn where

import Hydra.Cardano.Api.Prelude

import "cardano-ledger-core" Cardano.Ledger.BaseTypes qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Plutus (transSafeHash)
import "cardano-ledger-core" Cardano.Ledger.TxIn qualified as Ledger
import "containers" Data.Set qualified as Set
import "plutus-ledger-api" PlutusLedgerApi.V3 qualified as Plutus

-- * Extras

-- | Create a 'TxIn' (a.k.a UTXO) from a transaction and output index.
mkTxIn :: Tx era -> Word -> TxIn
mkTxIn (getTxId . getTxBody -> a) index =
  TxIn a (TxIx index)

-- | Attach some verification-key witness to a 'TxIn'.
withWitness :: TxIn -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn Era))
withWitness txIn =
  (txIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)

-- | Access inputs of a transaction, as an ordered list.
txIns' :: Tx era -> [TxIn]
txIns' (getTxBodyContent . getTxBody -> txBodyContent) =
  let TxBodyContent{txIns} = txBodyContent
   in fst <$> txIns

-- | Access inputs of a transaction, as an ordered set.
txInputSet :: Tx era -> Set TxIn
txInputSet = Set.fromList . txIns'

-- * Type Conversions

-- | Convert a cardano-ledger 'TxIn' into a cardano-api 'TxIn'
fromLedgerTxIn :: Ledger.TxIn -> TxIn
fromLedgerTxIn = fromShelleyTxIn

-- | Convert a cardano-api 'TxIn' into a cardano-ledger 'TxIn'
toLedgerTxIn :: TxIn -> Ledger.TxIn
toLedgerTxIn = toShelleyTxIn

-- | Convert a plutus' 'TxOutRef' into a cardano-api 'TxIn'
fromPlutusTxOutRef :: Plutus.TxOutRef -> TxIn
fromPlutusTxOutRef (Plutus.TxOutRef (Plutus.TxId bytes) ix) =
  TxIn
    (TxId $ unsafeHashFromBytes $ Plutus.fromBuiltin bytes)
    (TxIx $ fromIntegral ix)

-- | Convert a cardano-api 'TxIn' into a plutus 'TxOutRef'.
toPlutusTxOutRef :: TxIn -> Plutus.TxOutRef
toPlutusTxOutRef txIn =
  -- XXX: The upstream 'transTxIn' works only with the PlutusV1 type, so we
  -- needed to vendor its definition here.
  let (Ledger.TxIn (Ledger.TxId safe) txIx) = toLedgerTxIn txIn
   in Plutus.TxOutRef (Plutus.TxId $ transSafeHash safe) (toInteger $ Ledger.txIxToInt txIx)
