module Hydra.Cardano.Api.UTxO where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxId (toLedgerTxId)
import Hydra.Cardano.Api.TxIn (fromLedgerTxIn, toLedgerTxIn)
import Hydra.Cardano.Api.TxOut (fromLedgerTxOut, toLedgerTxOut)

import qualified Cardano.Api.UTxO as UTxO
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Map as Map

-- | Get a human-readable pretty text representation of a UTxO.
renderUTxO :: (TxIn, TxOut ctx era) -> Text
renderUTxO = UTxO.render

-- | Construct a UTxO from a transaction. This constructs artificial `TxIn`
-- (a.k.a output reference) from the transaction itself, zipping them to the
-- outputs they correspond to.
utxoFromTx :: Tx Era -> UTxO
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ Ledger.outputs' ledgerBody
      txIns =
        [ Ledger.TxIn (toLedgerTxId $ getTxId body) ix
        | ix <- [0 .. fromIntegral (length txOuts)]
        ]
   in fromLedgerUTxO $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

-- * Type Conversions

toLedgerUTxO :: UTxO -> Ledger.UTxO LedgerEra
toLedgerUTxO =
  Ledger.UTxO . Map.foldMapWithKey fn . UTxO.toMap
 where
  fn ::
    TxIn ->
    TxOut CtxUTxO Era ->
    Map (Ledger.TxIn StandardCrypto) (Ledger.TxOut LedgerEra)
  fn i o =
    Map.singleton (toLedgerTxIn i) (toLedgerTxOut o)

fromLedgerUTxO :: Ledger.UTxO LedgerEra -> UTxO
fromLedgerUTxO =
  UTxO . Map.foldMapWithKey fn . Ledger.unUTxO
 where
  fn ::
    Ledger.TxIn StandardCrypto ->
    Ledger.TxOut LedgerEra ->
    Map TxIn (TxOut CtxUTxO Era)
  fn i o =
    Map.singleton (fromLedgerTxIn i) (fromLedgerTxOut o)
