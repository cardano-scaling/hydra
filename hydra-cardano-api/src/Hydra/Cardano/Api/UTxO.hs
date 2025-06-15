module Hydra.Cardano.Api.UTxO where

import Hydra.Cardano.Api.Prelude hiding (fromLedgerUTxO)
import Hydra.Cardano.Api.TxId (toLedgerTxId)
import Hydra.Cardano.Api.TxIn (txIns')

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (outputsTxBodyL)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger
import Control.Lens ((^.))
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

-- | Construct a UTxO from a transaction. This constructs artificial `TxIn`
-- (a.k.a output reference) from the transaction itself, zipping them to the
-- outputs they correspond to.
utxoFromTx :: Tx Era -> UTxO Era
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ ledgerBody ^. outputsTxBodyL
      txIns =
        [ Ledger.TxIn (toLedgerTxId $ getTxId body) ix
        | ix <- [Ledger.TxIx 0 .. toEnum (length txOuts)]
        ]
   in UTxO.fromShelleyUTxO shelleyBasedEra $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

-- | Resolve tx inputs in a given UTxO
resolveInputsUTxO :: UTxO Era -> Tx Era -> UTxO Era
resolveInputsUTxO utxo tx =
  UTxO.fromList $
    mapMaybe (\txIn -> (txIn,) <$> UTxO.resolveTxIn txIn utxo) (txIns' tx)
