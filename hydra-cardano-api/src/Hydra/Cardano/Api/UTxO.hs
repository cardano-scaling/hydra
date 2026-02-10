module Hydra.Cardano.Api.UTxO where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude hiding (fromLedgerUTxO)
import "hydra-cardano-api" Hydra.Cardano.Api.TxIn (txIns')

import "base" Data.Foldable (toList)
import "base" Data.Maybe (mapMaybe)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-api" Cardano.Ledger.Api (outputsTxBodyL)
import "cardano-ledger-core" Cardano.Ledger.BaseTypes qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.TxIn qualified as Ledger
import "cardano-ledger-shelley" Cardano.Ledger.Shelley.UTxO qualified as Ledger
import "containers" Data.Map qualified as Map
import "lens" Control.Lens ((^.))

-- | Construct a UTxO from a transaction. This constructs artificial `TxIn`
-- (a.k.a output reference) from the transaction itself, zipping them to the
-- outputs they correspond to.
utxoFromTx :: Tx Era -> UTxO Era
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ ledgerBody ^. outputsTxBodyL
      txIns =
        [ Ledger.TxIn (toShelleyTxId $ getTxId body) ix
        | ix <- [Ledger.TxIx 0 .. toEnum (length txOuts)]
        ]
   in UTxO.fromShelleyUTxO shelleyBasedEra $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

-- | Resolve tx inputs in a given UTxO
resolveInputsUTxO :: UTxO Era -> Tx Era -> UTxO Era
resolveInputsUTxO utxo tx =
  UTxO.fromList $
    mapMaybe (\txIn -> (txIn,) <$> UTxO.resolveTxIn txIn utxo) (txIns' tx)
