module Hydra.Cardano.Api.UTxO where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxId (toLedgerTxId)
import Hydra.Cardano.Api.TxIn (txIns')

import Cardano.Api (Tx (..), TxBody (..), getTxId, shelleyBasedEra)
import Cardano.Api.Tx.UTxO qualified as Api.UTxO
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (outputsTxBodyL)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger
import Control.Lens ((^.))
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Text qualified as Text

-- | Get a human-readable pretty text representation of a UTxO.
renderUTxO :: IsString str => UTxO -> str
renderUTxO =
  fromString . Text.unpack . Text.intercalate "\n" . fmap UTxO.render . UTxO.toList

-- | Construct a UTxO from a transaction. This constructs artificial `TxIn`
-- (a.k.a output reference) from the transaction itself, zipping them to the
-- outputs they correspond to.
utxoFromTx :: Tx Era -> UTxO
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ ledgerBody ^. outputsTxBodyL
      txIns =
        [ Ledger.TxIn (toLedgerTxId $ getTxId body) ix
        | ix <- [Ledger.TxIx 0 .. toEnum (length txOuts)]
        ]
   in fromLedgerUTxO $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

-- | Resolve tx inputs in a given UTxO
resolveInputsUTxO :: UTxO -> Tx Era -> UTxO
resolveInputsUTxO utxo tx =
  UTxO.fromList $
    mapMaybe (\txIn -> (txIn,) <$> UTxO.resolveTxIn txIn utxo) (txIns' tx)

-- * Type Conversions

toLedgerUTxO :: UTxO -> Ledger.UTxO LedgerEra
toLedgerUTxO = Api.UTxO.toShelleyUTxO shelleyBasedEra . UTxO.toApi

fromLedgerUTxO :: Ledger.UTxO LedgerEra -> UTxO
fromLedgerUTxO = UTxO.fromApi . Api.UTxO.fromShelleyUTxO shelleyBasedEra
