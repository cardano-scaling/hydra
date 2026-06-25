module Hydra.Cardano.Api.UTxO where

import Hydra.Cardano.Api.Prelude hiding (fromLedgerUTxO)
import Hydra.Cardano.Api.TxIn (txIns')
import Hydra.Cardano.Api.TxOut (parseTxOutFromJSON)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (outputsTxBodyL)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger
import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)

-- | Parse a 'UTxO' from JSON using 'parseTxOutFromJSON' to correctly handle
-- non-canonical inline datums. See 'parseTxOutFromJSON' for details.
parseUTxOFromJSON :: Aeson.Value -> Parser (UTxO Era)
parseUTxOFromJSON = Aeson.withObject "UTxO" $ \hm -> do
  pairs <- mapM parsePair (KeyMap.toList hm)
  pure $ UTxO.fromList pairs
 where
  parsePair :: (KeyMap.Key, Aeson.Value) -> Parser (TxIn, TxOut CtxUTxO Era)
  parsePair (k, txOutVal) = do
    txIn <- parseJSON (Aeson.String $ Key.toText k)
    txOut <- parseTxOutFromJSON txOutVal
    pure (txIn, txOut)

-- | Construct a UTxO from a transaction. This constructs artificial `TxIn`
-- (a.k.a output reference) from the transaction itself, zipping them to the
-- outputs they correspond to.
utxoFromTx :: Tx Era -> UTxO Era
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ ledgerBody ^. outputsTxBodyL
      txIns =
        [ Ledger.TxIn (toShelleyTxId $ getTxId body) ix
        | ix <- [Ledger.TxIx 0 ..]
        ]
   in UTxO.fromShelleyUTxO shelleyBasedEra $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

-- | Resolve tx inputs in a given UTxO
resolveInputsUTxO :: UTxO Era -> Tx Era -> UTxO Era
resolveInputsUTxO utxo tx =
  UTxO.fromList $
    mapMaybe (\txIn -> (txIn,) <$> UTxO.resolveTxIn txIn utxo) (txIns' tx)
