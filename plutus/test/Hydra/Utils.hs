module Hydra.Utils where

import qualified Control.Foldl as L
import Control.Monad.Freer.Writer (tell)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Text.Prettyprint.Doc
import Data.Void (Void)
import Ledger (Address, Datum (Datum), DatumHash, TxOutTx (txOutTxOut), datumHash, txOutDatum)
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract.Test (TracePredicate)
import PlutusTx
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.Golden
import Wallet.Emulator.Folds (postMapM)
import qualified Wallet.Emulator.Folds as Folds

-- | Check that the given script address has some `DatumHash`.
datumAtAddress :: Address -> DatumHash -> TracePredicate
datumAtAddress address expected =
  flip postMapM (L.generalize $ Folds.utxoAtAddress address) $ \utxo -> do
    let datums = datum utxo
    if expected `elem` datums
      then return True
      else do
        tell @(Doc Void)
          ( "Expected datum with hash"
              <+> pretty expected
              <+> ", got datums at address"
              <+> pretty address
              <+> ":"
              <+> pretty datums
          )
        return False

toDatumHash :: IsData a => a -> DatumHash
toDatumHash = datumHash . Datum . toData

datum :: UtxoMap -> [DatumHash]
datum utxoMap =
  catMaybes $ Map.elems $ Map.map (txOutDatum . txOutTxOut) utxoMap

checkCompiledContractPIR :: FilePath -> CompiledCode a -> TestTree
checkCompiledContractPIR path code = goldenVsString "PIR" path (return $ fromString $ show $ pretty $ fromJust $ getPir code)
