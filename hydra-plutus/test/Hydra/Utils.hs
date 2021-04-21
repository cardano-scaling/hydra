module Hydra.Utils where

import Cardano.Prelude

import Control.Monad.Freer.Writer (tell)
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Ledger (Datum(..), Address, DatumHash, TxOutTx (txOutTxOut), txOutDatum, datumHash)
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract.Test (TracePredicate)
import PlutusTx
import PlutusTx.Prelude hiding (trace)
import Test.Tasty
import Test.Tasty.Golden
import Wallet.Emulator.Folds (postMapM)
import Wallet.Emulator.MultiAgent (EmulatorTimeEvent(..))
import Plutus.Trace.Emulator.Types(UserThreadMsg (..))

import qualified Control.Foldl as L
import qualified Data.Map as Map
import qualified Wallet.Emulator.Folds as Folds

-- | Check that the given script address has some `DatumHash`.
datumAtAddress :: IsData a => Address -> a -> TracePredicate
datumAtAddress address expected =
  flip postMapM (L.generalize $ Folds.utxoAtAddress address) $ \utxo -> do
    let datums = datum utxo
        expectedDatum = datumHash $ Datum $ toData expected
    if expectedDatum `elem` datums
      then return True
      else do
        tell @(Doc Void)
          ( "Expected datum with hash"
              <+> pretty expectedDatum
              <+> ", got datums at address"
              <+> pretty address
              <+> ":"
              <+> pretty datums
          )
        return False

datum :: UtxoMap -> [DatumHash]
datum utxoMap =
  catMaybes $ Map.elems $ Map.map (txOutDatum . txOutTxOut) utxoMap

assertLastLog :: String -> TracePredicate
assertLastLog expected = do
  flip postMapM (L.generalize Folds.userLog) $ \logs ->
    case reverse logs of
      (EmulatorTimeEvent _ (UserLog got)):_ -> return (got == expected)
      _ -> return False
