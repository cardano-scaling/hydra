module Hydra.Utils where

import qualified Control.Foldl as L
import Control.Monad.Freer (run)
import Control.Monad.Freer.Extras (LogLevel (Info))
import Control.Monad.Freer.Writer (tell)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Void (Void)
import Ledger (Address, DatumHash, TxOutTx (txOutTxOut), txOutDatum)
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract.Test (TracePredicate)
import Plutus.Trace (EmulatorTrace, defaultEmulatorConfig, runEmulatorStream, walletInstanceTag)
import PlutusTx
import PlutusTx.Prelude hiding (trace)
import qualified Streaming.Prelude as S
import Test.Tasty
import Test.Tasty.Golden
import Wallet.Emulator (Wallet)
import Wallet.Emulator.Folds (postMapM)
import qualified Wallet.Emulator.Folds as Folds
import Wallet.Emulator.Stream (filterLogLevel, foldEmulatorStreamM)

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

datum :: UtxoMap -> [DatumHash]
datum utxoMap =
  catMaybes $ Map.elems $ Map.map (txOutDatum . txOutTxOut) utxoMap

checkCompiledContractPIR :: FilePath -> CompiledCode a -> TestTree
checkCompiledContractPIR path code = goldenVsString "PIR" path (return $ fromString $ show $ pretty $ fromJust $ getPir code)

renderWalletLog :: Wallet -> EmulatorTrace () -> Text
renderWalletLog w1 trace =
  let result =
        run $
          foldEmulatorStreamM (L.generalize $ Folds.instanceLog (walletInstanceTag w1)) $
            filterLogLevel Info $
              runEmulatorStream defaultEmulatorConfig trace
   in renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ pretty <$> S.fst' result

-- renderEmulatorLog :: EmulatorTrace () -> ByteString
-- renderEmulatorLog trace =
--     let result =
--             run
--             $ foldEmulatorStreamM (L.generalize Folds.emulatorLog)
--             $ filterLogLevel Info
--             $ Trace.runEmulatorStream Trace.defaultEmulatorConfig trace
--     in BSL.fromStrict $ T.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ fmap pretty $ S.fst' result
