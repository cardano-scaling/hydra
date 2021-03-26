{-# LANGUAGE TemplateHaskell #-}

module Hydra.ContractSpec where

--import qualified Control.Foldl                         as L
--import           Control.Lens                          ((&), (.~))
-- import           Control.Monad                         (void)

-- import           Control.Monad.Freer.Extras.Log        (LogLevel (..))
-- import           Data.ByteString.Lazy                  (ByteString)
-- import qualified Data.ByteString.Lazy                  as BSL
-- import qualified Data.Text.Encoding                    as T
-- import           Data.Text.Prettyprint.Doc             (Pretty (..), defaultLayoutOptions, layoutPretty, vsep)
-- import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
-- import           Spec.Lib                              (timesFeeAdjust)

-- import           Test.Tasty.Golden                     (goldenVsString)
-- import qualified Test.Tasty.HUnit                      as HUnit

-- import qualified Ledger.Ada                            as Ada
-- import           Ledger.Slot                           (Slot (..))
-- import           Plutus.Trace.Emulator                 (ContractHandle (..), EmulatorTrace)

-- import qualified PlutusTx.Prelude                      as PlutusTx
--import qualified Streaming.Prelude                     as S
-- import qualified Wallet.Emulator.Folds                 as Folds
-- import           Wallet.Emulator.Stream                (filterLogLevel, foldEmulatorStreamM)

import Cardano.Prelude
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Data.String (String)
import Hydra.Contract
import Hydra.Utils (checkCompiledContractPIR)
import Ledger (Slot (Slot), ValidatorCtx)
import qualified Ledger.Ada as Ada
import Plutus.Contract hiding (runError)
import Plutus.Contract.Test
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl)
import Plutus.Trace.Effects.RunContract (RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import Test.Tasty

w1 :: Wallet
w1 = Wallet 1

theContract :: Contract () Schema ContractError ()
theContract = hydraHead

{- ORMOLU_DISABLE -}
compiledScript :: PlutusTx.CompiledCode (Datum -> Redeemer -> ValidatorCtx -> Bool)
compiledScript = $$(PlutusTx.compile [|| validate ||])
{- ORMOLU_ENABLE -}

tests :: TestTree
tests =
  testGroup
    "Contract Behaviour"
    [ checkCompiledContractPIR "test/Hydra/Contract.pir" compiledScript
    , checkPredicate
        "Expose 'collectCom' and 'close' endpoints"
        ( endpointAvailable @"collectCom" theContract (Trace.walletInstanceTag w1)
            .&&. endpointAvailable @"close" theContract (Trace.walletInstanceTag w1)
        )
        $ void (Trace.activateContractWallet w1 theContract)
    , checkPredicate
        "Close state after CollectCom"
        assertNoFailedTransactions
        collectAndClose
    ]

collectAndClose :: Eff '[RunContract, Waiting, EmulatorControl, EmulatedWalletAPI, LogMsg String] ()
collectAndClose = do
  callCollectCom
  void $ Trace.waitUntilSlot (Slot 10)
  callClose
  void $ Trace.waitUntilSlot (Slot 20)

callCollectCom :: Trace.EmulatorTrace ()
callCollectCom = do
  contractHandle <- Trace.activateContractWallet w1 (hydraHead @ContractError)
  Trace.callEndpoint @"collectCom" contractHandle (CollectComParams $ Ada.lovelaceValueOf 42)

callClose :: Trace.EmulatorTrace ()
callClose = do
  contractHandle <- Trace.activateContractWallet w1 (hydraHead @ContractError)
  Trace.callEndpoint @"close" contractHandle ()

-- renderWalletLog :: EmulatorTrace () -> ByteString
-- renderWalletLog trace =
--     let result =
--             run
--             $ foldEmulatorStreamM (L.generalize $ Folds.instanceLog (Trace.walletInstanceTag w1))
--             $ filterLogLevel Info
--             $ Trace.runEmulatorStream Trace.defaultEmulatorConfig trace
--     in BSL.fromStrict $ T.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ fmap pretty $ S.fst' result

-- renderEmulatorLog :: EmulatorTrace () -> ByteString
-- renderEmulatorLog trace =
--     let result =
--             run
--             $ foldEmulatorStreamM (L.generalize Folds.emulatorLog)
--             $ filterLogLevel Info
--             $ Trace.runEmulatorStream Trace.defaultEmulatorConfig trace
--     in BSL.fromStrict $ T.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ fmap pretty $ S.fst' result
