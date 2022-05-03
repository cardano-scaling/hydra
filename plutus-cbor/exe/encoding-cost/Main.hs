{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

import           Hydra.Prelude                            hiding (label)

import           Control.Exception                        (throw)
import           Control.Monad.Except
import           Data.Binary.Builder                      (toLazyByteString)
import           Data.ByteString.Builder.Scientific       (FPFormat (Fixed),
                                                           formatScientificBuilder)
import           Data.Ratio                               ((%))
import           Data.Scientific                          (unsafeFromRational)
import qualified Ledger.Typed.Scripts                     as Scripts
import           Plutus.Codec.CBOR.Encoding.Validator     (EncodeValidator,
                                                           ValidatorKind (..),
                                                           encodeTxOutValidator,
                                                           encodeTxOutValidator2,
                                                           encodeTxOutsValidator,
                                                           encodeTxOutsValidator2)
import qualified Plutus.V1.Ledger.Api                     as Plutus
import qualified PlutusCore                               as PLC
import           Test.Plutus.Validator                    (ExUnits (..),
                                                           distanceExUnits,
                                                           evaluateScriptExecutionUnits)
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           Test.QuickCheck                          (vectorOf)

import           Text.Printf                              (printf)


import           Terms
import           TxOutGen

maxExecutionUnits :: ExUnits
maxExecutionUnits =  ExUnits
    { exUnitsMem   = 10_000_000
    , exUnitsSteps = 10_000_000_000
    }


main :: IO ()
main = do
  printf "\n"
  printf "Full validations\n"
  printf "================\n\n"

  printf "# List of ADA-only TxOut; Scott-encoded TxOut serialised using encoder on-chain.\n"
  printf "   n         mem         cpu        %%mem      %%cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (vectorOf n genAdaOnlyTxOut) 42
    let (mem, cpu) = relativeCostOf x maxExecutionUnits encodeTxOutsValidator
    -- encodingToBuiltinByteString (encodeList encodeTxOut xs)
    case evaluateScriptExecutionUnits (encodeTxOutsValidator RealValidator) x of
      Right (ExUnits absMem absCpu) ->
           printf "%4d %12d %12d %s%% %s%%\n" n absMem absCpu (rationalToPercent mem) (rationalToPercent cpu)
      Left e -> printf "ERROR: %s\n" e

  printf "\n"

  printf "# List of ADA-only TxOut; serialised on-chain using serialiseData . toBuiltinData.\n"
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (vectorOf n genAdaOnlyTxOut) 42
    let (mem, cpu) = relativeCostOf x maxExecutionUnits encodeTxOutsValidator2
    -- Plutus.serialiseData (Plutus.toBuiltinData xs)
    case evaluateScriptExecutionUnits (encodeTxOutsValidator2 RealValidator) x of
      Right (ExUnits absMem absCpu) ->
           printf "%4d %12d %12d %s%% %s%%\n" n absMem absCpu (rationalToPercent mem) (rationalToPercent cpu)
      Left e -> printf "ERROR: %s\n" e

  printf "\n"
  printf "----------------------------------------------------------------"
  printf "\n\n"

  printf "# Single multi-asset TxOut; Scott-encoded TxOut serialised using encoder on-chain.\n"
  printf "   n         mem         cpu       %%mem      %%cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (genTxOut n) 42
    let (mem, cpu) = relativeCostOf x maxExecutionUnits encodeTxOutValidator
    -- encodingToBuiltinByteString . encodeTxOut
    case evaluateScriptExecutionUnits (encodeTxOutValidator RealValidator) x of
      Right (ExUnits absMem absCpu) ->
           printf "%4d %12d %12d %s%% %s%%\n" n absMem absCpu (rationalToPercent mem) (rationalToPercent cpu)
      Left e -> printf "ERROR: %s\n" e


  printf "\n"
  printf "# Single multi-asset TxOut; serialised on-chain using serialiseData . toBuiltinData.\n"
  printf "   n         mem         cpu       %%mem      %%cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (genTxOut n) 42
    let (mem, cpu) = relativeCostOf x maxExecutionUnits encodeTxOutValidator2
      -- serialiseData . toBuiltinData
    case evaluateScriptExecutionUnits (encodeTxOutValidator2 RealValidator) x of
      Right (ExUnits absMem absCpu) ->
           printf "%4d %12d %12d %s%% %s%%\n" n absMem absCpu (rationalToPercent mem) (rationalToPercent cpu)
      Left e -> printf "ERROR: %s\n" e


  printf "\n\n"
  printf "Serialisation/toBuiltinData costs only\n"
  printf "======================================\n\n"

  printf "# List of ADA-only TxOut, by list size (library).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let txouts = generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = serialiseMultipleScottTxOutsUsingLibrary txouts
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"

  printf "# List of ADA-only TxOut, by list size (builtin, toData on-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let txouts =  generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = serialiseMultipleScottTxOutsUsingBuiltin txouts
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"

  printf "# List of ADA-only TxOut, by list size (builtin, toData off-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let txouts =  generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = serialiseUsingBuiltin_after_toDataOffChain txouts
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"
  printf "----------------------------------------------------------------"
  printf "\n\n"

  printf "# Single multi-asset TxOut, by number of assets (library).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let txout = generateWith (genTxOut n) 42
        term = serialiseSingleScottTxOutUsingLibrary txout
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"
  printf "# Single multi-asset TxOut, by number of assets (builtin, toData on-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let txout = generateWith (genTxOut n) 42
        term = serialiseSingleScottTxOutUsingBuiltin txout
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)


  printf "\n"
  printf "# Single multi-asset TxOut, by number of assets (builtin, toData off-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let txout = generateWith (genTxOut n) 42
        term = serialiseUsingBuiltin_after_toDataOffChain txout
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)




runTerm :: Term UPLC.NamedDeBruijn -> Plutus.ExBudget
runTerm t = case runExcept @PLC.FreeVariableError $ PLC.runQuoteT $ UPLC.unDeBruijnTerm t of
    Left e   -> throw e
    Right t' -> case Cek.runCekNoEmit PLC.defaultCekParameters Cek.counting t' of
                  (_result, Cek.CountingSt budget) -> budget



relativeCostOf ::
  (Plutus.ToData a) =>
  a ->
  ExUnits ->
  (ValidatorKind -> Scripts.TypedValidator (EncodeValidator a)) ->
  (Rational, Rational)
relativeCostOf a (ExUnits maxMem maxCpu) validator =
  (relativeMemCost, relativeCpuCost)
 where
  ExUnits mem cpu = either (error . show) id $ do
    base <- evaluateScriptExecutionUnits (validator BaselineValidator) a
    real <- evaluateScriptExecutionUnits (validator RealValidator) a
    pure $ distanceExUnits base real

  (relativeMemCost, relativeCpuCost) =
    ( toInteger mem % toInteger maxMem
    , toInteger cpu % toInteger maxCpu
    )

--
-- Helpers
--

rationalToPercent :: Rational -> Text
rationalToPercent r =
  padLeft ' ' 8 $ decodeUtf8 (toLazyByteString $ toFixedDecimals 2 $ 100 * r)
 where
  toFixedDecimals n = formatScientificBuilder Fixed (Just n) . unsafeFromRational

