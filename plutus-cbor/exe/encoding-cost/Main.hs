{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

import           Hydra.Prelude                            hiding (label)

import           Control.Exception                        (throw)
import           Control.Monad.Except
import           Data.ByteString                          as BS
import           Flat                                     (flat)
import           GHC.IO.Encoding
import           Test.QuickCheck                          (vectorOf)
import           Text.Printf                              (printf)


import qualified Ledger.Typed.Scripts                     as Scripts
import           Plutus.Codec.CBOR.Encoding.Validator     (EncodeValidator,
                                                           ValidatorKind (..),
                                                           encodeTxOutValidator,
                                                           encodeTxOutValidatorUsingBuiltin,
                                                           encodeTxOutsValidator,
                                                           encodeTxOutsValidatorUsingBuiltin)
import qualified Plutus.V1.Ledger.Api                     as Plutus
import qualified PlutusCore                               as PLC
import           Test.Plutus.Validator                    (ExUnits (..),
                                                           distanceExUnits,
                                                           evaluateScriptExecutionUnits)
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           Terms
import           TxOutGen

maxExecutionUnits :: ExUnits
maxExecutionUnits =  ExUnits
    { exUnitsMem   = 10_000_000
    , exUnitsSteps = 10_000_000_000
    }

runTerm :: Term UPLC.NamedDeBruijn -> Plutus.ExBudget
runTerm t = case runExcept @PLC.FreeVariableError $ PLC.runQuoteT $ UPLC.unDeBruijnTerm t of
    Left e   -> throw e
    Right t' -> case Cek.runCekNoEmit PLC.defaultCekParameters Cek.counting t' of
                  (_result, Cek.CountingSt budget) -> budget

relativeCostAsPercentage ::
  (Plutus.ToData a) =>
  a ->
  ExUnits ->
  (ValidatorKind -> Scripts.TypedValidator (EncodeValidator a)) ->
  (Float, Float)
relativeCostAsPercentage a (ExUnits maxMem maxCpu) validator =
  (relativeMemCost, relativeCpuCost)
 where
  ExUnits mem cpu = either (error . show) id $ do
    base <- evaluateScriptExecutionUnits (validator BaselineValidator) a
    real <- evaluateScriptExecutionUnits (validator RealValidator) a
    pure $ distanceExUnits base real

  (relativeMemCost, relativeCpuCost) =
    ( (fromIntegral mem) / (fromIntegral maxMem) * 100
    , (fromIntegral cpu) / (fromIntegral maxCpu) * 100
    )


toMicroseconds :: Integral a => a -> Float
toMicroseconds x = (fromIntegral x)/1e6

-- Statistics for calculated budgets of full validators

printRelativeHeader :: IO ()
printRelativeHeader =
    printf "   n         mem         cpu      %%mem      %%cpu\n"

-- Some code abstracted from the original.

-- This is a bit odd: we report the cost as a percentage of the maximum
-- execution units as specified above, but not the total cost. Instead we report
-- (as a percentage of the maximum) the difference from a baseline validator
-- which contains all the same data but doesn't do any serialisation.
printRelativeBudget ::
    (Plutus.ToData a)
    => Int
    -> a
    -> (ValidatorKind -> Scripts.TypedValidator (EncodeValidator a))
    -> IO ()
printRelativeBudget n x encoder = do
    let (mem, cpu) = relativeCostAsPercentage x maxExecutionUnits encoder
    -- encodingToBuiltinByteString (encodeList encodeTxOut xs)
    case evaluateScriptExecutionUnits (encoder RealValidator) x of
      Right (ExUnits absMem absCpu) ->
           printf "%4d %12d %8.0f µs %8.2f%% %8.2f%%\n" n absMem (toMicroseconds absCpu) mem cpu
      Left e -> printf "ERROR: %s\n" e


-- Statistics for calculated budgets of serialisation-only scripts.
-- This just shows the absolute numbers.
printBudgetHeader :: IO ()
printBudgetHeader =
    printf "   n         mem         cpu       cpu/1e6     size\n"

-- Return the number of bytes in the serialised form of a term.
-- In reality the term would be wrapped in a Program, but that would only add a few bytes.
-- Also, get rid of the names; the evaluator expects these, but we don't serialise them.
sizeOfFlatSerialisedTerm :: Term UPLC.NamedDeBruijn -> Integer
sizeOfFlatSerialisedTerm term =
    let term' = UPLC.termMapNames (\(UPLC.NamedDeBruijn _ ix) -> UPLC.DeBruijn ix) term
    in fromIntegral . BS.length $ flat term'

printBudget :: Int -> Term UPLC.NamedDeBruijn -> IO ()
printBudget n term =
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s %8.0f µs %8d\n" n
                     (show absMem :: String)
                     (show absCpu :: String)
                     (toMicroseconds absCpu)
                     (sizeOfFlatSerialisedTerm term)


main :: IO ()
main = do
  setLocaleEncoding utf8  -- problems with mu

  -- Print calculated budgets for full validators
  printf "\n"
  printf "Full validations\n"
  printf "================\n\n"

  printf "# List of ADA-only TxOut; Scott-encoded TxOut serialised using plutus-cbor on-chain.\n"
  printRelativeHeader
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (vectorOf n genAdaOnlyTxOut) 42
    printRelativeBudget n x encodeTxOutsValidator

  printf "\n"

  printf "# List of ADA-only TxOut; serialised on-chain using serialiseData . toBuiltinData.\n"
  printRelativeHeader
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (vectorOf n genAdaOnlyTxOut) 42
    printRelativeBudget n x encodeTxOutsValidatorUsingBuiltin

  printf "\n"
  printf "----------------------------------------------------------------"
  printf "\n\n"

  printf "# Single multi-asset TxOut; Scott-encoded TxOut serialised using plutus-cbor on-chain.\n"
  printRelativeHeader
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (genTxOut n) 42
    printRelativeBudget n x encodeTxOutValidator

  printf "\n"
  printf "# Single multi-asset TxOut; serialised on-chain using serialiseData . toBuiltinData.\n"
  printRelativeHeader
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (genTxOut n) 42
    printRelativeBudget n x encodeTxOutValidatorUsingBuiltin


  -- Print calculated budgets for scripts that just do serialisation using different methods.
  -- Maybe this should be in a separate executable, but I'm too lazy.
  printf "\n\n"
  printf "Serialisation/toBuiltinData costs only\n"
  printf "======================================\n\n"

  printf "# List of ADA-only TxOut, by list size (plutus-cbor).\n"
  printBudgetHeader
  forM_ [0,10..150] $ \n -> do
    let txouts = generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = serialiseMultipleScottTxOutsUsingLibrary txouts
    printBudget n term

  printf "\n"

  printf "# List of ADA-only TxOut, by list size (builtin serialiseData, toData on-chain).\n"
  printBudgetHeader
  forM_ [0,10..150] $ \n -> do
    let txouts =  generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = serialiseMultipleScottTxOutsUsingBuiltin txouts
    printBudget n term

  printf "\n"

  printf "# List of ADA-only TxOut, by list size (builtin serialiseData, toData off-chain).\n"
  printBudgetHeader
  forM_ [0,10..150] $ \n -> do
    let txouts =  generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = serialiseUsingBuiltin_after_toDataOffChain txouts
    printBudget n term

  printf "\n"
  printf "----------------------------------------------------------------"
  printf "\n\n"

  printf "# Single multi-asset TxOut, by number of assets (plutus-cbor).\n"
  printBudgetHeader
  forM_ [0,10..150] $ \n -> do
    let txout = generateWith (genTxOut n) 42
        term = serialiseSingleScottTxOutUsingLibrary txout
    printBudget n term

  printf "\n"
  printf "# Single multi-asset TxOut, by number of assets (builtin serialiseData, toData on-chain).\n"
  printBudgetHeader
  forM_ [0,10..150] $ \n -> do
    let txout = generateWith (genTxOut n) 42
        term = serialiseSingleScottTxOutUsingBuiltin txout
    printBudget n term


  printf "\n"
  printf "# Single multi-asset TxOut, by number of assets (builtin serialiseData, toData off-chain).\n"
  printBudgetHeader
  forM_ [0,10..150] $ \n -> do
    let txout = generateWith (genTxOut n) 42
        term = serialiseUsingBuiltin_after_toDataOffChain txout
    printBudget n term

