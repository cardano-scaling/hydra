{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

import           Hydra.Prelude                            hiding (label)

import           Control.Exception                        (throw)
import           Control.Monad.Except
import           Data.Binary.Builder                      (toLazyByteString)
import qualified Data.ByteString                          as BS
import           Data.ByteString.Builder.Scientific       (FPFormat (Fixed),
                                                           formatScientificBuilder)
import           Data.Ratio                               ((%))
import           Data.Scientific                          (unsafeFromRational)
import qualified Ledger.Typed.Scripts                     as Scripts
import           Plutus.Codec.CBOR.Encoding               (encodeList,
                                                           encodingToBuiltinByteString)
import           Plutus.Codec.CBOR.Encoding.Validator     (EncodeValidator,
                                                           ValidatorKind (..),
                                                           encodeTxOut,
                                                           encodeTxOutValidator,
                                                           encodeTxOutValidator2,
                                                           encodeTxOutsValidator,
                                                           encodeTxOutsValidator2)
import qualified Plutus.V1.Ledger.Api                     as Plutus
import qualified PlutusCore                               as PLC
import qualified PlutusTx                                 as Tx
import qualified PlutusTx.AssocMap                        as Plutus.Map
import qualified PlutusTx.Builtins                        as Tx
import           Test.Plutus.Validator                    (ExUnits (..),
                                                           distanceExUnits,
                                                           evaluateScriptExecutionUnits)
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           Test.QuickCheck                          (choose, oneof,
                                                           vector, vectorOf)

import           Text.Printf                              (printf)


maxExecutionUnits :: ExUnits
maxExecutionUnits =  ExUnits
    { exUnitsMem   = 10_000_000
    , exUnitsSteps = 10_000_000_000
    }


{- Was previously
  ExUnits
    { exUnitsMem = 10_000_000
    , exUnitsSteps = 10_000_000_000
    }

-}

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
    let x = generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = mkScriptUsingEncode' x
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"

  printf "# List of ADA-only TxOut, by list size (builtin, toData on-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x =  generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = mkScriptUsingBuiltin_toData_onchain' x
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"

  printf "# List of ADA-only TxOut, by list size (builtin, toData off-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x =  generateWith (vectorOf n genAdaOnlyTxOut) 42
        term = mkScriptUsingBuiltin_toData_offchain x
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"
  printf "----------------------------------------------------------------"
  printf "\n\n"

  printf "# Single multi-asset TxOut, by number of assets (library).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (genTxOut n) 42
        term = mkScriptUsingEncode x
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)

  printf "\n"
  printf "# Single multi-asset TxOut, by number of assets (builtin, toData on-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (genTxOut n) 42
        term = mkScriptUsingBuiltin_toData_onchain x
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)


  printf "\n"
  printf "# Single multi-asset TxOut, by number of assets (builtin, toData off-chain).\n"
  printf "   n         mem         cpu\n"
  forM_ [0,10..150] $ \n -> do
    let x = generateWith (genTxOut n) 42
        term = mkScriptUsingBuiltin_toData_offchain x
    case runTerm term of
      Plutus.ExBudget (Plutus.ExCPU absCpu) (Plutus.ExMemory absMem) ->
          printf "%4d %12s %12s\n" n (show absMem :: String) (show absCpu :: String)


type Term name = UPLC.Term name UPLC.DefaultUni UPLC.DefaultFun ()
type Program name = UPLC.Program name UPLC.DefaultUni UPLC.DefaultFun ()

bodyOf :: Program name -> Term name
bodyOf (UPLC.Program _ _ term) = term


runTerm :: Term UPLC.NamedDeBruijn -> Plutus.ExBudget
runTerm t = case runExcept @PLC.FreeVariableError $ PLC.runQuoteT $ UPLC.unDeBruijnTerm t of
    Left e   -> throw e
    Right t' -> case Cek.runCekNoEmit PLC.defaultCekParameters Cek.counting t' of
                  (_result, Cek.CountingSt budget) -> budget


-- Take a TxOut, lift it to a Scott object, then serialise "on chain" using the library
mkScriptUsingEncode ::  Plutus.TxOut -> Term UPLC.NamedDeBruijn
mkScriptUsingEncode txout =
    bodyOf . Tx.getPlc $
               $$(Tx.compile [||
                              \x -> let bytes = encodingToBuiltinByteString (encodeTxOut x)
                                    in Tx.lengthOfByteString bytes `Tx.greaterThanInteger`  0
                                           ||])
                     `Tx.applyCode` Tx.liftCode txout

-- Take a list of TxOuts, lift it to a Scott object, then serialise "on chain" using the library.
mkScriptUsingEncode' ::  [Plutus.TxOut] -> Term UPLC.NamedDeBruijn
mkScriptUsingEncode' txout =
    bodyOf . Tx.getPlc $
               $$(Tx.compile [||
                              \x -> let bytes = encodingToBuiltinByteString (encodeList encodeTxOut x)
                                    in Tx.lengthOfByteString bytes `Tx.greaterThanInteger`  0
                                           ||])
                     `Tx.applyCode` Tx.liftCode txout

-- Take anything that can be converted to Data, do so off chain, lift the Data object, then serialise that using the builtin
mkScriptUsingBuiltin_toData_offchain :: Tx.ToData a => a -> Term UPLC.NamedDeBruijn
mkScriptUsingBuiltin_toData_offchain x =
    bodyOf . Tx.getPlc $
            $$(Tx.compile [||
                           \y -> let bytes = Tx.serialiseData y
                                 in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0 ||])
                  `Tx.applyCode` Tx.liftCode (Tx.toBuiltinData x)

mkScriptUsingBuiltin_toData_onchain :: Plutus.TxOut -> Term UPLC.NamedDeBruijn
mkScriptUsingBuiltin_toData_onchain txOut =
    bodyOf . Tx.getPlc $
            $$(Tx.compile [||
                           \(x::Plutus.TxOut) ->
                               let bytes = Tx.serialiseData (Tx.toBuiltinData x)  -- !!! REPLACE THIS WITH  Tx.serialiseData $ Tx.toBuiltinData x
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||])
                  `Tx.applyCode` Tx.liftCode txOut

mkScriptUsingBuiltin_toData_onchain' :: [Plutus.TxOut] -> Term UPLC.NamedDeBruijn
mkScriptUsingBuiltin_toData_onchain' txOuts =
    bodyOf . Tx.getPlc $
               $$(Tx.compile
                        [||
                         \(xs::[Plutus.TxOut]) ->
                             let bytes = Tx.serialiseData (Tx.toBuiltinData xs)
                             in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                        ||])
                     `Tx.applyCode` Tx.liftCode txOuts


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

--
-- Generators
--

genTxOut :: Int -> Gen Plutus.TxOut
genTxOut n = do
  Plutus.TxOut
    <$> genAddress
    <*> fmap mconcat (vectorOf n genValue)
    <*> oneof [pure Nothing, Just <$> genDatumHash]

genAdaOnlyTxOut :: Gen Plutus.TxOut
genAdaOnlyTxOut =
  Plutus.TxOut
    <$> genAddress
    <*> genAdaOnlyValue
    <*> oneof [pure Nothing, Just <$> genDatumHash]

genAddress :: Gen Plutus.Address
genAddress =
  Plutus.Address
    <$> fmap (Plutus.PubKeyCredential . Plutus.PubKeyHash . Plutus.toBuiltin) (genByteStringOf 28)
    <*> pure Nothing

genValue :: Gen Plutus.Value
genValue = do
  n <- genAssetQuantity
  policyId <- genCurrencySymbol
  assetName <- genTokenName
  pure $
    Plutus.Value $
      Plutus.Map.fromList
        [(policyId, Plutus.Map.fromList [(assetName, n)])]

genAdaOnlyValue :: Gen Plutus.Value
genAdaOnlyValue = do
  n <- genAssetQuantity
  pure $
    Plutus.Value $
      Plutus.Map.fromList
        [(Plutus.adaSymbol, Plutus.Map.fromList [(Plutus.adaToken, n)])]

genAssetQuantity :: Gen Integer
genAssetQuantity = choose (1, 4_294_967_296) -- NOTE: 2**32

genCurrencySymbol :: Gen Plutus.CurrencySymbol
genCurrencySymbol =
  Plutus.CurrencySymbol
    <$> fmap Plutus.toBuiltin (genByteStringOf 32)

genTokenName :: Gen Plutus.TokenName
genTokenName =
  Plutus.TokenName
    <$> fmap Plutus.toBuiltin (genByteStringOf =<< choose (0, 32))

genDatumHash :: Gen Plutus.DatumHash
genDatumHash =
  Plutus.DatumHash
    <$> fmap Plutus.toBuiltin (genByteStringOf 32)

genByteStringOf :: Int -> Gen ByteString
genByteStringOf n =
  BS.pack <$> vector n


f :: Plutus.TxOut -> Plutus.BuiltinData
f o = Plutus.toBuiltinData o
