{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
-- | Criterion benchmark of on-chain serialisation times of some Plutus values
-- to ByteString using the plutus-cbor library and also the serialiseData
-- built-in function.
module Main where

import           Control.Exception
import           Control.Monad.Except
import           Hydra.Prelude                            hiding ((<>))
import           TxGen

import           Criterion.Main                           (Benchmarkable, bench,
                                                           bgroup,
                                                           defaultConfig,
                                                           defaultMainWith, nf)
import           Criterion.Types                          (timeLimit)
import           Test.QuickCheck                          (vectorOf)

import           Plutus.Codec.CBOR.Encoding               (Encoding,
                                                           encodeByteString,
                                                           encodeInteger,
                                                           encodeList,
                                                           encodeListLen,
                                                           encodeMap,
                                                           encodeMaybe,
                                                           encodingToBuiltinByteString)
import           Plutus.V1.Ledger.Api                     (Address (..),
                                                           Credential (PubKeyCredential, ScriptCredential),
                                                           CurrencySymbol (CurrencySymbol),
                                                           DatumHash (DatumHash),
                                                           PubKeyHash (PubKeyHash),
                                                           TokenName (TokenName),
                                                           TxOut (TxOut),
                                                           ValidatorHash (ValidatorHash),
                                                           Value (getValue))
import qualified PlutusTx                                 as Tx
import qualified PlutusTx.Builtins                        as Tx
import           PlutusTx.Semigroup                       ((<>))

import qualified PlutusCore                               as PLC
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek as Cek

type Program name = UPLC.Program name UPLC.DefaultUni UPLC.DefaultFun ()
type Term name = UPLC.Term name UPLC.DefaultUni UPLC.DefaultFun ()

bodyOf :: Program name -> Term name
bodyOf (UPLC.Program _ _ term) = term

benchCek :: Term UPLC.NamedDeBruijn -> Benchmarkable
benchCek t = case runExcept @PLC.FreeVariableError $ PLC.runQuoteT $ UPLC.unDeBruijnTerm t of
    Left e   -> throw e
    Right t' -> nf (unsafeEvaluateCek noEmitter PLC.defaultCekParameters) t'

serialiseMultipleTxOutsUsingLibrary_onchain :: [TxOut] -> Term UPLC.NamedDeBruijn
serialiseMultipleTxOutsUsingLibrary_onchain txOuts =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(xs::[TxOut]) ->
                               let bytes = encodingToBuiltinByteString (encodeList encodeTxOut xs)
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOuts)

serialiseMultipleTxOutsUsingBuiltin_toData_onchain :: [TxOut] -> Term UPLC.NamedDeBruijn
serialiseMultipleTxOutsUsingBuiltin_toData_onchain txOuts =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(xs::[TxOut]) ->
                               let bytes = Tx.serialiseData (Tx.toBuiltinData xs)
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOuts)


serialiseMultipleTxOutsUsingBuiltin_toData_offchain :: [TxOut] -> Term UPLC.NamedDeBruijn
serialiseMultipleTxOutsUsingBuiltin_toData_offchain txOuts =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \d ->
                               let bytes = Tx.serialiseData d
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode (Tx.toBuiltinData txOuts))

serialiseSingleTxOutUsingLibrary_onchain :: TxOut -> Term UPLC.NamedDeBruijn
serialiseSingleTxOutUsingLibrary_onchain txOut =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(x::TxOut) ->
                               let bytes = encodingToBuiltinByteString (encodeTxOut x)
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOut)

-- We should also try converting to Data off chain then using the library to
-- serialise on-chain, but there's no encoding function for Data. Maybe we
-- should write one?

serialiseSingleTxOutUsingBuiltin_toData_onchain :: TxOut -> Term UPLC.NamedDeBruijn
serialiseSingleTxOutUsingBuiltin_toData_onchain txOut =
    bodyOf . Tx.getPlc $
               $$(Tx.compile
                        [||
                         \(x::TxOut) ->
                             let bytes = Tx.serialiseData (Tx.toBuiltinData x)
                             in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                        ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOut)


serialiseSingleTxOutUsingBuiltin_toData_offchain :: TxOut -> Term UPLC.NamedDeBruijn
serialiseSingleTxOutUsingBuiltin_toData_offchain txOut =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \d ->
                               let bytes = Tx.serialiseData d
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode (Tx.toBuiltinData txOut))



encodeTxOut :: TxOut -> Encoding
encodeTxOut (TxOut addr value datum) =
  encodeListLen 3
    <> encodeAddress addr
    <> encodeValue value
    <> encodeDatum datum
{-# INLINEABLE encodeTxOut #-}

-- NOTE 1: This is missing the header byte with network discrimination. For the
-- sake of getting an order of magnitude and moving forward, it is fine.
--
-- NOTE 2: This is ignoring any stake reference and assuming that all addresses
-- are plain script or payment addresses with no delegation whatsoever. Again,
-- see NOTE #1.
encodeAddress :: Address -> Encoding
encodeAddress Address{addressCredential} =
  encodeByteString (credentialToBytes addressCredential)
 where
  credentialToBytes = \case
    PubKeyCredential (PubKeyHash h)    -> h
    ScriptCredential (ValidatorHash h) -> h
{-# INLINEABLE encodeAddress #-}

encodeValue :: Value -> Encoding
encodeValue =
  encodeMap encodeCurrencySymbol (encodeMap encodeTokenName encodeInteger) . getValue
 where
  encodeCurrencySymbol (CurrencySymbol symbol) = encodeByteString symbol
  encodeTokenName (TokenName token) = encodeByteString token
{-# INLINEABLE encodeValue #-}

encodeDatum :: Maybe DatumHash -> Encoding
encodeDatum =
  encodeMaybe (\(DatumHash h) -> encodeByteString h)
{-# INLINEABLE encodeDatum #-}



-- Bench both lists of Ada-only TxOuts and single multi-asset TxOuts

main :: IO ()
main = do
  let sizes = [10,20..150]
  defaultMainWith (defaultConfig { timeLimit = 5 }) $
    [ bgroup
      "onchain.encoder"
      [ bgroup "ada.only"
        (map (mkAdaOnlyBM serialiseMultipleTxOutsUsingLibrary_onchain) sizes)
      , bgroup "multi.asset"
        (map (mkMultiAssetBM serialiseSingleTxOutUsingLibrary_onchain) sizes)
      , bgroup
      "serialiseData.toData.onChain"
      [ bgroup "ada.only"
        (map (mkAdaOnlyBM serialiseMultipleTxOutsUsingBuiltin_toData_onchain) sizes)
      , bgroup "multi.asset"
        (map (mkMultiAssetBM serialiseSingleTxOutUsingBuiltin_toData_onchain) sizes)
      ]
    , bgroup
      "serialiseData.toData.offChain"
      [ bgroup "ada.only"
        (map (mkAdaOnlyBM serialiseMultipleTxOutsUsingBuiltin_toData_offchain) sizes)
      , bgroup "multi.asset"
        (map (mkMultiAssetBM serialiseSingleTxOutUsingBuiltin_toData_offchain) sizes)
      ]
      ]
    ]
 where
   mkAdaOnlyBM encoder n =
       let txouts = generateWith (vectorOf n genAdaOnlyTxOut) 42
       in bench (show n) $ benchCek (encoder txouts)

   mkMultiAssetBM encoder n =
       let txout = generateWith (genTxOut n) 42
       in bench (show n) $ benchCek (encoder txout)


