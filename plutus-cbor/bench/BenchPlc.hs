{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
-- | Criterion benchmark of serializing some Plutus values to ByteString using
-- this library, but also `cborg` as a reference.
module Main where

import           Control.Exception
import           Control.Monad.Except
import           Hydra.Prelude                            hiding ((<>))
import           TxGen

import           Codec.Serialise                          (serialise)
import           Criterion.Main                           (Benchmarkable, bench,
                                                           bgroup,
                                                           defaultConfig,
                                                           defaultMainWith, nf,
                                                           whnf)
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
                                                           BuiltinByteString,
                                                           Credential (PubKeyCredential, ScriptCredential),
                                                           CurrencySymbol (CurrencySymbol),
                                                           DatumHash (DatumHash),
                                                           PubKeyHash (PubKeyHash),
                                                           TokenName (TokenName),
                                                           TxOut (TxOut),
                                                           ValidatorHash (ValidatorHash),
                                                           Value (getValue),
                                                           toBuiltin, toData)
import qualified PlutusTx                                 as Tx
import qualified PlutusTx.Builtins                        as Tx
import           PlutusTx.Semigroup                       ((<>))

import qualified PlutusCore                               as PLC
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek as Cek

-- Bench both lists of Ada-only TxOuts and single multi-asset TxOuts

main :: IO ()
main = do
  let sizes = [10,20..150]
  defaultMainWith (defaultConfig { timeLimit = 5 }) $
    [ bgroup
      "serialiseData"
      [ bgroup "ada.only"
        (map (mkAdaOnlyBM serialiseMultipleTxOutsUsingBuiltin) sizes)
      , bgroup "multi.asset"
        (map (mkMultiAssetBM serialiseSingleTxOutUsingBuiltin) sizes)
      ]
    , bgroup
      "onchain.encoder"
      [ bgroup "ada.only"
        (map (mkAdaOnlyBM serialiseMultipleTxOutsUsingLibrary) sizes)
      , bgroup "multi.asset"
        (map (mkMultiAssetBM serialiseSingleTxOutUsingLibrary) sizes)
      ]
    ]
 where
   mkAdaOnlyBM encoder n =
       let txouts = generateWith (vectorOf n genAdaOnlyTxOut) 42
       in bench (show n) $ whnf encoder txouts

   mkMultiAssetBM encoder n =
       let txout = generateWith (genTxOut n) 42
       in bench (show n) $ whnf encoder txout

--

type Program name = UPLC.Program name UPLC.DefaultUni UPLC.DefaultFun ()
type Term name = UPLC.Term name UPLC.DefaultUni UPLC.DefaultFun ()

bodyOf :: Program name -> Term name
bodyOf (UPLC.Program _ _ term) = term

serialiseMultipleTxOutsUsingBuiltin :: [TxOut] -> Term UPLC.NamedDeBruijn
serialiseMultipleTxOutsUsingBuiltin txOuts =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(xs::[TxOut]) ->
                               let bytes = Tx.serialiseData (Tx.toBuiltinData xs)
                               in Tx.lengthOfByteString bytes
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOuts)


serialiseSingleTxOutUsingBuiltin :: TxOut -> Term UPLC.NamedDeBruijn
serialiseSingleTxOutUsingBuiltin txOut =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(x::TxOut) ->
                               let bytes = Tx.serialiseData (Tx.toBuiltinData x)
                               in Tx.lengthOfByteString bytes
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOut)

serialiseMultipleTxOutsUsingLibrary :: [TxOut] -> Term UPLC.NamedDeBruijn
serialiseMultipleTxOutsUsingLibrary txOuts =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(xs::[TxOut]) ->
                               let bytes = encodingToBuiltinByteString (encodeList encodeTxOut xs)
                               in Tx.lengthOfByteString bytes
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOuts)


serialiseSingleTxOutUsingLibrary :: TxOut -> Term UPLC.NamedDeBruijn
serialiseSingleTxOutUsingLibrary txOut =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(x::TxOut) ->
                               let bytes = encodingToBuiltinByteString (encodeTxOut x)
                               in Tx.lengthOfByteString bytes
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOut)


benchCek :: Term UPLC.NamedDeBruijn -> Benchmarkable
benchCek t = case runExcept @PLC.FreeVariableError $ PLC.runQuoteT $ UPLC.unDeBruijnTerm t of
    Left e   -> throw e
    Right t' -> nf (unsafeEvaluateCek noEmitter PLC.defaultCekParameters) t'
    -- ** ... or whnf?

-- unDeBruijnTerm converts `Term NamedDeBruijn` to `Term Name`

-- We want to benchmark some terms involving serialisation (using both the library and
-- the builtin) in here.  What should we use?


{- | Convert a de-Bruijn-named UPLC term to a Benchmark -}
benchTermCek :: NFData name => Term name -> Benchmarkable
benchTermCek term = nf id term
--    nf (runTermCek) $! term -- Or whnf?

-- | Use the provided 'Serialise' instance for 'Data' from plutus.
cborgSerialize :: TxOut -> BuiltinByteString
cborgSerialize = toBuiltin . toStrict . serialise . toData

-- | Serialize a 'TxOut' to cbor using our on-chain encoder plutus-cbor, but run in Haskell.
plutusSerialize :: TxOut -> BuiltinByteString
plutusSerialize = encodingToBuiltinByteString . encodeTxOut

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

