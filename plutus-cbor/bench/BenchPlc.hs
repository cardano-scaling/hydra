{-# LANGUAGE TypeApplications #-}
-- | Criterion benchmark of on-chain serialisation times of some Plutus values
-- to ByteString using the plutus-cbor library and also the serialiseData
-- built-in function.
module Main where

import           Control.Exception
import           Control.Monad.Except
import           Hydra.Prelude                            hiding ((<>))

import           Criterion.Main                           (Benchmarkable, bench,
                                                           bgroup,
                                                           defaultConfig,
                                                           defaultMainWith, nf)
import           Criterion.Types                          (timeLimit)
import           Test.QuickCheck                          (vectorOf)

import           Terms
import           TxOutGen

import qualified PlutusCore                               as PLC
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek as Cek

benchCek :: Term UPLC.NamedDeBruijn -> Benchmarkable
benchCek t = case runExcept @PLC.FreeVariableError $ PLC.runQuoteT $ UPLC.unDeBruijnTerm t of
    Left e   -> throw e
    Right t' -> nf (unsafeEvaluateCek noEmitter PLC.defaultCekParameters) t'

-- Bench both lists of Ada-only TxOuts and single multi-asset TxOuts

main :: IO ()
main = do
  let sizes = [0,10..150]
      mkAdaOnlyBM encoder n =
          let txouts = generateWith (vectorOf n genAdaOnlyTxOut) 42
          in bench (show n) $ benchCek (encoder txouts)
      mkMultiAssetBM encoder n =
          let txout = generateWith (genTxOut n) 42
          in bench (show n) $ benchCek (encoder txout)
  defaultMainWith (defaultConfig { timeLimit = 5 }) $
    [ bgroup
      "ada.only"
      [ bgroup "library"
        (map (mkAdaOnlyBM serialiseMultipleScottTxOutsUsingLibrary) sizes)
      , bgroup "builtin"
        (map (mkAdaOnlyBM serialiseMultipleScottTxOutsUsingBuiltin) sizes)
      ,  bgroup "builtin.todata.offchain"
        (map (mkAdaOnlyBM serialiseUsingBuiltin_after_toDataOffChain) sizes)
      ]
    , bgroup
      "multi-asset"
      [ bgroup "library"
        (map (mkMultiAssetBM serialiseSingleScottTxOutUsingLibrary) sizes)
      , bgroup "builtin"
        (map (mkMultiAssetBM serialiseSingleScottTxOutUsingBuiltin) sizes)
      , bgroup "builtin.todata.offchain"
        (map (mkMultiAssetBM serialiseUsingBuiltin_after_toDataOffChain) sizes)
      ]
    ]


