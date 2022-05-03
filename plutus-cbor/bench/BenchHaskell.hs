-- | Criterion benchmark of serializing some Plutus values to ByteString using
-- this library, but also `cborg` as a reference.
module Main where

import           Hydra.Prelude              hiding ((<>))
import           TxOutGen

import           Codec.Serialise            (serialise)
import           Criterion.Main             (bench, bgroup, defaultConfig,
                                             defaultMainWith, whnf)
import           Criterion.Types            (timeLimit)
import           Plutus.Codec.CBOR.Encoding (encodingToBuiltinByteString)
import           Plutus.V1.Ledger.Api       (BuiltinByteString, TxOut,
                                             toBuiltin, toData)

import           EncodeTxOut

-- | Use the provided 'Serialise' instance for 'Data' from plutus.
cborgSerialize :: TxOut -> BuiltinByteString
cborgSerialize = toBuiltin . toStrict . serialise . toData

-- | Serialize a 'TxOut' to cbor using our on-chain encoder plutus-cbor, but run in Haskell.
plutusSerialize :: TxOut -> BuiltinByteString
plutusSerialize = encodingToBuiltinByteString . encodeTxOut

main :: IO ()
main = do
  defaultMainWith (defaultConfig { timeLimit = 5 }) $
    [ bgroup
      "TxOut"
      (
       (bgroup
        "ada only"
        [ bench "plutus-cbor" $ whnf plutusSerialize txOutAdaOnly
        , bench "cborg"       $ whnf cborgSerialize  txOutAdaOnly
        ]
        )
       : map mkMultiAssetBM [0,10..150]
      )
    ]
 where
  txOutAdaOnly   = generateWith genAdaOnlyTxOut 42
  mkMultiAssetBM n =
      let assets = generateWith (genTxOut n) 42
      in bgroup
             (show n ++ " assets")
             [ bench "plutus-cbor" $ whnf plutusSerialize assets
             , bench "cborg"       $ whnf cborgSerialize assets
             ]
