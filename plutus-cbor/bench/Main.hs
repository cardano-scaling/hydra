module Main where

import Hydra.Prelude

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import qualified Data.ByteString as BS
import Plutus.V1.Ledger.Api (BuiltinByteString, Data, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as Plutus.Map
import Test.QuickCheck (choose, oneof, vector, vectorOf)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "TxOut serialization in Haskell"
        [ bgroup
            "ada only"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOutAdaOnly
            , bench "cborg" $ whnf cborgSerialize txOutAdaOnly
            ]
        ]
    ]

cborgSerialize :: Data -> BuiltinByteString
cborgSerialize = const ""

-- | Serialize a 'TxOut' to cbor using a simplified encoder.
plutusSerialize :: Data -> BuiltinByteString
plutusSerialize = const ""

-- * Benchmark values

txOutAdaOnly :: Data
txOutAdaOnly =
  toData $ generateWith genAdaOnlyTxOut 42

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
