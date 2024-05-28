-- | Criterion benchmark of serializing some Plutus values to ByteString using
-- this library, but also `cborg` as a reference.
module Main where

import Hydra.Prelude hiding ((<>))

import Codec.Serialise (serialise)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.ByteString qualified as BS
import Plutus.Codec.CBOR.Encoding (Encoding, encodeByteString, encodeInteger, encodeListLen, encodeMap, encodeMaybe, encodingToBuiltinByteString)
import PlutusLedgerApi.V1 (
  Address (..),
  BuiltinByteString,
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  DatumHash (DatumHash),
  PubKeyHash (PubKeyHash),
  ScriptHash (ScriptHash),
  TokenName (TokenName),
  TxOut (TxOut),
  Value (getValue),
  toBuiltin,
  toData,
 )
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx.AssocMap qualified as Plutus.Map
import PlutusTx.Semigroup ((<>))
import Test.QuickCheck (choose, oneof, vector, vectorOf)

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "TxOut"
        [ bgroup
            "ada only"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOutAdaOnly
            , bench "cborg" $ whnf cborgSerialize txOutAdaOnly
            ]
        , bgroup
            "20 assets"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOut20Assets
            , bench "cborg" $ whnf cborgSerialize txOut20Assets
            ]
        , bgroup -- roughly current maxValSize=5000 on mainchain
            "80 assets"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOut80Assets
            , bench "cborg" $ whnf cborgSerialize txOut80Assets
            ]
        , bgroup
            "100 assets"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOut100Assets
            , bench "cborg" $ whnf cborgSerialize txOut100Assets
            ]
        ]
    ]
 where
  txOutAdaOnly = generateWith genAdaOnlyTxOut 42
  txOut20Assets = generateWith (genTxOut 20) 42
  txOut80Assets = generateWith (genTxOut 80) 42
  txOut100Assets = generateWith (genTxOut 100) 42

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
    PubKeyCredential (PubKeyHash h) -> h
    ScriptCredential (ScriptHash h) -> h
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

-- * Benchmark values

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
      Plutus.Map.safeFromList
        [(policyId, Plutus.Map.safeFromList [(assetName, n)])]

genAdaOnlyValue :: Gen Plutus.Value
genAdaOnlyValue = do
  n <- genAssetQuantity
  pure $
    Plutus.Value $
      Plutus.Map.safeFromList
        [(Plutus.adaSymbol, Plutus.Map.safeFromList [(Plutus.adaToken, n)])]

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
