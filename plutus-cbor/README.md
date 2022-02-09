# Plutus CBOR [![](https://img.shields.io/badge/haddock-1.0.0-blue?style=for-the-badge&logo=haskell)](https://input-output-hk.github.io/hydra-poc/haddock/plutus-cbor/Plutus-Codec-CBOR-Encoding.html)

Encode builtin Plutus types to [CBOR](https://cbor.io/).

## Getting Started

```hs
import Plutus.Codec.CBOR.Encoding
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as Map

serialiseTxOuts :: [TxOut] -> BuiltinByteString
serialiseTxOuts outs =
  encodingToBuiltinByteString $
    encodeBeginList <> foldMap encodeTxOut outs <> encodeBreak
{-# INLINEABLE serialiseTxOuts #-}

encodeTxOut :: TxOut -> Encoding
encodeTxOut TxOut{txOutAddress, txOutValue, txOutDatumHash} =
  case txOutDatumHash of
    Just h ->
      encodeListLen 3
        <> encodeAddress txOutAddress
        <> encodeValue txOutValue
        <> encodeDatum h
    Nothing ->
      encodeListLen 2
        <> encodeAddress txOutAddress
        <> encodeValue txOutValue
{-# INLINEABLE encodeTxOut #-}

encodeAssets :: Map CurrencySymbol (Map TokenName Integer) -> Encoding
encodeAssets m
  | length m <= 23 = encodeMap encodeCurrencySymbol encodeSingleAsset m
  | otherwise = encodeMapIndef encodeCurrencySymbol encodeSingleAsset m
{-# INLINEABLE encodeAssets #-}
```
