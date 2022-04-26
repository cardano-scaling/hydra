-- | Criterion benchmark of serializing some Plutus values to ByteString using
-- this library, but also `cborg` as a reference.
module Main where

import           Hydra.Prelude              hiding ((<>))
import           TxGen

import           Codec.Serialise            (serialise)
import           Criterion.Main             (bench, bgroup, defaultConfig,
                                             defaultMainWith, whnf)
import           Criterion.Types            (timeLimit)
import           Plutus.Codec.CBOR.Encoding (Encoding, encodeByteString,
                                             encodeInteger, encodeListLen,
                                             encodeMap, encodeMaybe,
                                             encodingToBuiltinByteString)
import           Plutus.V1.Ledger.Api       (Address (..), BuiltinByteString,
                                             Credential (PubKeyCredential, ScriptCredential),
                                             CurrencySymbol (CurrencySymbol),
                                             DatumHash (DatumHash),
                                             PubKeyHash (PubKeyHash),
                                             TokenName (TokenName),
                                             TxOut (TxOut),
                                             ValidatorHash (ValidatorHash),
                                             Value (getValue), toBuiltin,
                                             toData)
import           PlutusTx.Semigroup         ((<>))

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
       : map mkMultiAssetBM [10,20..150]
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

