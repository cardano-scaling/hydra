{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EncodeTxOut where

import           Hydra.Prelude              hiding ((<>))
import           Plutus.Codec.CBOR.Encoding (Encoding, encodeByteString,
                                             encodeInteger, encodeList,
                                             encodeListLen, encodeMap,
                                             encodeMaybe,
                                             encodingToBuiltinByteString)
import           Plutus.V1.Ledger.Api       (Address (..),
                                             Credential (PubKeyCredential, ScriptCredential),
                                             CurrencySymbol (CurrencySymbol),
                                             DatumHash (DatumHash),
                                             PubKeyHash (PubKeyHash),
                                             TokenName (TokenName),
                                             TxOut (TxOut),
                                             ValidatorHash (ValidatorHash),
                                             Value (getValue))
import           PlutusTx.Semigroup         ((<>))


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

