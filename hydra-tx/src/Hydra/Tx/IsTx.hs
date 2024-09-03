{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.IsTx where

import Hydra.Cardano.Api

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator, serialize')
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Data.Aeson (FromJSONKey, ToJSONKey, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (withObject)
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)
import Hydra.Cardano.Api.UTxO qualified as Api
import Hydra.Contract.Head qualified as Head
import Hydra.Prelude
import PlutusLedgerApi.V2 (fromBuiltin)

class
  ( Eq tx
  , Show tx
  , Typeable tx
  , FromCBOR tx
  , ToCBOR tx
  , FromJSON tx
  , ToJSON tx
  , --
    Eq (TxIdType tx)
  , Ord (TxIdType tx)
  , Show (TxIdType tx)
  , Typeable (TxIdType tx)
  , FromJSON (TxIdType tx)
  , ToJSON (TxIdType tx)
  , FromCBOR (TxIdType tx)
  , ToCBOR (TxIdType tx)
  , FromJSONKey (TxIdType tx)
  , ToJSONKey (TxIdType tx)
  , --
    Eq (TxOutType tx)
  , Show (TxOutType tx)
  , ToJSON (TxOutType tx)
  , FromJSON (TxOutType tx)
  , --
    Eq (UTxOType tx)
  , Show (UTxOType tx)
  , Monoid (UTxOType tx)
  , FromJSON (UTxOType tx)
  , ToJSON (UTxOType tx)
  , FromCBOR (UTxOType tx)
  , ToCBOR (UTxOType tx)
  ) =>
  IsTx tx
  where
  type TxIdType tx
  type TxOutType tx = out | out -> tx
  type UTxOType tx = utxo | utxo -> tx
  type ValueType tx

  -- XXX(SN): this name easily conflicts
  txId :: tx -> TxIdType tx
  balance :: UTxOType tx -> ValueType tx

  -- | Hash a utxo set to be able to sign (off-chain) and verify it (on-chain).
  hashUTxO :: UTxOType tx -> ByteString

  txSpendingUTxO :: UTxOType tx -> tx

  -- | Get the UTxO produced by given transaction.
  utxoFromTx :: tx -> UTxOType tx

  -- | Get only the outputs in given UTxO.
  outputsOfUTxO :: UTxOType tx -> [TxOutType tx]

  -- | Return the left-hand side without the right-hand side.
  withoutUTxO :: UTxOType tx -> UTxOType tx -> UTxOType tx

-- * Cardano Tx

instance ToJSON Tx where
  toJSON tx =
    object
      [ "cborHex" .= Aeson.String (decodeUtf8 $ Base16.encode $ serialiseToCBOR tx)
      , "txId" .= txId tx
      , "type" .= txType tx
      , "description" .= Aeson.String mempty
      ]

instance FromJSON Tx where
  parseJSON =
    withObject "Tx" $ \o -> do
      hexText <- o .: "cborHex"
      ty <- o .: "type"
      bytes <- decodeBase16 hexText
      case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
        Left e -> fail $ show e
        Right tx ->
          (o .:? "txId") >>= \case
            Nothing -> pure tx
            Just txid' -> do
              guard (txType tx == ty)
              guard (txid' == txId tx)
              pure tx

instance ToCBOR Tx where
  toCBOR = CBOR.encodeBytes . serialize' ledgerEraVersion . toLedgerTx

instance FromCBOR Tx where
  fromCBOR = do
    bs <- CBOR.decodeBytes
    decodeFullAnnotator ledgerEraVersion "Tx" decCBOR (fromStrict bs)
      & either
        (fail . toString . toLazyText . build)
        (pure . fromLedgerTx)

instance ToCBOR UTxO where
  toCBOR = toCBOR . toLedgerUTxO
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR UTxO where
  fromCBOR = fromLedgerUTxO <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

txType :: Tx -> Text
txType tx' = case getTxWitnesses tx' of
  [] -> "Unwitnessed Tx BabbageEra"
  _ -> "Witnessed Tx BabbageEra"

instance IsTx Tx where
  type TxIdType Tx = TxId
  type TxOutType Tx = TxOut CtxUTxO
  type UTxOType Tx = UTxO
  type ValueType Tx = Value

  txId = getTxId . getTxBody
  balance = foldMap txOutValue

  -- NOTE: See note from `Head.hashTxOuts`.
  hashUTxO = fromBuiltin . Head.hashTxOuts . mapMaybe toPlutusTxOut . toList

  txSpendingUTxO = Hydra.Cardano.Api.txSpendingUTxO

  utxoFromTx = Api.utxoFromTx

  outputsOfUTxO = toList

  withoutUTxO = UTxO.difference
