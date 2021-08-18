{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano where

import Hydra.Prelude hiding (id)

import Cardano.Binary (Annotator, FullByteString (Full), decodeFull', runAnnotator, serialize')
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Mary.Value as Cardano
import Data.Aeson (
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSONKey,
  Value (String),
  decode,
  object,
  toJSONKey,
  withArray,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Data.Aeson.Types (Parser, toJSONKeyText)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Hydra.Ledger (Ledger (..), Tx (..))
import qualified Shelley.Spec.Ledger.API as Cardano

-- WitnessSet pattern is not reexported in API??
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (WitnessSet))

type CardanoEra = MaryEra StandardCrypto

data CardanoTx crypto = CardanoTx
  { id :: Cardano.TxId crypto
  , body :: CardanoTxBody crypto
  , witnesses :: CardanoTxWitnesses crypto
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type CardanoTxBody crypto = Cardano.TxBody (MaryEra crypto)

type CardanoTxWitnesses crypto = Cardano.WitnessSet (MaryEra crypto)

--
--  Transaction Id
--

instance Crypto crypto => ToJSON (Cardano.TxId crypto) where
  toJSON = String . txIdToText @crypto

instance Crypto crypto => FromJSON (Cardano.TxId crypto) where
  parseJSON = withText "base16 encoded TxId" txIdFromText

txIdToText :: forall crypto. Crypto crypto => Cardano.TxId crypto -> Text
txIdToText = encodeBase16 . serialize'

txIdFromText :: (MonadFail m, Crypto crypto) => Text -> m (Cardano.TxId crypto)
txIdFromText t =
  case decodeBase16 (encodeUtf8 t) of
    Left e -> fail $ show e
    Right bytes -> case decodeFull' bytes of
      Left e -> fail $ show e
      Right a -> pure a

--
-- Transaction Body
--

instance Crypto crypto => FromJSON (CardanoTxBody crypto) where
  parseJSON = withObject "CardanoTxBody" $ \o -> do
    inputs <- o .: "inputs" >>= traverse parseJSON
    outputs <- o .: "outputs" >>= traverse parseJSON
    pure $
      Cardano.TxBody
        (Set.fromList inputs)
        (StrictSeq.fromList outputs)
        mempty
        (Cardano.Wdrl mempty)
        mempty
        maxBound
        Cardano.SNothing
        Cardano.SNothing

instance Crypto crypto => ToJSON (CardanoTxBody crypto) where
  toJSON (Cardano.TxBody inputs outputs _certs _wdrls _txfee _ttl _update _mdHash) =
    object
      [ "inputs" .= inputs
      , "outputs" .= outputs
      ]

--
-- Input
--

instance Crypto crypto => ToJSON (Cardano.TxIn crypto) where
  toJSON = toJSON . txInToText

instance Crypto crypto => FromJSON (Cardano.TxIn crypto) where
  parseJSON = withText "TxIn" textToTxIn

textToTxIn :: Crypto crypto => Text -> Parser (Cardano.TxIn crypto)
textToTxIn t = do
  let (txIdText, txIxText) = Text.breakOn "#" t
  Cardano.TxIn
    <$> txIdFromText txIdText
    <*> parseIndex txIxText
 where
  parseIndex txIxText =
    maybe
      (fail $ "cannot parse " <> show txIxText <> " as a natural index")
      pure
      (decode (encodeUtf8 $ Text.drop 1 txIxText))

txInToText :: Crypto crypto => Cardano.TxIn crypto -> Text
txInToText (Cardano.TxIn id ix) = txIdToText id <> "#" <> show ix

instance Crypto crypto => FromJSONKey (Cardano.TxIn crypto) where
  fromJSONKey = FromJSONKeyTextParser textToTxIn

instance Crypto crypto => ToJSONKey (Cardano.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

--
-- Output
--

instance Crypto crypto => ToJSON (Cardano.TxOut (MaryEra crypto)) where
  toJSON (Cardano.TxOut addr value) =
    object
      -- TODO: Use ledger's instance, which is a base16-encoded
      -- serialized address. We might want to use bech32
      -- serialization in the end.
      [ "address" .= addr
      , "value" .= valueToJson value
      ]

instance Crypto crypto => FromJSON (Cardano.TxOut (MaryEra crypto)) where
  parseJSON = withObject "TxOut" $ \o -> do
    address <- o .: "address"
    value <- o .: "value" >>= valueParseJson
    pure $ Cardano.TxOut address value
   where
    valueParseJson = withObject "Value" $ \o ->
      Cardano.Value <$> o .: "lovelace" <*> pure mempty

valueToJson :: Cardano.Value crypto -> Value
valueToJson (Cardano.Value lovelace _assets) =
  object ["lovelace" .= lovelace]

--
-- Utxo
--

instance Semigroup (Cardano.UTxO (MaryEra crypto)) where
  Cardano.UTxO u1 <> Cardano.UTxO u2 = Cardano.UTxO (u1 <> u2)

instance Monoid (Cardano.UTxO (MaryEra crypto)) where
  mempty = Cardano.UTxO mempty

instance Crypto crypto => ToJSON (Cardano.UTxO (MaryEra crypto)) where
  toJSON = toJSON . Cardano.unUTxO

instance Crypto crypto => FromJSON (Cardano.UTxO (MaryEra crypto)) where
  parseJSON v = Cardano.UTxO <$> parseJSON v

--
-- witnesses
--

instance Crypto crypto => ToJSON (CardanoTxWitnesses crypto) where
  toJSON (WitnessSet addrWitnesses _ _) = toJSON $ map (encodeBase16 . serialize') $ toList addrWitnesses

instance
  ( Crypto crypto
  , FromCBOR (Annotator (Cardano.WitVKey 'Cardano.Witness crypto))
  ) =>
  FromJSON (CardanoTxWitnesses crypto)
  where
  parseJSON = withArray "CardanoTxWitnesses" $ \a -> do
    wits <- toList <$> traverse parseAddressWitness a
    pure $ WitnessSet (Set.fromList wits) mempty mempty
   where
    parseAddressWitness = withText "AddrWitness" $ \t ->
      -- TODO(AB): this is ugly
      case decodeBase16 $ encodeUtf8 t of
        Left err -> fail $ show err
        Right bs' -> case decodeFull' bs' of
          Left err -> fail $ show err
          Right v -> pure $ runAnnotator v (Full $ fromStrict bs')

instance Crypto crypto => Tx (CardanoTx crypto) where
  type Utxo (CardanoTx crypto) = Cardano.UTxO (MaryEra crypto)
  type TxId (CardanoTx crypto) = Cardano.TxId crypto

  txId = id

cardanoLedger :: Ledger (CardanoTx crypto)
cardanoLedger =
  Ledger
    { applyTransactions = error "not implemented"
    , initUtxo = mempty
    }
