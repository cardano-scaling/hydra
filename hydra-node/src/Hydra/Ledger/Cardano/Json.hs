{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans ToJSON/FromJSON instances on ledger types used by
-- Hydra.Ledger.Cardano to have JSON representations for various types.
--
-- XXX: The ledger team notified that we should be using lenses going forward.
module Hydra.Ledger.Cardano.Json where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Api (Babbage)
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Api.Era (eraProtVerLow)
import Cardano.Ledger.Babbage.PParams (BabbagePParams (..))
import Cardano.Ledger.Babbage.PParams qualified as Ledger
import Cardano.Ledger.Babbage.Tx qualified as Ledger
import Cardano.Ledger.Binary (
  DecCBOR,
  EncCBOR,
  decCBOR,
  decodeFullAnnotator,
  decodeFullDecoder,
 )
import Cardano.Ledger.Binary.Decoding (Annotator)
import Cardano.Ledger.Shelley.API qualified as Ledger
import Data.Aeson (
  Value (String),
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (
  Parser,
 )
import Data.ByteString.Base16 qualified as Base16

-- XXX: Maybe use babbagePParamsHKDPairs?
instance FromJSON (Ledger.BabbagePParams Identity era) where
  parseJSON =
    Aeson.withObject "PParams" $ \obj ->
      BabbagePParams
        <$> obj
          .: "minFeeA"
        <*> obj
          .: "minFeeB"
        <*> obj
          .: "maxBlockBodySize"
        <*> obj
          .: "maxTxSize"
        <*> obj
          .: "maxBlockHeaderSize"
        <*> obj
          .: "keyDeposit"
        <*> obj
          .: "poolDeposit"
        <*> obj
          .: "eMax"
        <*> obj
          .: "nOpt"
        <*> obj
          .: "a0"
        <*> obj
          .: "rho"
        <*> obj
          .: "tau"
        -- NOTE: 'protocolVersion' here is set to optional until the upstream
        -- bug fix is released. Relevant PR https://github.com/IntersectMBO/cardano-ledger/pull/3953
        <*> (obj .:? "protocolVersion" .!= Ledger.ProtVer (eraProtVerLow @Babbage) 0)
        <*> obj
          .: "minPoolCost"
          .!= mempty
        <*> obj
          .: "coinsPerUTxOByte"
        <*> obj
          .: "costmdls"
        <*> obj
          .: "prices"
        <*> obj
          .: "maxTxExUnits"
        <*> obj
          .: "maxBlockExUnits"
        <*> obj
          .: "maxValSize"
        <*> obj
          .: "collateralPercentage"
        <*> obj
          .: "maxCollateralInputs"

--
-- AlonzoTx
--

instance
  ( EncCBOR (Ledger.TxBody era)
  , EncCBOR (Ledger.TxWits era)
  , EncCBOR (Ledger.TxAuxData era)
  , Ledger.EraTxBody era
  , Ledger.Era era
  ) =>
  ToJSON (Ledger.AlonzoTx era)
  where
  toJSON = Aeson.String . decodeUtf8 . Base16.encode . toStrict . serialize

instance
  ( DecCBOR (Annotator (Ledger.TxBody era))
  , DecCBOR (Annotator (Ledger.TxWits era))
  , DecCBOR (Annotator (Ledger.TxAuxData era))
  , Ledger.Era era
  ) =>
  FromJSON (Ledger.AlonzoTx era)
  where
  parseJSON value =
    -- We accept transactions in three forms:
    --
    -- (1) As a JSON 'text-envelope', which is a format defined and produced by
    -- the cardano-cli, wrapping base16-encoded strings as JSON objects with
    -- tags.
    parseAsEnvelopedBase16CBOR value
      -- (2) As base16 string representing a CBOR-serialized transaction, since
      -- this is the most common medium of exchange used for transactions.
      <|> parseHexEncodedCborAnnotated @era "Tx" value
   where
    parseAsEnvelopedBase16CBOR =
      withObject "Tx" $ \o -> do
        let TextEnvelopeType envelopeType = textEnvelopeType (proxyToAsType (Proxy @Tx))
        str <- o .: "cborHex"
        guard . (== envelopeType) =<< (o .: "type")
        parseHexEncodedCborAnnotated @era "Tx" (String str)

-- | Parse a hex-encoded CBOR value in given 'era'.
parseHexEncodedCbor ::
  forall era a.
  (Ledger.Era era, DecCBOR a) =>
  Text ->
  Aeson.Value ->
  Parser a
parseHexEncodedCbor lbl =
  withText (toString lbl) $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs ->
        case decodeFullDecoder version lbl decCBOR (fromStrict bs) of
          Left err -> fail $ show err
          Right v -> pure v
 where
  version = Ledger.eraProtVerLow @era

-- | Parse a hex-encoded, annotated CBOR value in given 'era'.
parseHexEncodedCborAnnotated ::
  forall era a.
  (Ledger.Era era, DecCBOR (Annotator a)) =>
  Text ->
  Aeson.Value ->
  Parser a
parseHexEncodedCborAnnotated lbl =
  withText (toString lbl) $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs ->
        case decodeFullAnnotator version lbl decCBOR (fromStrict bs) of
          Left err -> fail $ show err
          Right v -> pure v
 where
  version = Ledger.eraProtVerLow @era
