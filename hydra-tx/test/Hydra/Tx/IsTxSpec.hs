module Hydra.Tx.IsTxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

-- NOTE: Arbitrary UTxO and Tx instances
import Test.Hydra.Tx.Gen ()

import Cardano.Binary (decodeFull, serialize')
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Hydra.Tx.IsTx (txId)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (Property, counterexample, property, (.&&.), (===))
import Hydra.Cardano.Api (pattern Tx, Tx, UTxO, fromLedgerTx, toLedgerTx, getTxId)
import Cardano.Api.UTxO (toApi, fromApi)

spec :: Spec
spec =
  parallel $ do
    describe "UTxO" $ do
      roundtripAndGoldenSpecs (Proxy @UTxO)
      prop "Roundtrip to and from Api" roundtripFromAndToApi

    describe "Tx" $ do
      roundtripAndGoldenSpecs (Proxy @(ReasonablySized Tx))

      prop "Same TxId before/after JSON encoding" roundtripTxId

      prop "Same TxId as TxBody after JSON decoding" roundtripTxId'

      prop "Roundtrip to and from Ledger" roundtripLedger

      prop "Roundtrip CBOR encoding" $ roundtripCBOR @Tx

roundtripFromAndToApi :: UTxO -> Property
roundtripFromAndToApi utxo =
  fromApi (toApi utxo) === utxo

roundtripTxId :: Tx -> Property
roundtripTxId tx@(Tx body _) =
  case Aeson.decode (Aeson.encode tx) of
    Nothing ->
      property False
    Just tx'@(Tx body' _) ->
      (tx === tx' .&&. getTxId body === getTxId body')
        & counterexample ("after:  " <> decodeUtf8 (Base16.encode $ serialize' tx'))
        & counterexample ("before: " <> decodeUtf8 (Base16.encode $ serialize' tx))

roundtripTxId' :: Tx -> Property
roundtripTxId' tx@(Tx body _) =
  case Aeson.decode (Aeson.encode tx) of
    Nothing ->
      property False
    Just tx'@(Tx body' _) ->
      (txId tx === getTxId body' .&&. txId tx' == getTxId body)
        & counterexample ("after:  " <> decodeUtf8 (Base16.encode $ serialize' tx'))
        & counterexample ("before: " <> decodeUtf8 (Base16.encode $ serialize' tx))

roundtripLedger :: Tx -> Property
roundtripLedger tx =
  fromLedgerTx (toLedgerTx tx) === tx

roundtripCBOR :: (Eq a, Show a, ToCBOR a, FromCBOR a) => a -> Property
roundtripCBOR a =
  let encoded = serialize' a
      decoded = decodeFull $ fromStrict encoded
   in decoded == Right a
        & counterexample ("encoded: " <> show encoded <> ". decode: " <> show decoded)
