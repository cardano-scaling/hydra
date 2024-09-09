module Hydra.Tx.IsTxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

-- NOTE: Arbitrary UTxO and Tx instances
import Test.Hydra.Tx.Gen ()

import Cardano.Api.UTxO (fromApi, toApi)
import Cardano.Binary (serialize')
import Cardano.Binary qualified as CB
import Cardano.Ledger.Api (bodyTxL, inputsTxBodyL)
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Api.Tx (EraTxBody (outputsTxBodyL))
import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Hydra.Cardano.Api (Tx, UTxO, fromLedgerTx, getTxId, toLedgerTx, pattern Tx)
import Hydra.Tx.IsTx (balance, txId, utxoFromTx)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (Property, counterexample, forAll, property, (.&&.), (===))

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
      prop "JSON decode Babbage era transactions" $
        forAll (arbitrary @(Ledger.Tx Ledger.Babbage)) $ \tx ->
          case Aeson.eitherDecode $ Aeson.encode $ fromLedgerTx tx of
            Left err ->
              property False
                & counterexample ("Failed to decode: " <> err)
            Right (decodedTx :: Tx) ->
              -- NOTE: Not full comparison as the decoding "upgraded" it to
              -- the latest era.
              tx ^. bodyTxL . inputsTxBodyL
                === toLedgerTx decodedTx ^. bodyTxL . inputsTxBodyL

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
      decoded = CB.decodeFull $ fromStrict encoded
   in decoded == Right a
        & counterexample ("encoded: " <> show encoded <> ". decode: " <> show decoded)
