module Hydra.Tx.IsTxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

-- NOTE: Arbitrary UTxO and Tx instances
import Test.Hydra.Tx.Gen ()

import Hydra.Cardano.Api (Tx, fromLedgerTx, getTxId, toLedgerTx, pattern Tx)
import Hydra.Tx.IsTx (txId)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (Property, counterexample, forAll, property, (.&&.), (===))
import "aeson" Data.Aeson qualified as Aeson
import "base16-bytestring" Data.ByteString.Base16 qualified as Base16
import "cardano-binary" Cardano.Binary (decodeFull', serialize')
import "cardano-binary" Cardano.Binary qualified as CB
import "cardano-ledger-api" Cardano.Ledger.Api (bodyTxL, certsTxBodyL, inputsTxBodyL, updateTxBodyL)
import "cardano-ledger-api" Cardano.Ledger.Api qualified as Ledger
import "lens" Control.Lens ((.~), (^.))

spec :: Spec
spec =
  parallel $ do
    describe "Tx" $ do
      roundtripAndGoldenSpecs (Proxy @(ReasonablySized Tx))
      prop "Same TxId before/after JSON encoding" roundtripTxId
      prop "Same TxId as TxBody after JSON decoding" roundtripTxId'
      prop "Roundtrip to and from Ledger" roundtripLedger
      prop "Roundtrip CBOR encoding" $ roundtripCBOR @Tx
      prop "JSON decode Babbage era transactions" $
        forAll genConwayCompatibleBabbageTx $ \tx ->
          case Aeson.eitherDecode $ Aeson.encode $ fromLedgerTx tx of
            Left err ->
              property False
                & counterexample ("Failed to decode: " <> err)
            Right (decodedTx :: Tx) ->
              -- NOTE: Not full comparison as the decoding "upgraded" it to
              -- the latest era.
              tx ^. bodyTxL . inputsTxBodyL
                === toLedgerTx decodedTx ^. bodyTxL . inputsTxBodyL
      prop "CBOR decode Babbage era transactions" $
        forAll genConwayCompatibleBabbageTx $ \tx ->
          let encoded = serialize' $ fromLedgerTx tx
           in case decodeFull' encoded of
                Left err ->
                  property False
                    & counterexample ("Failed to decode: " <> show err)
                    & counterexample ("Encoded: " <> decodeUtf8 (Base16.encode encoded))
                Right (decodedTx :: Tx) ->
                  -- NOTE: Not full comparison as the decoding "upgraded" it to
                  -- the latest era.
                  tx ^. bodyTxL . inputsTxBodyL
                    === toLedgerTx decodedTx ^. bodyTxL . inputsTxBodyL

genConwayCompatibleBabbageTx :: Gen (Ledger.Tx Ledger.BabbageEra)
genConwayCompatibleBabbageTx = do
  tx <- arbitrary
  pure $
    tx
      & bodyTxL . certsTxBodyL .~ mempty
      & bodyTxL . updateTxBodyL .~ empty

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
