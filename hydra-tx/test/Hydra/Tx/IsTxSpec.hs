module Hydra.Tx.IsTxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude (Arbitrary (arbitrary), Gen, ReasonablySized, Spec, describe, parallel, prop)

-- NOTE: Arbitrary UTxO and Tx instances
import Test.Hydra.Tx.Gen ()

import Cardano.Binary (decodeFull', serialize')
import Cardano.Binary qualified as CB
import Cardano.Ledger.Api (bodyTxL, certsTxBodyL, inputsTxBodyL, updateTxBodyL)
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Core (TxLevel (..))
import Control.Lens ((.~), (^.))
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Cardano.Api (CtxUTxO, Tx, TxId, TxIn (..), TxIx (..), TxOut, UTxO, fromLedgerTx, getTxId, toLedgerTx, toLedgerTxIn, pattern Tx, pattern UTxO)
import Hydra.Tx.IsTx (txId, txInputsAvailable, withoutTxInputs)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (Property, conjoin, counterexample, forAll, property, (.&&.), (===))

spec :: Spec
spec =
  parallel $ do
    describe "Tx" $ do
      roundtripAndGoldenSpecs (Proxy @(ReasonablySized Tx))
      prop "requires spending, reference and collateral inputs without consuming references" requiredInputs
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

genConwayCompatibleBabbageTx :: Gen (Ledger.Tx TopTx Ledger.BabbageEra)
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

-- This checks UTxO references, independently of script and witness validity.
requiredInputs :: Tx -> TxId -> TxOut CtxUTxO -> Property
requiredInputs original tid output =
  let spending = TxIn tid (TxIx 0)
      reference = TxIn tid (TxIx 1)
      collateral = TxIn tid (TxIx 2)
      unrelated = TxIn tid (TxIx 3)
      tx =
        fromLedgerTx $
          toLedgerTx original
            & bodyTxL . inputsTxBodyL .~ Set.singleton (toLedgerTxIn spending)
            & bodyTxL . Ledger.referenceInputsTxBodyL .~ Set.singleton (toLedgerTxIn reference)
            & bodyTxL . Ledger.collateralInputsTxBodyL .~ Set.singleton (toLedgerTxIn collateral)
      entries = Map.fromList [(ref, output) | ref <- [spending, reference, collateral, unrelated]]
   in conjoin $
        [ txInputsAvailable tx (UTxO entries) === True
        , withoutTxInputs tx (UTxO entries) === (UTxO (Map.delete spending entries) :: UTxO)
        , withoutTxInputs tx mempty === mempty
        ]
          <> [txInputsAvailable tx (UTxO $ Map.delete missing entries) === False | missing <- [spending, reference, collateral]]
