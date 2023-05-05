{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Ledger.CardanoSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull, serialize')
import Cardano.Ledger.Credential (Credential (..))
import Data.Aeson (eitherDecode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import Data.Text (unpack)
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv)
import Hydra.Ledger (ChainSlot (..), applyTransactions)
import Hydra.Ledger.Cardano (
  cardanoLedger,
  genOneUTxOFor,
  genSequenceOfSimplePaymentTransactions,
  genTxOut,
  genUTxOAdaOnlyOfSize,
  genUTxOAlonzo,
  genUTxOFor,
 )
import Hydra.Ledger.Cardano.Evaluate (slotNoFromUTCTime, slotNoToUTCTime)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Cardano.Ledger.MaryEraGen ()
import Test.QuickCheck (Property, checkCoverage, conjoin, counterexample, cover, forAll, forAllBlind, property, sized, vectorOf, (.&&.), (===))
import Test.Util (propCollisionResistant)

spec :: Spec
spec =
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @AssetName)
    -- FIXME: Roundtrip instances for all JSON types we depend on
    roundtripAndGoldenSpecs (Proxy @UTxO)
    -- roundtripAndGoldenSpecs (Proxy @TxWitnesses)
    roundtripAndGoldenSpecs (Proxy @Tx)

    prop "Same TxId before/after JSON encoding" roundtripTxId

    prop "Roundtrip to and from Ledger" roundtripLedger

    prop "CBOR encoding of Tx" $ roundtripCBOR @Tx

    prop "applies valid transaction" appliesValidTransaction

    prop "applies valid transaction serialised from JSON" appliesValidTransactionFromJSON

    -- TODO(SN): rather ensure we use bech32 for addresses as a test
    it "should parse a Cardano.UTxO" $ do
      let bs =
            "{\"9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903#0\":\
            \  {\"address\":\"addr1vx35vu6aqmdw6uuc34gkpdymrpsd3lsuh6ffq6d9vja0s6spkenss\",\
            \   \"value\":{\"lovelace\":14}}}"
      shouldParseJSONAs @UTxO bs

    -- TODO(SN): rather ensure we use the right (cardano-api's) witness format as a test
    it "should parse a Tx" $ do
      let bs =
            "{\"witnesses\":\
            \    {\"keys\": [\"8200825820db995fe25169d141cab9bbba92baa01f9f2e1ece7df4cb2ac05190f37fcc1f9d58400599ccd0028389216631446cf0f9a4b095bbed03c25537595aa5a2e107e3704a55050c4ee5198a0aa9fc88007791ef9f3847cd96f3cb9a430d1c2d81c817480c\"]\
            \    },\
            \ \"body\":{\"outputs\":[{\"address\":\"addr1vx35vu6aqmdw6uuc34gkpdymrpsd3lsuh6ffq6d9vja0s6spkenss\",\"value\":{\"lovelace\":14}}],\
            \ \"inputs\":[\"9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903#0\"]},\
            \ \"auxiliaryData\":null}"
      shouldParseJSONAs @Tx bs

    describe "Generators" $ do
      propCollisionResistant "arbitrary @TxIn" (arbitrary @TxIn)
      propCollisionResistant "arbitrary @TxId" (arbitrary @TxId)
      propCollisionResistant "arbitrary @(VerificationKey PaymentKey)" (arbitrary @(VerificationKey PaymentKey))
      propCollisionResistant "arbitrary @(Hash PaymentKey)" (arbitrary @(Hash PaymentKey))
      propDoesNotCollapse "genUTxOAlonzo" genUTxOAlonzo
      propDoesNotCollapse "genUTxOAdaOnlyOfSize" (sized genUTxOAdaOnlyOfSize)
      propCollisionResistant "genUTxOFor" (genUTxOFor (arbitrary `generateWith` 42))
      propCollisionResistant "genOneUTxOFor" (genOneUTxOFor (arbitrary `generateWith` 42))

      describe "genTxOut" $ do
        it "does generate good values" $
          forAll genTxOut prop_generatesGoodTxOut

    describe "Evaluate helpers" $
      prop "slotNoFromUTCTime . slotNoToUTCTime === id" $ \slot ->
        slotNoFromUTCTime (slotNoToUTCTime slot) === slot

shouldParseJSONAs :: forall a. FromJSON a => LByteString -> Expectation
shouldParseJSONAs bs =
  case Aeson.eitherDecode bs of
    Left err -> failure err
    Right (_ :: a) -> pure ()

roundtripTxId :: Tx -> Property
roundtripTxId tx@(Tx body _) =
  case Aeson.decode (Aeson.encode tx) of
    Nothing ->
      property False
    Just tx'@(Tx body' _) ->
      (tx === tx' .&&. getTxId body === getTxId body')
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

appliesValidTransaction :: Property
appliesValidTransaction =
  forAllBlind genSequenceOfSimplePaymentTransactions $ \(utxo, txs) ->
    let result = applyTransactions (cardanoLedger defaultGlobals defaultLedgerEnv) (ChainSlot 0) utxo txs
     in case result of
          Right _ -> property True
          Left (tx, err) ->
            property False
              & counterexample ("Error: " <> show err)
              & counterexample ("Failing tx: " <> renderTx tx)
              & counterexample ("All txs: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON txs))
              & counterexample ("Initial UTxO: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON utxo))

appliesValidTransactionFromJSON :: Property
appliesValidTransactionFromJSON =
  forAllBlind genSequenceOfSimplePaymentTransactions $ \(utxo, txs) ->
    let encoded = encode txs
        result =
          eitherDecode encoded
            >>= first show . applyTransactions (cardanoLedger defaultGlobals defaultLedgerEnv) (ChainSlot 0) utxo
     in isRight result
          & counterexample ("Result: " <> show result)
          & counterexample ("All txs: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON txs))
          & counterexample ("Initial UTxO: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON utxo))

propDoesNotCollapse :: (Show (t a), Foldable t, Monoid (t a)) => String -> Gen (t a) -> Spec
propDoesNotCollapse name gen =
  prop (name <> " does not generate collapsing values") $
    forAll (vectorOf 100 gen) $ \xs ->
      sum (length <$> xs) === length (fold xs)

-- | Check that the given 'TxOut' fulfills several requirements and does not use
-- unsupported features. See 'genTxOut' for rationale.
prop_generatesGoodTxOut :: TxOut CtxUTxO -> Property
prop_generatesGoodTxOut txOut =
  checkCoverage $
    conjoin
      [ propNoReferenceScript
      , propNoByronAddress
      ]
      & cover 5 hasDatum "has datum"
      & cover 5 isVKOutput "is VK output"
      & cover 5 isScriptOutput "is Script output"
      & cover 1 hasOnlyADA "has only ADA"
      & cover 1 hasMultiAssets "has multiple assets "
 where
  propNoReferenceScript =
    txOutReferenceScript txOut
      === ReferenceScriptNone
      & counterexample "generated reference script"

  propNoByronAddress = case txOutAddress txOut of
    ByronAddressInEra ByronAddress{} -> property False & counterexample "generated byron address"
    ShelleyAddressInEra ShelleyAddress{} -> property True

  hasDatum = txOutDatum txOut /= TxOutDatumNone

  hasOnlyADA = all (\(an, _) -> an == AdaAssetId) assets

  hasMultiAssets = any (\(an, _) -> an /= AdaAssetId) assets

  assets = valueToList $ txOutValue txOut

  isVKOutput = case txOutAddress txOut of
    ByronAddressInEra ByronAddress{} -> False
    ShelleyAddressInEra (ShelleyAddress _ cred _) ->
      case cred of
        KeyHashObj{} -> True
        ScriptHashObj{} -> False

  isScriptOutput = case txOutAddress txOut of
    ByronAddressInEra ByronAddress{} -> False
    ShelleyAddressInEra (ShelleyAddress _ cred _) ->
      case cred of
        KeyHashObj{} -> False
        ScriptHashObj{} -> True
