{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Ledger.CardanoSpec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)
import Test.Hspec (Expectation, Spec, describe, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.Hydra.Prelude

import Cardano.Ledger.Api (ensureMinCoinTxOut)
import Cardano.Ledger.Credential (Credential (..))
import Data.Aeson (eitherDecode, encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key)
import Data.Text (unpack)
import GHC.IsList (IsList (..))
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.JSONSchema (prop_validateJSONSchema)
import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Cardano (cardanoLedger, genSequenceOfSimplePaymentTransactions)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, defaultPParams)
import Test.Hydra.Tx.Gen (genOneUTxOFor, genOutput, genTxOut, genUTxOFor, genValue)
import Test.QuickCheck (
  Property,
  checkCoverage,
  conjoin,
  counterexample,
  cover,
  forAll,
  forAllBlind,
  property,
  (===),
 )
import Test.Util (propCollisionResistant)

spec :: Spec
spec =
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @AssetName)

    -- XXX: Move API conformance tests to API specs and add any missing ones
    describe "UTxO" $ do
      prop "JSON encoding of UTxO according to schema" $
        prop_validateJSONSchema @UTxO "api.json" $
          key "components" . key "schemas" . key "UTxO"

      -- TODO(SN): rather ensure we use bech32 for addresses as a test
      it "parses a specific UTxO" $ do
        let bs =
              "{\"9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903#0\":\
              \  {\"address\":\"addr1vx35vu6aqmdw6uuc34gkpdymrpsd3lsuh6ffq6d9vja0s6spkenss\",\
              \   \"value\":{\"lovelace\":14}}}"
        shouldParseJSONAs @UTxO bs

    describe "PParams" $
      prop "Roundtrip JSON encoding" roundtripPParams

    describe "Tx" $ do
      prop "JSON encoding of Tx according to schema" $
        prop_validateJSONSchema @Tx "api.json" $
          key "components" . key "schemas" . key "Transaction"

    describe "applyTransactions" $ do
      prop "works with valid transaction" appliesValidTransaction
      prop "works with valid transaction deserialised from JSON" appliesValidTransactionFromJSON

    describe "Generators" $ do
      propCollisionResistant "arbitrary @TxIn" (arbitrary @TxIn)
      propCollisionResistant "arbitrary @TxId" (arbitrary @TxId)
      propCollisionResistant "arbitrary @(VerificationKey PaymentKey)" (arbitrary @(VerificationKey PaymentKey))
      propCollisionResistant "arbitrary @(Hash PaymentKey)" (arbitrary @(Hash PaymentKey))
      propCollisionResistant "genUTxOFor" (genUTxOFor (arbitrary `generateWith` 42))
      propCollisionResistant "genOneUTxOFor" (genOneUTxOFor (arbitrary `generateWith` 42))

      describe "genTxOut" $
        it "does generate good values" $
          forAll genTxOut propGeneratesGoodTxOut

      describe "genOutput" $
        it "has enough lovelace to cover assets" $
          forAll (arbitrary >>= genOutput) propHasEnoughLovelace

      describe "genValue" $
        it "produces realistic values" $
          forAll genValue propRealisticValue

      describe "genChainPoint" $
        prop "generates only some genesis points" $
          checkCoverage $
            forAll genChainPoint $ \cp ->
              cover 80 (cp /= ChainPointAtGenesis) "not at genesis" $ property True

shouldParseJSONAs :: forall a. (HasCallStack, FromJSON a) => LByteString -> Expectation
shouldParseJSONAs bs =
  case Aeson.eitherDecode bs of
    Left err -> failure err
    Right (_ :: a) -> pure ()

-- | Test that the 'PParams' To/FromJSON instances to roundtrip.
roundtripPParams :: PParams LedgerEra -> Property
roundtripPParams pparams = do
  case Aeson.decode (Aeson.encode pparams) of
    Nothing ->
      property False
    Just actual ->
      pparams === actual

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
        result = eitherDecode encoded >>= first show . applyTransactions (cardanoLedger defaultGlobals defaultLedgerEnv) (ChainSlot 0) utxo
     in isRight result
          & counterexample ("Result: " <> show result)
          & counterexample ("All txs: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON txs))
          & counterexample ("Initial UTxO: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON utxo))

-- | A transaction or transaction output can usually only contain a realistic
-- number of native asset entries. This property checks a realistic order of
-- magnitude (100).
propRealisticValue :: Value -> Property
propRealisticValue value =
  numberOfAssets < 100
    & counterexample ("too many individual assets: " <> show numberOfAssets)
 where
  numberOfAssets = length (toList value)

-- | Check that an output has enough lovelace to cover asset deposits.
propHasEnoughLovelace :: TxOut CtxUTxO -> Property
propHasEnoughLovelace txOut =
  ensureMinCoinTxOut defaultPParams (toLedgerTxOut txOut) === toLedgerTxOut txOut
    & counterexample "ensureMinCoinTxOut deemed not enough lovelace in txOut"

-- | Check that the given 'TxOut' fulfills several requirements and does not use
-- unsupported features. See 'genTxOut' for rationale.
propGeneratesGoodTxOut :: TxOut CtxUTxO -> Property
propGeneratesGoodTxOut txOut =
  checkCoverage $
    conjoin
      [ propNoReferenceScript
      , propNoByronAddress
      , propRealisticValue (txOutValue txOut)
      , propHasEnoughLovelace txOut
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

  assets = toList $ txOutValue txOut

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
