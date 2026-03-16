{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Ledger.CardanoSpec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)
import Test.Hydra.Prelude

import Cardano.Ledger.Api (ensureMinCoinTxOut)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoSlotToRelativeTime, fixedEpochInfo, hoistEpochInfo)
import Cardano.Slotting.Time (RelativeTime (..), mkSlotLength)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key)
import Data.SOP.NonEmpty (NonEmpty (NonEmptyCons, NonEmptyOne))
import Data.Text (unpack)
import GHC.IsList (IsList (..))
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.JSONSchema (prop_validateJSONSchema)
import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Cardano (cardanoLedger)
import Ouroboros.Consensus.Block (GenesisWindow (..))
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  Summary (Summary),
  initBound,
  mkInterpreter,
 )
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Consensus.Shelley.Crypto (StandardCrypto)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Gen.Cardano.Api.Typed (genChainPoint)
import Test.Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, defaultPParams)
import Test.Hydra.Tx.Gen (genOneUTxOFor, genOutputFor, genTxOut, genUTxOFor, genValue)
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
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.Util (propCollisionResistant)

spec :: Spec
spec =
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @AssetName)

    describe "EpochInfo" $ do
      it "fixedEpochInfo gives wrong slot-to-time for multi-era chains" $ do
        -- On a real chain (mainnet/testnet), Byron era has 20s slots and
        -- Shelley+ has 1s slots. fixedEpochInfo uses a single slot length,
        -- which produces wrong POSIXTime conversions for Plutus scripts.
        let
          -- Byron era: 21600 slots per epoch, 20s per slot, runs for 208 epochs
          byronSlots = 21600 * 208 -- 4,492,800 slots
          byronSecondsPerSlot = 20
          byronDuration = byronSlots * byronSecondsPerSlot -- seconds
          -- A slot well into the Shelley era (1s slots)
          testSlot = SlotNo (byronSlots + 1000)
          -- Expected time: byron duration + 1000 seconds (1s per Shelley slot)
          expectedRelativeTime = byronDuration + 1000

          -- What fixedEpochInfo computes (assumes 1s slots from genesis):
          fixedRelativeTime = unSlotNo testSlot -- 4,493,800 seconds

        -- The multi-era EpochInfo should give a different (correct) time
        -- than fixedEpochInfo. fixedEpochInfo assumes all slots are 1s,
        -- but the Byron slots were actually 20s each.
        expectedRelativeTime `shouldNotBe` fixedRelativeTime

        -- The actual time via era-aware interpreter
        let eraHistory = multiEraHistory
            EraHistory interpreter = eraHistory
        case Consensus.interpretQuery interpreter (Consensus.slotToWallclock testSlot) of
          Left err -> expectationFailure $ "Failed to query era history: " <> show err
          Right (relTime, _slotLen) -> do
            -- Era-aware gives the correct time (byron duration + 1000s)
            relTime `shouldBe` RelativeTime (fromIntegral expectedRelativeTime)
            -- This differs from what fixedEpochInfo would compute
            relTime `shouldNotBe` RelativeTime (fromIntegral fixedRelativeTime)

      it "fixedEpochInfo causes wrong Globals for L2 Plutus evaluation on mainnet" $ do
        -- This test demonstrates the actual bug: Globals constructed with
        -- fixedEpochInfo (as in newGlobals) will have a different epochInfo
        -- than Globals constructed with the correct era-aware EpochInfo.
        -- When Ledger.applyTx evaluates Plutus scripts, it uses
        -- Globals.epochInfo to convert slot numbers to POSIXTime in the
        -- ScriptContext. With the wrong epochInfo, time-sensitive Plutus
        -- scripts (like Close, Contest, Fanout) receive incorrect time values.
        let shelleySlotLength = mkSlotLength 1
            shelleyEpochSize = EpochSize 432000
            -- This is what newGlobals currently does (wrong for multi-era):
            fixedEI = fixedEpochInfo shelleyEpochSize shelleySlotLength
            -- This is what it should do (correct for multi-era):
            EraHistory interpreter = multiEraHistory
            eraAwareEI :: EpochInfo (Either Text)
            eraAwareEI =
              hoistEpochInfo (first show . runExcept) $
                Consensus.interpreterToEpochInfo interpreter
            -- A slot in the Shelley era
            testSlot = SlotNo 5000000

        -- The two Globals will produce different results when the ledger
        -- converts slots to POSIXTime for Plutus script evaluation.
        -- We verify the epochInfos disagree on the time for the test slot.
        let fixedResult = epochInfoSlotToRelativeTime fixedEI testSlot :: Either Text RelativeTime
            eraAwareResult = epochInfoSlotToRelativeTime eraAwareEI testSlot :: Either Text RelativeTime
        case (fixedResult, eraAwareResult) of
          (Left err, _) -> expectationFailure $ "Fixed epochInfo failed: " <> show err
          (_, Left err) -> expectationFailure $ "Era-aware epochInfo failed: " <> show err
          (Right fixedTime, Right eraTime) -> do
            -- fixedEpochInfo says: slot 5000000 * 1s = 5000000s from system start
            fixedTime `shouldBe` RelativeTime 5000000
            -- Era-aware says: byron(4492800 slots * 20s) + shelley(507200 slots * 1s)
            --               = 89856000 + 507200 = 90363200s
            eraTime `shouldNotBe` fixedTime

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

      describe "genOutputFor" $
        it "has enough lovelace to cover assets" $
          forAll (arbitrary >>= genOutputFor) propHasEnoughLovelace

      describe "genValue" $
        it "produces realistic values" $
          forAll genValue propRealisticValue

      describe "genChainPoint" $
        prop "generates only some genesis points" $
          checkCoverage $
            forAll (hedgehog genChainPoint) $ \cp ->
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

-- | A realistic multi-era 'EraHistory' mimicking mainnet/testnet where:
-- - Byron era: 21600 slots/epoch, 20s/slot, runs for 208 epochs (4,492,800 slots)
-- - Shelley+ era: 432000 slots/epoch, 1s/slot, open-ended
--
-- This causes slot-to-time conversions to differ from a simple fixedEpochInfo
-- because Byron slots are 20x longer than Shelley slots.
multiEraHistory :: EraHistory
multiEraHistory =
  EraHistory (mkInterpreter summary)
 where
  byronSlotsPerEpoch = 21600
  byronEpochs = 208
  byronSlots = byronSlotsPerEpoch * byronEpochs -- 4,492,800
  byronSlotLength = mkSlotLength 20
  byronDurationSeconds = fromIntegral byronSlots * 20 -- 89,856,000 seconds
  summary :: Summary (CardanoEras StandardCrypto)
  summary =
    Summary $
      NonEmptyCons
        byronEra
        ( NonEmptyOne shelleyEra
        )

  byronEra =
    EraSummary
      { eraStart = initBound
      , eraEnd =
          EraEnd
            Bound
              { boundTime = RelativeTime byronDurationSeconds
              , boundSlot = SlotNo byronSlots
              , boundEpoch = EpochNo byronEpochs
              }
      , eraParams =
          EraParams
            { eraEpochSize = EpochSize byronSlotsPerEpoch
            , eraSlotLength = byronSlotLength
            , eraSafeZone = StandardSafeZone (2 * byronSlotsPerEpoch)
            , eraGenesisWin = GenesisWindow (2 * byronSlotsPerEpoch)
            }
      }

  shelleyEra =
    EraSummary
      { eraStart =
          Bound
            { boundTime = RelativeTime byronDurationSeconds
            , boundSlot = SlotNo byronSlots
            , boundEpoch = EpochNo byronEpochs
            }
      , eraEnd = EraUnbounded
      , eraParams =
          EraParams
            { eraEpochSize = EpochSize 432000
            , eraSlotLength = mkSlotLength 1
            , eraSafeZone = UnsafeIndefiniteSafeZone
            , eraGenesisWin = GenesisWindow 432000
            }
      }
