{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import Cardano.Ledger.Alonzo.Scripts (ExUnits)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Cardano.Ledger.Alonzo.Tools (
  BasicFailure,
  ScriptFailure,
  evaluateTransactionExecutionUnits,
 )
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (closeRedeemer, closeTx, policyId)
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import Hydra.Contract.MockHead (verifyPartySignature, verifySnapshotSignature)
import qualified Hydra.Contract.MockHead as MockHead
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Cardano (
  AlonzoEra,
  CardanoTx,
  CtxUTxO,
  Era,
  LedgerCrypto,
  LedgerEra,
  Tx (Tx),
  TxBody (ShelleyTxBody),
  TxBodyScriptData (TxBodyNoScriptData, TxBodyScriptData),
  TxOut (..),
  Utxo,
  describeCardanoTx,
  fromLedgerTx,
  fromLedgerUtxo,
  mkTxOutDatumHash,
  toLedgerTx,
  toLedgerUtxo,
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (
  MultiSigned (MultiSigned),
  Signed (UnsafeSigned),
  SigningKey,
  aggregate,
  deriveParty,
  generateKey,
  sign,
  toPlutusSignatures,
  vkey,
 )
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromData, toBuiltin, toData)
import Plutus.V1.Ledger.Crypto (Signature (Signature))
import Test.QuickCheck (
  Positive (Positive),
  Property,
  arbitrarySizedNatural,
  choose,
  counterexample,
  forAll,
  oneof,
  property,
  suchThat,
  withMaxSuccess,
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "On-chain contracts" $ do
  describe "Signature validator" $ do
    prop
      "verifies single signature produced off-chain"
      prop_verifyOffChainSignatures
    -- FIXME(AB): This property exists solely because our current multisignature implementation
    -- is just the aggregates of individual (mock) signatures and there is no point in doing some
    -- complicated shuffle logic to verify signatures given we'll end up verifying a single Ed25519
    -- signatures.
    prop
      "verifies snapshot multi-signature for list of parties and signatures"
      prop_verifySnapshotSignatures
  describe "Close" $ do
    prop "is healthy" $
      propTransactionValidates healthyCloseTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseTx genCloseMutation

--
-- Properties
--

prop_verifyOffChainSignatures :: Property
prop_verifyOffChainSignatures =
  forAll arbitrary $ \(snapshot :: Snapshot SimpleTx) ->
    forAll arbitrary $ \(Positive n) ->
      let sk = generateKey n
          UnsafeSigned signature = sign sk snapshot
          party = partyFromVerKey $ deriveVerKeyDSIGN sk
          snapshotNumber = toInteger $ number snapshot
       in verifyPartySignature snapshotNumber party (Signature $ toBuiltin signature)
            & counterexample ("signed: " <> show (BS.unpack signature))
            & counterexample ("party: " <> show party)
            & counterexample ("message: " <> show (getSignableRepresentation snapshot))

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll arbitrary $ \(snapshot :: Snapshot SimpleTx) ->
    forAll genListOfSigningKeys $ \sks ->
      let parties = partyFromVerKey . deriveVerKeyDSIGN <$> sks
          signatures = toPlutusSignatures $ aggregate [sign sk snapshot | sk <- sks]
          snapshotNumber = toInteger $ number snapshot
       in verifySnapshotSignature parties snapshotNumber signatures

propMutation :: (CardanoTx, Utxo) -> ((CardanoTx, Utxo) -> Gen (Text, Mutation)) -> Property
propMutation (tx, utxo) genMutation =
  forAll (genMutation (tx, utxo)) $ \(_label, mutation) ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate
      & withMaxSuccess 1000

propTransactionDoesNotValidate :: (CardanoTx, Utxo) -> Property
propTransactionDoesNotValidate (tx, lookupUtxo) =
  let result = evaluateTx tx lookupUtxo
   in counterexample "Should have not validated" $
        case result of
          Left _ ->
            property True
          Right redeemerReport ->
            any isLeft (Map.elems redeemerReport)
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
              & counterexample ("Redeemer report: " <> show redeemerReport)

propTransactionValidates :: (CardanoTx, Utxo) -> Property
propTransactionValidates (tx, lookupUtxo) =
  let result = evaluateTx tx lookupUtxo
   in counterexample "Should have validated" $
        case result of
          Left _ ->
            property False
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
          Right redeemerReport ->
            all isRight (Map.elems redeemerReport)
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
              & counterexample ("Redeemer report: " <> show redeemerReport)

--
-- Mutation
--

data Mutation
  = ChangeHeadRedeemer MockHead.Input
  | ChangeHeadDatum MockHead.State
  deriving (Show, Generic)

applyMutation :: Mutation -> (CardanoTx, Utxo) -> (CardanoTx, Utxo)
applyMutation mutation (tx@(Tx body wits), utxo) = case mutation of
  ChangeHeadRedeemer newRedeemer ->
    let ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
        redeemers = alterRedeemers (changeHeadRedeemer newRedeemer) scriptData
        body' = ShelleyTxBody era ledgerBody scripts redeemers mAuxData scriptValidity
     in (Tx body' wits, utxo)
  ChangeHeadDatum d' ->
    let fn o@(TxOut addr value _)
          | isHeadOutput o =
            (TxOut addr value $ mkTxOutDatumHash d')
          | otherwise =
            o
     in (tx, fmap fn utxo)

--
-- CloseTx
--

healthyCloseTx :: (CardanoTx, Utxo)
healthyCloseTx =
  ( fromLedgerTx tx
  , fromLedgerUtxo lookupUtxo
  )
 where
  tx = closeTx healthySnapshotNumber (healthySignature healthySnapshotNumber) (headInput, headOutput, headDatum)
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyCloseDatum
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

healthySnapshot :: Snapshot CardanoTx
healthySnapshot =
  Snapshot
    { number = healthySnapshotNumber
    , utxo = mempty
    , confirmed = []
    }

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthyCloseDatum :: MockHead.State
healthyCloseDatum = MockHead.Open (partyFromVerKey . vkey . deriveParty <$> healthyPartyCredentials)

healthyPartyCredentials :: [SigningKey]
healthyPartyCredentials = [1, 2, 3]

healthySignature :: SnapshotNumber -> MultiSigned (Snapshot CardanoTx)
healthySignature number = MultiSigned [sign sk snapshot | sk <- healthyPartyCredentials]
 where
  snapshot = healthySnapshot{number}

genCloseMutation :: (CardanoTx, Utxo) -> Gen (Text, Mutation)
genCloseMutation (_tx, _utxo) =
  oneof
    [ second ChangeHeadRedeemer <$> genChangeHeadRedeemer
    , second ChangeHeadDatum <$> genChangeHeadDatum
    ]
 where
  -- Mutations of the close redeemer
  genChangeHeadRedeemer =
    -- FIXME(SN): we need to ensure that each branch is tested at least once ->
    -- 'cover'?
    -- FIXME: using 'closeRedeemer' here is actually too high-level and reduces
    -- the power of the mutators, we should test at the level of the validator.
    -- That is, using the on-chain types. 'closeRedeemer' is also not used
    -- anywhere after changing this and can be moved into the closeTx
    oneof
      [ ("mutate signature, but not snapshot number",) <$> do
          closeRedeemer (number healthySnapshot) <$> arbitrary
      , ("mutate snapshot number, but not signature",) <$> do
          mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (/= healthySnapshotNumber)
          pure (closeRedeemer mutatedSnapshotNumber $ healthySignature healthySnapshotNumber)
      , ("mutate snapshot number to ill-formed value, with valid signature",) <$> do
          mutatedSnapshotNumber <- arbitrary `suchThat` (< 0)
          let mutatedSignature =
                MultiSigned [sign sk $ serialize' mutatedSnapshotNumber | sk <- healthyPartyCredentials]
          pure
            MockHead.Close
              { MockHead.snapshotNumber = mutatedSnapshotNumber
              , MockHead.signature = toPlutusSignatures mutatedSignature
              }
      ]

  genChangeHeadDatum =
    oneof
      [ ("mutate parties arbitrarily",) <$> arbitrary `suchThat` \case
          MockHead.Open{MockHead.parties = parties} ->
            parties /= MockHead.parties healthyCloseDatum
          _ ->
            True
      ]

--
-- Generators
--

genListOfSigningKeys :: Gen [SigningKey]
genListOfSigningKeys = choose (1, 20) <&> fmap generateKey . enumFromTo 1

genBytes :: Gen ByteString
genBytes = arbitrary

---
--- Orphans
---

deriving instance Eq MockHead.Input

instance Arbitrary MockHead.Input where
  arbitrary = genericArbitrary

instance Arbitrary MockHead.State where
  arbitrary = genericArbitrary

--
-- Helpers
--

isHeadOutput :: TxOut CtxUTxO Era -> Bool
isHeadOutput (TxOut addr _ _) = addr == headAddress
 where
  headAddress = Api.mkScriptAddress @Api.PlutusScriptV1 Fixture.testNetworkId headScript
  headScript = Api.fromPlutusScript $ MockHead.validatorScript policyId

changeHeadRedeemer :: MockHead.Input -> (Ledger.Data era, Ledger.ExUnits) -> (Ledger.Data era, Ledger.ExUnits)
changeHeadRedeemer newRedeemer redeemer@(dat, units) =
  case fromData (Ledger.getPlutusData dat) of
    Just (_ :: MockHead.Input) ->
      (Ledger.Data (toData newRedeemer), units)
    Nothing ->
      redeemer

alterRedeemers ::
  ((Ledger.Data LedgerEra, Ledger.ExUnits) -> (Ledger.Data LedgerEra, Ledger.ExUnits)) ->
  TxBodyScriptData AlonzoEra ->
  TxBodyScriptData AlonzoEra
alterRedeemers fn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData supportedInEra dats (Ledger.Redeemers redeemers) ->
    let newRedeemers = fmap fn redeemers
     in TxBodyScriptData supportedInEra dats (Ledger.Redeemers newRedeemers)

type RedeemerReport =
  (Map RdmrPtr (Either (ScriptFailure LedgerCrypto) ExUnits))

evaluateTx ::
  CardanoTx ->
  Utxo ->
  Either (BasicFailure LedgerCrypto) RedeemerReport
evaluateTx tx utxo =
  runIdentity $
    evaluateTransactionExecutionUnits
      Fixture.pparams
      (toLedgerTx tx)
      (toLedgerUtxo utxo)
      Fixture.epochInfo
      Fixture.systemStart
      Fixture.costModels
