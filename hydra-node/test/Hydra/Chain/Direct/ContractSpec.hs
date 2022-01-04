{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
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
import Hydra.Chain.Direct.Fixture (testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (closeRedeemer, closeTx, policyId)
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import qualified Hydra.Contract.Hash as Hash
import Hydra.Contract.MockHead (naturalToCBOR, verifyPartySignature, verifySnapshotSignature)
import qualified Hydra.Contract.MockHead as MockHead
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Cardano (
  AlonzoEra,
  BuildTxWith (BuildTxWith),
  CardanoTx,
  CtxUTxO,
  Era,
  LedgerCrypto,
  LedgerEra,
  PlutusScriptV1,
  Tx (Tx),
  TxBody (ShelleyTxBody),
  TxBodyScriptData (TxBodyNoScriptData, TxBodyScriptData),
  TxOut (..),
  Utxo,
  Utxo' (Utxo),
  addInputs,
  describeCardanoTx,
  emptyTxBody,
  fromAlonzoExUnits,
  fromLedgerTx,
  fromLedgerUtxo,
  fromPlutusScript,
  lovelaceToTxOutValue,
  mkDatumForTxIn,
  mkRedeemerForTxIn,
  mkScriptAddress,
  mkScriptWitness,
  mkTxOutDatum,
  mkTxOutDatumHash,
  toCtxUTxOTxOut,
  toLedgerTx,
  toLedgerUtxo,
  unsafeBuildTransaction,
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
import Plutus.V1.Ledger.Api (fromBuiltin, fromData, toBuiltin, toData)
import Plutus.V1.Ledger.Crypto (Signature (Signature))
import Test.QuickCheck (
  Positive (Positive),
  Property,
  arbitrarySizedNatural,
  checkCoverage,
  choose,
  counterexample,
  forAll,
  oneof,
  property,
  suchThat,
 )
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  prop "correctly encode 'small' integer to CBOR" prop_encode16BitsNaturalToCBOROnChain
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

  describe "Hash" $
    prop "execution units" $ \input algorithm ->
      let output = toCtxUTxOTxOut $ TxOut address value (mkTxOutDatum datum)
          value = lovelaceToTxOutValue 1_000_000
          address = mkScriptAddress @PlutusScriptV1 testNetworkId script
          tx = unsafeBuildTransaction $ emptyTxBody & addInputs [(input, witness)]
          utxo = Utxo $ Map.singleton input output
          witness = BuildTxWith $ mkScriptWitness script (mkDatumForTxIn datum) redeemer
          script = fromPlutusScript Hash.validatorScript
          bytes = fold $ replicate 10000 ("a" :: ByteString)
          datum = Hash.datum $ toBuiltin bytes
          redeemer = mkRedeemerForTxIn $ Hash.redeemer algorithm
       in case evaluateTx tx utxo of
            Left basicFailure ->
              property False
                & counterexample ("Basic failure: " <> show basicFailure)
            Right report ->
              case Map.elems report of
                [Right units] ->
                  property True
                    & counterexample ("Redeemer report: " <> show report)
                    & counterexample ("Tx: " <> show tx)
                    & QC.label (show algorithm <> ":" <> show (fromAlonzoExUnits units))
                _ ->
                  property False
                    & counterexample ("Too many redeemers in report: " <> show report)

--
-- Properties
--

prop_encode16BitsNaturalToCBOROnChain :: Property
prop_encode16BitsNaturalToCBOROnChain =
  forAll arbitrary $ \(word16 :: Word16) ->
    let offChainCBOR = serialize' word16
        onChainCBOR = fromBuiltin $ naturalToCBOR (fromIntegral word16)
     in offChainCBOR == onChainCBOR
          & counterexample ("offChainCBOR: " <> show offChainCBOR)
          & counterexample ("onChainCBOR: " <> show onChainCBOR)

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

propMutation :: (CardanoTx, Utxo) -> ((CardanoTx, Utxo) -> Gen SomeMutation) -> Property
propMutation (tx, utxo) genMutation =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate
      & genericCoverTable [label]
      & checkCoverage

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

data SomeMutation = forall lbl.
  (Typeable lbl, Enum lbl, Bounded lbl, Show lbl) =>
  SomeMutation
  { label :: lbl
  , mutation :: Mutation
  }
deriving instance Show SomeMutation

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

data CloseMutation
  = MutateSignatureButNotSnapshotNumber
  | MutateSnapshotNumberButNotSignature
  | MutateSnapshotToIllFormedValue
  | MutateParties
  deriving (Generic, Show, Enum, Bounded)

genCloseMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genCloseMutation (_tx, _utxo) =
  -- FIXME: using 'closeRedeemer' here is actually too high-level and reduces
  -- the power of the mutators, we should test at the level of the validator.
  -- That is, using the on-chain types. 'closeRedeemer' is also not used
  -- anywhere after changing this and can be moved into the closeTx
  oneof
    [ SomeMutation MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        closeRedeemer (number healthySnapshot) <$> arbitrary
    , SomeMutation MutateSnapshotNumberButNotSignature . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (/= healthySnapshotNumber)
        pure (closeRedeemer mutatedSnapshotNumber $ healthySignature healthySnapshotNumber)
    , SomeMutation MutateSnapshotToIllFormedValue . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (< 0)
        let mutatedSignature =
              MultiSigned [sign sk $ serialize' mutatedSnapshotNumber | sk <- healthyPartyCredentials]
        pure
          MockHead.Close
            { MockHead.snapshotNumber = mutatedSnapshotNumber
            , MockHead.signature = toPlutusSignatures mutatedSignature
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> arbitrary `suchThat` \case
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
