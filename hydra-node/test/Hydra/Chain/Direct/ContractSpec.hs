{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
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
import Hydra.Contract.MockHead (hashBytes, mockSign, verifyPartySignature, verifySnapshotSignature)
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
import Hydra.Party (MultiSigned (MultiSigned), Signed (UnsafeSigned), SigningKey, deriveParty, generateKey, sign, vkey)
import Hydra.Snapshot (Snapshot (..))
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromBuiltin, fromData, toBuiltin, toData)
import Plutus.V1.Ledger.Crypto (Signature (Signature))
import Test.QuickCheck (
  Positive (Positive, getPositive),
  Property,
  choose,
  counterexample,
  forAll,
  oneof,
  property,
  suchThat,
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
  forAll genBytes $ \bs ->
    forAll arbitrary $ \(Positive n) ->
      let sk = generateKey n
          UnsafeSigned signature = sign sk bs
          party = partyFromVerKey $ deriveVerKeyDSIGN sk
          msg = toBuiltin bs
          hashed = hashBytes msg
          encoded = mockSign n hashed
       in verifyPartySignature msg party (Signature $ toBuiltin signature)
            & counterexample ("signed: " <> show (BS.unpack signature))
            & counterexample ("party: " <> show party)
            & counterexample ("encoded: " <> show (BS.unpack $ fromBuiltin encoded))
            & counterexample ("hashed: " <> show (BS.unpack $ fromBuiltin hashed))
            & counterexample ("message: " <> show (BS.unpack $ fromBuiltin msg))

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll genBytes $ \sn ->
    forAll listOfSigningKeys $ \sks ->
      let parties = partyFromVerKey . deriveVerKeyDSIGN <$> sks
          signatures = [Signature $ toBuiltin bytes | sk <- sks, let UnsafeSigned bytes = sign sk sn]
       in verifySnapshotSignature parties (toBuiltin sn) signatures

listOfSigningKeys :: Gen [SigningKey]
listOfSigningKeys = choose (1, 20) <&> fmap generateKey . enumFromTo 1

genBytes :: Gen ByteString
genBytes = arbitrary

propMutation :: (CardanoTx, Utxo) -> ((CardanoTx, Utxo) -> Gen (Text, Mutation)) -> Property
propMutation (tx, utxo) genMutation =
  forAll (genMutation (tx, utxo)) $ \(_label, mutation) ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate

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

evaluateTx :: CardanoTx -> Utxo -> Either (BasicFailure LedgerCrypto) (Map RdmrPtr (Either (ScriptFailure LedgerCrypto) ExUnits))
evaluateTx tx utxo =
  runIdentity $
    evaluateTransactionExecutionUnits
      Fixture.pparams
      (toLedgerTx tx)
      (toLedgerUtxo utxo)
      Fixture.epochInfo
      Fixture.systemStart
      Fixture.costModels

--
-- Healthy Transactions
--

healthyCloseTx :: (CardanoTx, Utxo)
healthyCloseTx =
  ( fromLedgerTx $ closeTx 1 healthySignature (headInput, headOutput, headDatum)
  , fromLedgerUtxo lookupUtxo
  )
 where
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyCloseDatum
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

healthySnapshot :: Snapshot CardanoTx
healthySnapshot =
  Snapshot
    { number = 1
    , utxo = mempty
    , confirmed = []
    }

healthyCloseDatum :: MockHead.State
healthyCloseDatum = MockHead.Open (partyFromVerKey . vkey . deriveParty <$> healthyPartyCredentials)

healthyPartyCredentials :: [SigningKey]
healthyPartyCredentials = [1, 2, 3]

healthySignature :: MultiSigned (Snapshot CardanoTx)
healthySignature = MultiSigned [sign sk healthySnapshot | sk <- healthyPartyCredentials]

genCloseMutation :: (CardanoTx, Utxo) -> Gen (Text, Mutation)
genCloseMutation (_tx, _utxo) =
  oneof
    [ second ChangeHeadRedeemer <$> genChangeHeadRedeemer
    , second ChangeHeadDatum <$> genChangeHeadDatum
    ]
 where
  -- Mutations of the close redeemer
  genChangeHeadRedeemer =
    -- FIXME(SN): we need to ensure that each branch is tested at least once -> 'cover'?
    oneof
      [ ("change signature, but not snapshot number",) . closeRedeemer (number healthySnapshot) <$> arbitrary
      , ("change snapshot number, but not signature",) <$> do
          mutatedSnapshotNumber <- fromInteger . getPositive <$> arbitrary
          pure (closeRedeemer mutatedSnapshotNumber healthySignature)
      ]

  genChangeHeadDatum =
    oneof
      [ ("change parties arbitrarily",) <$> arbitrary `suchThat` \case
          MockHead.Open{MockHead.parties = parties} ->
            parties /= MockHead.parties healthyCloseDatum
          _ ->
            True
      ]

--
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
