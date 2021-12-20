{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

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
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (closeTx, policyId)
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
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
import Hydra.Party (MultiSigned (MultiSigned), SigningKey, deriveParty, sign, vkey)
import Hydra.Snapshot (Snapshot)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromData, toData)
import Test.QuickCheck (
  Property,
  counterexample,
  forAll,
  oneof,
  property,
  suchThat,
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "On-chain contracts" $ do
  describe "Close" $ do
    prop "is healthy" $
      propTransactionValidates healthyCloseTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseTx genCloseMutation

--
-- Properties
--

propMutation :: (CardanoTx, Utxo) -> ((CardanoTx, Utxo) -> Gen Mutation) -> Property
propMutation (tx, utxo) genMutation =
  forAll (genMutation (tx, utxo)) $ \mutation ->
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
  ( fromLedgerTx $ closeTx 1 multiSignedSnapshot (headInput, headOutput, headDatum)
  , fromLedgerUtxo lookupUtxo
  )
 where
  multiSignedSnapshot = MultiSigned [sign sk healthyOpenSnapshot | sk <- healthyPartyCredentials]
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyCloseDatum
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

healthyOpenSnapshot :: Snapshot CardanoTx
healthyOpenSnapshot = undefined

healthyCloseDatum :: MockHead.State
healthyCloseDatum = MockHead.Open (partyFromVerKey . vkey . deriveParty <$> healthyPartyCredentials)

healthyPartyCredentials :: [SigningKey]
healthyPartyCredentials = [1, 2, 3]

genCloseMutation :: (CardanoTx, Utxo) -> Gen Mutation
genCloseMutation (_tx, _utxo) =
  oneof
    [ ChangeHeadRedeemer <$> genChangeHeadRedeemer
    , ChangeHeadDatum <$> genChangeHeadDatum
    ]
 where
  genChangeHeadRedeemer =
    oneof
      [ arbitrary `suchThat` \case
          MockHead.Close{MockHead.snapshotNumber = snapshotNumber} -> snapshotNumber /= "1" -- TODO: magic number, this should come from tx
          _ -> True
      , do
          -- TODO: also here, we would want to alter the provided tx's redeemer, rather
          -- than generating a completly new one.
          signature' <- arbitrary
          pure $
            MockHead.Close
              { MockHead.signature = signature'
              , MockHead.snapshotNumber = "1" -- FIXME(SN): serialized snapshot number
              }
      ]
  genChangeHeadDatum =
    arbitrary `suchThat` \case
      MockHead.Open{MockHead.parties = parties} ->
        parties /= MockHead.parties healthyCloseDatum
      _ ->
        True

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
    Just (_ :: MockHead.Input) -> do
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
