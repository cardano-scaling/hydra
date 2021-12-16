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
import Hydra.Chain.Direct.Tx (closeTx)
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import qualified Hydra.Contract.MockHead as MockHead
import Hydra.Ledger.Cardano (
  AlonzoEra,
  CardanoTx,
  LedgerCrypto,
  LedgerEra,
  Tx (Tx),
  TxBody (ShelleyTxBody),
  TxBodyScriptData (TxBodyNoScriptData, TxBodyScriptData),
  Utxo,
  describeCardanoTx,
  fromLedgerTx,
  fromLedgerUtxo,
  toLedgerTx,
  toLedgerUtxo,
 )
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromData, toData)
import Test.QuickCheck (
  Property,
  counterexample,
  forAll,
  property,
  suchThat,
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "On-chain contracts" $ do
  describe "Close" $ do
    prop "validates a proper close tx." $
      forAll genCloseTx propTransactionValidates
    prop "does not survive random mutations." $
      propMutation genCloseTx

--
-- Properties
--

propMutation :: Gen (CardanoTx, Utxo) -> Property
propMutation txGenerator =
  forAll txGenerator $ \(tx, lookupUtxo) ->
    forAll arbitrary $ \mutation ->
      forAll (applyMutation tx mutation) $ \tx' ->
        propTransactionDoesNotValidate (tx', lookupUtxo)

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
--
-- Mutation
--

data Mutation
  = ChangeHeadRedeemer
  deriving (Show, Generic)

instance Arbitrary Mutation where
  arbitrary = genericArbitrary

applyMutation :: CardanoTx -> Mutation -> Gen CardanoTx
applyMutation (Tx body wits) = \case
  ChangeHeadRedeemer -> do
    let ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
    body' <-
      alterRedeemers changeHeadRedeemer scriptData
        >>= \redeemers -> pure $ ShelleyTxBody era ledgerBody scripts redeemers mAuxData scriptValidity
    pure (Tx body' wits)

changeHeadRedeemer :: (Ledger.Data era, Ledger.ExUnits) -> Gen (Ledger.Data era, Ledger.ExUnits)
changeHeadRedeemer redeemer@(dat, units) =
  case fromData (Ledger.getPlutusData dat) of
    Just (_ :: MockHead.Input) -> do
      newRedeemer <-
        arbitrary `suchThat` \case
          MockHead.Close{} -> False
          _ -> True
      pure (Ledger.Data (toData newRedeemer), units)
    Nothing ->
      pure redeemer

alterRedeemers ::
  ((Ledger.Data LedgerEra, Ledger.ExUnits) -> Gen (Ledger.Data LedgerEra, Ledger.ExUnits)) ->
  TxBodyScriptData AlonzoEra ->
  Gen (TxBodyScriptData AlonzoEra)
alterRedeemers fn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData supportedInEra dats (Ledger.Redeemers redeemers) -> do
    newRedeemers <- traverse fn redeemers
    pure $ TxBodyScriptData supportedInEra dats (Ledger.Redeemers newRedeemers)

--
-- Generators
--

genCloseTx :: Gen (CardanoTx, Utxo)
genCloseTx = do
  utxo <- arbitrary
  headInput <- arbitrary
  let headOutput = mkHeadOutput (SJust headDatum)
      headDatum = Ledger.Data $ toData MockHead.Open
      lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput
  pure (fromLedgerTx $ closeTx 1 utxo (headInput, headOutput, headDatum), fromLedgerUtxo lookupUtxo)

---
--- Orphans
---

deriving instance Eq MockHead.Input

instance Arbitrary MockHead.Input where
  arbitrary = genericArbitrary
