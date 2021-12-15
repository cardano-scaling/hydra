module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Alonzo.Tools (
  evaluateTransactionExecutionUnits,
 )
import Cardano.Ledger.Alonzo.TxWitness (unRedeemers)
import qualified Data.Map as Map
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Ledger.Cardano (
  CardanoTx,
  ShelleyBasedEra (ShelleyBasedEraAlonzo),
  Tx (Tx),
  TxBody (ShelleyTxBody),
  TxBodyScriptData (TxBodyNoScriptData, TxBodyScriptData),
  Utxo,
  describeCardanoTx,
  toLedgerTx,
  toLedgerUtxo,
 )
import Test.QuickCheck (
  Property,
  counterexample,
  forAll,
  property,
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "On-chain contracts" $ do
  describe "Close" $ do
    prop "close survives random mutations." $
      propMutation genCloseTx

--
-- Properties
--

propMutation :: Gen (CardanoTx, Utxo) -> Property
propMutation txGenerator =
  forAll txGenerator $ \(tx, lookupUtxo) ->
    forAll arbitrary $ \mutations ->
      forAll (applyMutation mutations tx) $ \tx' ->
        propTransactionDoesNotValidate (tx', lookupUtxo)

propTransactionDoesNotValidate :: (CardanoTx, Utxo) -> Property
propTransactionDoesNotValidate (tx, lookupUtxo) =
  let result =
        runIdentity $
          evaluateTransactionExecutionUnits
            Fixture.pparams
            (toLedgerTx tx)
            (toLedgerUtxo lookupUtxo)
            Fixture.epochInfo
            Fixture.systemStart
            Fixture.costModels
   in case result of
        Left _ ->
          property True
        Right redeemerReport ->
          any isLeft (Map.elems redeemerReport)
            & counterexample ("Tx: " <> toString (describeCardanoTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
            & counterexample ("Redeemer report: " <> show redeemerReport)

--
-- Mutation
--

data Mutation
  = ChangeHeadRedeemer
  deriving (Show, Generic)

instance Arbitrary Mutation where
  arbitrary = genericArbitrary

applyMutation :: Mutation -> CardanoTx -> Gen CardanoTx
applyMutation ChangeHeadRedeemer tx = error "not implemented"
 where
  Tx body _wits = tx

  ShelleyTxBody _era _body _scripts scriptData _mAuxData _scriptValidity = body

  foo = \case
    TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
    TxBodyScriptData support dats reds -> error "wip"

--
-- Generators
--

genCloseTx :: Gen (CardanoTx, Utxo)
genCloseTx = error "TODO: genCloseTx"
