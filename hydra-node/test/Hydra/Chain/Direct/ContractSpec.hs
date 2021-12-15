module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Alonzo.Tools (
  evaluateTransactionExecutionUnits,
 )
import qualified Data.Map as Map
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Ledger.Cardano (
  CardanoTx,
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

applyMutation :: [Mutation] -> CardanoTx -> Gen CardanoTx
applyMutation = error "TODO: applyMutation"

--
-- Generators
--

genCloseTx :: Gen (CardanoTx, Utxo)
genCloseTx = error "TODO: genCloseTx"
