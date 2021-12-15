module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Alonzo.Tools (
  BasicFailure,
  ScriptFailure,
  evaluateTransactionExecutionUnits,
 )
import Hydra.Chain.Direct.Fixture ()
import Hydra.Ledger.Cardano (
  CardanoTx,
  Utxo,
 )
import Test.QuickCheck (
  Property,
  counterexample,
  forAll,
 )
import Test.QuickCheck.Instances ()
import qualified Prelude

spec :: Spec
spec = describe "On-chain contracts" $ do
  describe "Close" $ do
    prop "close survives random mutations." (propMutation genCloseTx)

--
-- Properties
--

propMutation :: (CardanoTx, Utxo) -> [Mutation] -> Property
propMutation (tx, lookupUtxo) mutations = do
  forAll (applyMutation mutations tx) $ \tx' ->
    propTransactionValidates tx' lookupUtxo

propTransactionValidates :: (CardanoTx, Utxo) -> Property
propTransactionValidates =
  let result =
        runIdentity $
          evaluateTransactionExecutionUnits
            Fixture.pparams
            tx
            lookupUtxo
            Fixture.epochInfo
            Fixture.systemStart
            Fixture.costModels
   in case result of
        Left basicFailure ->
          property False & counterexample ("Basic failure: " <> show basicFailure)
        Right redeemerReport ->
          all isRight (Map.elems redeemerReport)
            & counterexample ("Tx: " <> toString (describeCardanoTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty utxo))
            & counterexample ("Redeemer report: " <> show redeemerReport)

--
-- Mutation
--

data Mutation
  = AddArbitraryOutput
  deriving (Show)

instance Arbitrary Mutation where
  arbitrary = error "TODO: Arbitrary@Mutation"

applyMutation :: [Mutation] -> CardanoTx -> Gen CardanoTx
applyMutation = error "TODO: applyMutation"

--
-- Generators
--

genCloseTx :: Gen (CardanoTx, Utxo)
genCloseTx = error "TODO: genCloseTx"
