-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Chain.Direct.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeMintedValueQuantityFrom,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Ledger.Cardano (genOneUTxOFor, genValue)
import Test.QuickCheck (choose, elements, oneof, suchThat, vectorOf)
import qualified Prelude

--
-- InitTx
--

healthyInitTx :: (Tx, UTxO)
healthyInitTx =
  (tx, healthyLookupUTxO)
 where
  tx =
    initTx
      testNetworkId
      healthyParties
      parameters
      healthySeedInput

  parameters =
    flip generateWith 42 $
      HeadParameters
        <$> arbitrary
        <*> vectorOf (length healthyParties) arbitrary

healthySeedInput :: TxIn
healthySeedInput =
  fst . Prelude.head $ UTxO.pairs healthyLookupUTxO

healthyParties :: [VerificationKey PaymentKey]
healthyParties =
  generateWith (vectorOf 3 arbitrary) 42

healthyLookupUTxO :: UTxO
healthyLookupUTxO =
  generateWith (genOneUTxOFor (Prelude.head healthyParties)) 42

data InitMutation
  = MutateThreadTokenQuantity
  | MutateAddAnotherPT
  | MutateInitialOutputValue
  | MutateDropInitialOutput
  | MutateDropSeedInput
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx 1
    , SomeMutation MutateAddAnotherPT <$> addPTWithQuantity tx 1
    , SomeMutation MutateInitialOutputValue <$> do
        let outs = txOuts' tx
        (ix, out) <- elements (zip [1 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation MutateDropInitialOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation MutateDropSeedInput <$> do
        pure $ RemoveInput healthySeedInput
    ]
