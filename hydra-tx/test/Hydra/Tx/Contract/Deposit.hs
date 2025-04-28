-- | Healthy deposit transactions and mutations
-- As no Hydra script is run in these transactions, the mutations here should
-- make the deposit transaction not observed as a valid deposi.
module Hydra.Tx.Contract.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude

import Data.List qualified as List
import GHC.IsList qualified as GHC
import Hydra.Tx (mkHeadId)
import Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import Hydra.Tx.Deposit (depositTx)
import Test.Hydra.Tx.Fixture (depositDeadline, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOSized)
import Test.Hydra.Tx.Mutation (Mutation (ChangeOutput), SomeMutation (..))
import Test.QuickCheck (chooseInteger, elements)

genHealthyDepositTx :: Gen (Tx, UTxO)
genHealthyDepositTx = do
  -- XXX: Ideally we would want to have more arbitrary utxo here, but 'genUTxO'
  -- and other generators yield value quantities that fail to be put into
  -- transaction outputs.
  healthyDepositUTxO <- genUTxOSized 1
  let tx =
        depositTx
          testNetworkId
          (mkHeadId testPolicyId)
          CommitBlueprintTx{blueprintTx = txSpendingUTxO healthyDepositUTxO, lookupUTxO = healthyDepositUTxO}
          depositDeadline -- TODO: should generate
  pure (tx, healthyDepositUTxO)

data DepositMutation
  = -- | Change the output value to a subset of the deposited value. This
    -- simulates an attack where someone claims to have deposited more than they
    -- actually did.
    MutateDepositOutputValue
  deriving (Show, Bounded, Enum)

genDepositMutation :: (Tx, UTxO) -> Gen SomeMutation
genDepositMutation (tx, _utxo) =
  SomeMutation [] MutateDepositOutputValue <$> do
    change <- do
      (asset, Quantity q) <- elements (GHC.toList $ txOutValue depositTxOut)
      diff <- fromInteger <$> chooseInteger (1, q)
      pure $ GHC.fromList [(asset, diff)]
    pure $ ChangeOutput 0 (depositTxOut & modifyTxOutValue (<> negateValue change))
 where
  depositTxOut = List.head $ txOuts' tx
