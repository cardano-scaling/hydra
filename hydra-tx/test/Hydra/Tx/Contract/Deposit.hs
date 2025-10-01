-- | Healthy deposit transactions and mutations
-- As no Hydra script is run in these transactions, the mutations here should
-- make the deposit transaction not observed as a valid deposi.
module Hydra.Tx.Contract.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude

import Data.List qualified as List
import GHC.IsList qualified as GHC
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Tx (mkHeadId)
import Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import Hydra.Tx.Deposit (depositTx)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOSized)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..))
import Test.QuickCheck (chooseEnum, chooseInteger, elements)

genHealthyDepositTx :: Gen (Tx, UTxO)
genHealthyDepositTx = do
  -- XXX: Ideally we would want to have more arbitrary utxo here, but 'genUTxO'
  -- and other generators yield value quantities that fail to be put into
  -- transaction outputs.
  toDeposit <- genUTxOSized 1
  let tx =
        depositTx
          testNetworkId
          (mkHeadId testPolicyId)
          (mkSimpleBlueprintTx toDeposit)
          slot
          healthyDeadline
  pure (tx, toDeposit)
 where
  slot = chooseEnum (0, healthyDeadlineSlot) `generateWith` 42

healthyDeadline :: UTCTime
healthyDeadline = slotNoToUTCTime systemStart slotLength healthyDeadlineSlot

healthyDeadlineSlot :: SlotNo
healthyDeadlineSlot = arbitrary `generateWith` 42

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
