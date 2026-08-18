-- | Healthy deposit transactions and mutations
-- As no Hydra script is run in these transactions, the mutations here should
-- make the deposit transaction not observed as a valid deposi.
module Hydra.Tx.Contract.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Data.List qualified as List
import GHC.IsList qualified as GHC
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Tx (mkHeadId)
import Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import Hydra.Tx.Deposit (depositTx)
import Test.Hydra.Tx.Fixture (defaultPParams, slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOSized, genVerificationKey)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..))
import Test.QuickCheck (chooseEnum, chooseInteger, elements, oneof)

genHealthyDepositTx :: Gen (Tx, UTxO)
genHealthyDepositTx = do
  -- XXX: Ideally we would want to have more arbitrary utxo here, but 'genUTxO'
  -- and other generators yield value quantities that fail to be put into
  -- transaction outputs.
  toDeposit <- genUTxOSized 1
  let tx =
        depositTx
          testNetworkId
          defaultPParams
          (mkHeadId testPolicyId)
          (mkSimpleBlueprintTx toDeposit)
          slot
          healthyDeadline
          Nothing
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
  | -- | Push the deposit off output index 0 by prepending another output. A
    -- deposit is identified by its transaction id alone, which only holds
    -- because it is that transaction's first output — 'recoverTx' spends
    -- @TxIn depositTxId (TxIx 0)@ and the increment validator requires index 0 —
    -- so a deposit anywhere else must not be observed, or parties could sign a
    -- snapshot committing something that can neither be claimed nor recovered.
    MoveDepositOffFirstOutput
  deriving stock (Show, Bounded, Enum)

genDepositMutation :: (Tx, UTxO) -> Gen SomeMutation
genDepositMutation (tx, _utxo) =
  oneof
    [ SomeMutation [] MutateDepositOutputValue <$> do
        change <- do
          (asset, Quantity q) <- elements (GHC.toList $ txOutValue depositTxOut)
          diff <- fromInteger <$> chooseInteger (1, q)
          pure $ GHC.fromList [(asset, diff)]
        pure $ ChangeOutput 0 (depositTxOut & modifyTxOutValue (<> negateValue change))
    , SomeMutation [] MoveDepositOffFirstOutput <$> do
        vk <- genVerificationKey
        pure $
          PrependOutput $
            TxOut (mkVkAddress testNetworkId vk) (lovelaceToValue 2_000_000) TxOutDatumNone ReferenceScriptNone
    ]
 where
  depositTxOut = List.head $ txOuts' tx
