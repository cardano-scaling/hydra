-- | Healthy deposit transactions and mutations
-- As no Hydra script is run in these transactions, the mutations here should
-- make the deposit transaction not observed as a valid deposi.
module Hydra.Tx.Contract.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import GHC.IsList qualified as GHC
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit (DepositDatum)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Tx (mkHeadId)
import Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import Hydra.Tx.Deposit (depositTx)
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx (fromData, toData)
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Hydra.Tx.Fixture (defaultPParams, slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOSized, genVerificationKey)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), modifyInlineDatum)
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
  | -- | Repeat a commit's input in the datum. 'UTxO.fromList' is keyed by 'TxIn',
    -- so the two collapse into one entry off-chain while the validators hash the
    -- list as written — two copies of the same bytes. The deposit output's value is
    -- left alone so it still matches the collapsed total, i.e. only the count check
    -- can reject this.
    DuplicateCommitInput
  | -- | Give a commit a reference script, which 'fromPlutusTxOut' drops. The commit
    -- then round-trips to different bytes than the datum holds, so every hash
    -- derived from the off-chain UTxO differs from the one the validators compute.
    CommitWithReferenceScript
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
    , SomeMutation [] DuplicateCommitInput <$> do
        pure $
          ChangeOutput 0 $
            flip modifyInlineDatum depositTxOut $
              \((headCS, deadline, commits) :: DepositDatum) ->
                (headCS, deadline, commits <> take 1 commits)
    , SomeMutation [] CommitWithReferenceScript <$> do
        pure $
          ChangeOutput 0 $
            flip modifyInlineDatum depositTxOut $
              \((headCS, deadline, commits) :: DepositDatum) ->
                (headCS, deadline, withReferenceScript <$> commits)
    ]
 where
  depositTxOut = List.head $ txOuts' tx

  -- Re-encode a commit's output with a reference script, which the off-chain
  -- 'TxOut' cannot carry: 'fromPlutusTxOut' hardcodes 'ReferenceScriptNone', so the
  -- commit no longer round-trips to the bytes the datum holds.
  withReferenceScript commit@Commit.Commit{Commit.preSerializedOutput} =
    case deserialiseOrFail . LBS.fromStrict $ fromBuiltin preSerializedOutput of
      Left{} -> commit
      Right dat ->
        case fromData dat of
          Nothing -> commit
          Just out ->
            commit
              { Commit.preSerializedOutput =
                  toBuiltin . LBS.toStrict . serialise . toData $
                    (out :: Plutus.TxOut){Plutus.txOutReferenceScript = Just refScriptHash}
              }

  refScriptHash = Plutus.ScriptHash . toBuiltin $ BS.replicate 28 0
