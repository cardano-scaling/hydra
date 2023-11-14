{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Hydra.Chain.Direct.Contract.FanOut where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens, changeMintedValueQuantityFrom)
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (fanoutTx, mkHeadOutput, hydraHeadV1AssetName)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript, headPolicyId)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (
  adaOnly,
  genOutput,
  genUTxOWithSimplifiedAddresses,
  genValue,
 )
import Hydra.Ledger.Cardano.Evaluate (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Party (partyToChain)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import PlutusTx.Builtins (toBuiltin)
import Test.QuickCheck (choose, elements, oneof, suchThat, vectorOf)
import Test.QuickCheck.Instances ()
import Hydra.Contract.CommitError (CommitError(STNotBurnedError))
import Hydra.Contract.InitialError (InitialError(STNotBurned))

healthyFanoutTx :: (Tx, UTxO)
healthyFanoutTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, headOutput)
      <> registryUTxO scriptRegistry

  tx =
    fanoutTx
      scriptRegistry
      healthyFanoutUTxO
      (headInput, headOutput)
      healthySlotNo
      headTokenScript

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput' = mkHeadOutput testNetworkId testPolicyId (toUTxOContext $ mkTxOutDatumInline healthyFanoutDatum)

  parties = generateWith (vectorOf 3 (arbitrary @(VerificationKey PaymentKey))) 42

  headOutput = modifyTxOutValue (<> participationTokens) headOutput'

  participationTokens =
    valueFromList $
      map
        ( \vk ->
            (AssetId testPolicyId (AssetName . serialiseToRawBytes . verificationKeyHash $ vk), 1)
        )
        parties

healthyFanoutUTxO :: UTxO
healthyFanoutUTxO =
  -- FIXME: fanoutTx would result in 0 outputs and MutateChangeOutputValue below fail
  adaOnly <$> generateWith (genUTxOWithSimplifiedAddresses `suchThat` (not . null)) 42

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime $ healthySlotNo - 1

healthyFanoutDatum :: Head.State
healthyFanoutDatum =
  Head.Closed
    { snapshotNumber = 1
    , utxoHash = toBuiltin $ hashUTxO @Tx healthyFanoutUTxO
    , parties = partyToChain <$> arbitrary `generateWith` 42
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , contestationPeriod = healthyContestationPeriod
    , headId = toPlutusCurrencySymbol testPolicyId
    , contesters = []
    }
 where
  healthyContestationPeriodSeconds = 10

  healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

data FanoutMutation
  = MutateAddUnexpectedOutput
  | MutateChangeOutputValue
  | MutateValidityBeforeDeadline
  | -- | Meant to test that the minting policy is burning all PTs present in tx
    MutateThreadTokenQuantity
  | -- | State token is not burned
    DoNotBurnST
  | -- | Here we want to check that the initial validator also fails on abort.
    DoNotBurnSTInitial
  deriving stock (Generic, Show, Enum, Bounded)

genFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genFanoutMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutput
    , SomeMutation (Just $ toErrorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) MutateChangeOutputValue <$> do
        let outs = txOuts' tx
        -- NOTE: Assumes the fanout transaction has non-empty outputs, which
        -- might not be always the case when testing unbalanced txs and we need
        -- to ensure it by at least one utxo is in healthyFanoutUTxO
        (ix, out) <- elements (zip [0 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (Just $ toErrorCode LowerBoundBeforeContestationDeadline) MutateValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , SomeMutation (Just $ toErrorCode BurntTokenNumberMismatch) MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx (-1)
    , SomeMutation (Just $ toErrorCode STNotBurnedError) DoNotBurnST
        <$> changeMintedTokens tx (valueFromList [(AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName, 1)])
    , SomeMutation (Just $ toErrorCode STNotBurned) DoNotBurnSTInitial
        <$> changeMintedTokens tx (valueFromList [(AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName, 1)])
    ]
 where
  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)
