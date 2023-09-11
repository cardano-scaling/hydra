{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.FanOut where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..))
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (fanoutTx, mkHeadOutput)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import qualified Hydra.Data.ContestationPeriod as OnChain
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
      (headInput, headOutput, headDatum)
      healthySlotNo
      headTokenScript

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput' = mkHeadOutput testNetworkId testPolicyId (toUTxOContext $ mkTxOutDatum healthyFanoutDatum)

  parties = generateWith (vectorOf 3 (arbitrary @(VerificationKey PaymentKey))) 42

  headOutput = modifyTxOutValue (<> participationTokens) headOutput'

  participationTokens =
    valueFromList $
      map
        ( \vk ->
            (AssetId testPolicyId (AssetName . serialiseToRawBytes . verificationKeyHash $ vk), 1)
        )
        parties

  headDatum = toScriptData healthyFanoutDatum

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
  deriving (Generic, Show, Enum, Bounded)

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
    ]
 where
  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)
