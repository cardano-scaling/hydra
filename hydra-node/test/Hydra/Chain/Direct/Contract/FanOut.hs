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
import Hydra.Chain.Direct.Tx (fanoutTx, mkHeadOutput, mkHeadTokenScript)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Data.ContestationPeriod (posixFromUTCTime)
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (
  adaOnly,
  genOutput,
  genUTxOWithSimplifiedAddresses,
  genValue,
 )
import Hydra.Ledger.Cardano.Evaluate (slotNoToUTCTime)
import Hydra.Party (partyToChain)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (toBuiltin, toData)
import Test.QuickCheck (elements, oneof, suchThat, vectorOf)
import Test.QuickCheck.Instances ()

healthyFanoutTx :: (Tx, UTxO)
healthyFanoutTx =
  (tx, lookupUTxO)
 where
  tx =
    fanoutTx
      healthyFanoutUTxO
      (headInput, headOutput, headDatum)
      healthySlotNo
      headTokenScript

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

  headDatum = fromPlutusData $ toData healthyFanoutDatum

  lookupUTxO = UTxO.singleton (headInput, headOutput)

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
    }

data FanoutMutation
  = MutateAddUnexpectedOutput
  | MutateChangeOutputValue
  | MutateValidityBeforeDeadline
  deriving (Generic, Show, Enum, Bounded)

genFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genFanoutMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutput
    , SomeMutation MutateChangeOutputValue <$> do
        let outs = txOuts' tx
        -- NOTE: Assumes the fanout transaction has non-empty outputs, which
        -- might not be always the case when testing unbalanced txs and we need
        -- to ensure it by at least one utxo is in healthyFanoutUTxO
        (ix, out) <- elements (zip [0 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation MutateValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- arbitrary `suchThat` slotBeforeContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    ]
 where
  slotBeforeContestationDeadline slotNo =
    slotNoToUTCTime slotNo < healthyContestationDeadline
