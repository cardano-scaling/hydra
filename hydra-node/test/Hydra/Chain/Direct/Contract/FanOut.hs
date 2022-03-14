{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.FanOut where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..))
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.Tx (fanoutTx, mkHeadOutput, mkHeadTokenScript)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Ledger.Cardano (
  adaOnly,
  genOutput,
  genUTxOWithSimplifiedAddresses,
  genValue,
  hashTxOuts,
 )
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (toBuiltin, toData)
import Test.QuickCheck (elements, oneof, suchThat, vectorOf)
import Test.QuickCheck.Instances ()

healthyFanoutTx :: (Tx, UTxO)
healthyFanoutTx =
  (tx, lookupUTxO)
 where
  tx = fanoutTx healthyFanoutUTxO (headInput, headOutput, headDatum) headTokenScript
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

healthyFanoutDatum :: Head.State
healthyFanoutDatum =
  Head.Closed 1 (toBuiltin $ hashTxOuts $ toList healthyFanoutUTxO)

data FanoutMutation
  = MutateAddUnexpectedOutput
  | MutateChangeOutputValue
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
    ]
