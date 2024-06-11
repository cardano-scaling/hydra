{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.FanOut where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens)
import Hydra.Chain.Direct.Fixture (slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (splitUTxO)
import Hydra.Chain.Direct.Tx (fanoutTx, mkHeadOutput)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (
  adaOnly,
  genOutput,
  genUTxOWithSimplifiedAddresses,
  genValue,
 )
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Party (Party, partyToChain, vkey)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import PlutusTx.Builtins (toBuiltin)
import Test.QuickCheck (choose, elements, oneof, suchThat)
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
      (fst healthyFanoutSnapshotUTxO)
      (Just $ snd healthyFanoutSnapshotUTxO)
      (headInput, headOutput)
      healthySlotNo
      headTokenScript

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput' = mkHeadOutput testNetworkId testPolicyId (toUTxOContext $ mkTxOutDatumInline healthyFanoutDatum)

  headOutput = modifyTxOutValue (<> participationTokens) headOutput'

  participationTokens =
    valueFromList $
      map
        ( \party ->
            (AssetId testPolicyId (AssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1)
        )
        healthyParties

healthyFanoutUTxO :: UTxO
healthyFanoutUTxO =
  -- FIXME: fanoutTx would result in 0 outputs and MutateChangeOutputValue below fail
  adaOnly <$> generateWith (genUTxOWithSimplifiedAddresses `suchThat` (not . null)) 42

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime systemStart slotLength $ healthySlotNo - 1

healthyFanoutSnapshotUTxO :: (UTxO, UTxO)
healthyFanoutSnapshotUTxO = splitUTxO healthyFanoutUTxO

healthyFanoutDatum :: Head.State
healthyFanoutDatum =
  Head.Closed
    { snapshotNumber = 1
    , utxoHash = toBuiltin $ hashUTxO @Tx (fst healthyFanoutSnapshotUTxO)
    , utxoToDecommitHash = toBuiltin $ hashUTxO @Tx (snd healthyFanoutSnapshotUTxO)
    , parties =
        partyToChain <$> healthyParties
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , contestationPeriod = healthyContestationPeriod
    , headId = toPlutusCurrencySymbol testPolicyId
    , contesters = []
    }
 where
  healthyContestationPeriodSeconds = 10

  healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]

data FanoutMutation
  = MutateAddUnexpectedOutput
  | MutateChangeOutputValue
  | MutateValidityBeforeDeadline
  | -- | Meant to test that the minting policy is burning all PTs and ST present in tx
    MutateThreadTokenQuantity
  | -- | Alter the fanout redeemer in order to trigger output hash missmatches.
    MutateFanoutRedeemer
  deriving stock (Generic, Show, Enum, Bounded)

genFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genFanoutMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutput
    , SomeMutation (pure $ toErrorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) MutateChangeOutputValue <$> do
        let outs = txOuts' tx
        -- NOTE: Assumes the fanout transaction has non-empty outputs, which
        -- might not be always the case when testing unbalanced txs and we need
        -- to ensure it by at least one utxo is in healthyFanoutUTxO
        (ix, out) <- elements (zip [0 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutateValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) MutateThreadTokenQuantity <$> do
        (token, _) <- elements burntTokens
        changeMintedTokens tx (valueFromList [(token, 1)])
    , SomeMutation (pure $ toErrorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutput
    , SomeMutation (pure $ toErrorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) MutateFanoutRedeemer . ChangeHeadRedeemer <$> do
        let noOfUtxoToOutputs = fromIntegral . size $ toMap (fst healthyFanoutSnapshotUTxO)
        let noOfUtxoDecommitToOutputs = fromIntegral . size $ toMap (snd healthyFanoutSnapshotUTxO)
        n <- elements [1 .. 3]
        pure (Head.Fanout noOfUtxoToOutputs (noOfUtxoDecommitToOutputs - n))
    , SomeMutation (pure $ toErrorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) MutateFanoutRedeemer . ChangeHeadRedeemer <$> do
        let noOfUtxoToOutputs = fromIntegral . size $ toMap (fst healthyFanoutSnapshotUTxO)
        let noOfUtxoDecommitToOutputs = fromIntegral . size $ toMap (snd healthyFanoutSnapshotUTxO)
        n <- elements [1 .. 3]
        pure (Head.Fanout (noOfUtxoToOutputs - n) noOfUtxoDecommitToOutputs)
    ]
 where
  burntTokens =
    case txMintValue $ txBodyContent $ txBody tx of
      TxMintValueNone -> error "expected minted value"
      TxMintValue v _ -> valueToList v

  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)
