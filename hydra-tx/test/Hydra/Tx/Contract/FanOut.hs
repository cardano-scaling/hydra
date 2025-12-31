{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.FanOut where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import GHC.IsList (IsList (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Fanout (fanoutTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO))
import Hydra.Tx.Party (Party, partyToChain, vkey)
import Hydra.Tx.Utils (adaOnly, splitUTxO)
import PlutusTx.Builtins (bls12_381_G2_uncompress, toBuiltin)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genOutputFor, genScriptRegistry, genUTxOWithSimplifiedAddresses, genValue)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens)
import Test.QuickCheck (choose, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyFanoutTx :: (Tx, UTxO)
healthyFanoutTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    fanoutTx
      scriptRegistry
      (fst healthyFanoutSnapshotUTxO)
      Nothing
      (Just $ snd healthyFanoutSnapshotUTxO)
      (headInput, headOutput)
      healthySlotNo
      headTokenScript

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput' :: TxOut CtxUTxO
  headOutput' = mkHeadOutput testNetworkId testPolicyId (mkTxOutDatumInline healthyFanoutDatum)

  headOutput = modifyTxOutValue (<> participationTokens) headOutput'

  participationTokens =
    fromList $
      map
        ( \party ->
            (AssetId testPolicyId (UnsafeAssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1)
        )
        healthyParties

healthyFanoutUTxO :: UTxO
healthyFanoutUTxO =
  UTxO.map adaOnly $ generateWith (genUTxOWithSimplifiedAddresses `suchThat` \u -> UTxO.size u > 1) 42

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime systemStart slotLength $ healthySlotNo - 1

healthyFanoutSnapshotUTxO :: (UTxO, UTxO)
healthyFanoutSnapshotUTxO = splitUTxO healthyFanoutUTxO

accumulator :: Accumulator.HydraAccumulator
accumulator = Accumulator.buildFromUTxO @Tx (uncurry (<>) healthyFanoutSnapshotUTxO)

healthyFanoutDatum :: Head.State
healthyFanoutDatum =
  Head.Closed
    Head.ClosedDatum
      { snapshotNumber = 1
      , utxoHash = toBuiltin $ hashUTxO @Tx (fst healthyFanoutSnapshotUTxO)
      , alphaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
      , omegaUTxOHash = toBuiltin $ hashUTxO @Tx (snd healthyFanoutSnapshotUTxO)
      , parties =
          partyToChain <$> healthyParties
      , contestationDeadline = posixFromUTCTime healthyContestationDeadline
      , contestationPeriod = healthyContestationPeriod
      , headId = toPlutusCurrencySymbol testPolicyId
      , contesters = []
      , version = 0
      , accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash accumulator
      , proof =
          let snapshotUTxO =
                uncurry (<>) healthyFanoutSnapshotUTxO
           in bls12_381_G2_uncompress $
                toBuiltin $
                  Accumulator.createMembershipProofFromUTxO @Tx snapshotUTxO accumulator (Accumulator.generateCRS $ UTxO.size snapshotUTxO + 1)
      , accumulatorCommitment =
          Accumulator.getAccumulatorCommitment $
            Accumulator.buildFromSnapshotUTxOs (fst healthyFanoutSnapshotUTxO) mempty (Just $ snd healthyFanoutSnapshotUTxO)
      }
 where
  healthyContestationPeriodSeconds = 10

  healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]

data FanoutMutation
  = MutateValidityBeforeDeadline
  | -- | Meant to test that the minting policy is burning all PTs and ST present in tx
    MutateThreadTokenQuantity
  | MutateAddUnexpectedOutput
  | MutateFanoutOutputValue
  | MutateDecommitOutputValue
  deriving stock (Generic, Show, Enum, Bounded)

genFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genFanoutMutation (tx, _utxo) =
  oneof
    [ -- Spec: Transaction is posted after contestation deadline tmin > tfinal .
      SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutateValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , -- Spec: All tokens are burnt |{cid 7→ · 7→ −1} ∈ mint| = m′ + 1.
      SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) MutateThreadTokenQuantity <$> do
        (token, _) <- elements burntTokens
        changeMintedTokens tx (fromList [(token, 1)])
    , -- Spec: The first m outputs are distributing funds according to η. That is, the outputs exactly
      -- correspond to the UTxO canonically combined U
      SomeMutation (pure $ toErrorCode FanoutUTxOHashMismatch) MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutputFor
    , -- Spec: The following n outputs are distributing funds according to η∆ .
      -- That is, the outputs exactly # correspond to the UTxO canonically combined U∆
      SomeMutation (pure $ toErrorCode FanoutUTxOHashMismatch) MutateFanoutOutputValue <$> do
        let outs = txOuts' tx
        let noOfUtxoToOutputs = size $ UTxO.toMap (fst healthyFanoutSnapshotUTxO)
        (ix, out) <- elements (zip [0 .. noOfUtxoToOutputs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , -- Spec: The following n outputs are distributing funds according to η∆.
      -- That is, the outputs exactly # correspond to the UTxO canonically combined U∆
      SomeMutation (pure $ toErrorCode FanoutUTxOToDecommitHashMismatch) MutateDecommitOutputValue <$> do
        let outs = txOuts' tx
        let noOfUtxoToOutputs = size $ UTxO.toMap (fst healthyFanoutSnapshotUTxO)
        (ix, out) <- elements (zip [noOfUtxoToOutputs .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    ]
 where
  burntTokens =
    case toList . txMintValueToValue . txMintValue $ getTxBodyContent $ txBody tx of
      [] -> error "expected minted value"
      v -> v

  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)
