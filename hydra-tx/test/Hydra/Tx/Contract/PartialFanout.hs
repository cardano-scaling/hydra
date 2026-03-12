{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.PartialFanout where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point2)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (LowerBoundBeforeContestationDeadline, PartialFanoutMembershipFailed))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Fanout (partialFanoutTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO, utxoToElement))
import Hydra.Tx.Party (Party, partyToChain, vkey)
import Hydra.Tx.Utils (adaOnly)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusTx.Builtins (bls12_381_G2_uncompress)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genScriptRegistryWithCRSSize, genUTxOWithSimplifiedAddresses, genValue)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens)
import Test.QuickCheck (choose, elements, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()

-- | A healthy partial fanout transaction that distributes a subset of UTxOs
-- and continues the Closed state with an updated accumulator.
healthyPartialFanoutTx :: (Tx, UTxO)
healthyPartialFanoutTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    partialFanoutTx
      scriptRegistry
      healthyDistributeUTxO
      (headInput, headOutput)
      healthySlotNo
      healthyClosedDatum
      remainingAccumulator

  scriptRegistry = genScriptRegistryWithCRSSize crsSize `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput' :: TxOut CtxUTxO
  headOutput' = mkHeadOutput testNetworkId testPolicyId (mkTxOutDatumInline healthyPartialFanoutState)

  headOutput = modifyTxOutValue (<> participationTokens) headOutput'

  participationTokens =
    fromList $
      map
        ( \party ->
            (AssetId testPolicyId (UnsafeAssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1)
        )
        healthyParties

-- | The full UTxO to be distributed, matching the E2E test scenario with
-- 20 UTxOs to exercise partial fanout (threshold is 19).
healthyFullUTxO :: UTxO
healthyFullUTxO =
  let utxo = UTxO.map adaOnly $ generateWith (resize 100 genUTxOWithSimplifiedAddresses) 42
      utxoList = UTxO.toList utxo
   in UTxO.fromList $ take 20 utxoList

-- | Split the full UTxO: distribute the first 15, keep the remaining 5.
-- This matches the E2E fanoutChunkSize of 15.
healthyDistributeUTxO :: UTxO
healthyDistributeUTxO =
  UTxO.fromList $ take 15 $ UTxO.toList healthyFullUTxO

healthyRemainingUTxO :: UTxO
healthyRemainingUTxO =
  UTxO.fromList $ drop 15 $ UTxO.toList healthyFullUTxO

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime systemStart slotLength $ healthySlotNo - 1

-- | The full accumulator covers all UTxOs that were originally in the snapshot.
fullAccumulator :: Accumulator.HydraAccumulator
fullAccumulator =
  Accumulator.buildFromSnapshotUTxOs
    healthyFullUTxO
    Nothing
    Nothing

-- | After distributing healthyDistributeUTxO, the remaining accumulator covers
-- only the remaining UTxOs.
remainingAccumulator :: Accumulator.HydraAccumulator
remainingAccumulator =
  Accumulator.buildFromUTxO @Tx healthyRemainingUTxO

crsSize :: Int
crsSize = Accumulator.requiredCRSSize fullAccumulator + 1

crs :: [Point2]
crs = Accumulator.generateCRS crsSize

-- | The ClosedDatum that the head output currently carries (before partial fanout).
-- This represents a freshly-closed head with the full accumulator.
healthyClosedDatum :: Head.ClosedDatum
healthyClosedDatum =
  Head.ClosedDatum
    { snapshotNumber = 1
    , utxoHash = toBuiltin $ hashUTxO @Tx healthyFullUTxO
    , alphaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , omegaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , parties = partyToChain <$> healthyParties
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , contestationPeriod = healthyContestationPeriod
    , headId = toPlutusCurrencySymbol testPolicyId
    , contesters = []
    , version = 0
    , accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash fullAccumulator
    , proof =
        let allOutputs = UTxO.txOutputs healthyFullUTxO
            subsetElements = utxoToElement @Tx <$> allOutputs
         in bls12_381_G2_uncompress $
              toBuiltin $
                Accumulator.createMembershipProof subsetElements fullAccumulator crs
    , accumulatorCommitment =
        Accumulator.getAccumulatorCommitment fullAccumulator
    }
 where
  healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime 10

-- | The Head.State wrapping the ClosedDatum for the datum on the head output.
healthyPartialFanoutState :: Head.State
healthyPartialFanoutState = Head.Closed healthyClosedDatum

healthyParties :: [Party]
healthyParties =
  [generateWith arbitrary i | i <- [1 .. 3]]

data PartialFanoutMutation
  = MutatePartialFanoutValidityBeforeDeadline
  | -- | Partial fanout must NOT burn tokens (unlike full fanout)
    MutatePartialFanoutBurnTokens
  | MutatePartialFanoutOutputValue
  deriving stock (Generic, Show, Enum, Bounded)

genPartialFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genPartialFanoutMutation (tx, _utxo) =
  oneof
    [ -- Partial fanout must also respect the contestation deadline
      SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutatePartialFanoutValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , -- Partial fanout must NOT burn any tokens (tokens are kept for subsequent fanouts).
      -- mustNotMintOrBurn uses error code "U01"
      SomeMutation (pure "U01") MutatePartialFanoutBurnTokens <$> do
        let headTokenScript = mkHeadTokenScript testSeedInput
        changeMintedTokens tx (fromList [(AssetId (scriptPolicyId (PlutusScript headTokenScript)) (UnsafeAssetName ""), -1)])
    , -- Mutating a distributed output value should fail accumulator membership verification
      SomeMutation (pure $ toErrorCode PartialFanoutMembershipFailed) MutatePartialFanoutOutputValue <$> do
        -- The distributed outputs start at index 1 (index 0 is the continuing head output)
        let outs = txOuts' tx
        let numDistributed = UTxO.size healthyDistributeUTxO
        -- Pick one of the distributed outputs (indices 1..numDistributed)
        (ix, out) <- elements (zip [1 .. numDistributed] (drop 1 outs))
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    ]
 where
  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)
