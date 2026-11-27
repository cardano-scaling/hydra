{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator.Core where

import Accumulator (Accumulator)
import Accumulator qualified
import Data.Map.Strict qualified as Map
import Hydra.Contract.KZGTrustedSetup qualified as KZG
import Hydra.Prelude hiding (show)
import Hydra.Tx.Accumulator.Commitment qualified as Commitment
import Hydra.Tx.IsTx (IsTx (..), combinedUTxO, restrictedTo)
import Text.Show (Show (..))

data HydraAccumulator = HydraAccumulator
  { unHydraAccumulator :: Accumulator
  , _cachedCommitment :: ByteString
  -- ^ Lazy thunk: compressed G1 commitment. Forced at most once per value,
  -- shared by the hash below and by 'getAccumulatorCommitment' (datum
  -- construction at close/contest/fanout).
  , _cachedHash :: ByteString
  -- ^ Lazy thunk: blake2b-256 of '_cachedCommitment'. Forced once on first
  -- access; subsequent reads return the memoized value, avoiding repeated
  -- BLS12-381 multi-scalar multiplications for the same accumulator.
  }

-- | Shows only the element map: the derived instance would force the cached
-- commitment, so showing head state in logs or test output would compute a
-- BLS commitment as a side effect.
instance Show HydraAccumulator where
  show HydraAccumulator{unHydraAccumulator = acc} =
    "HydraAccumulator " <> show acc

instance Eq HydraAccumulator where
  a == b = unHydraAccumulator a == unHydraAccumulator b

mkHydraAccumulator :: Accumulator -> HydraAccumulator
mkHydraAccumulator acc = HydraAccumulator acc cachedCommitment cachedHash
 where
  cachedCommitment = computeG1CommitmentBytes acc
  cachedHash = Commitment.hashCommitment cachedCommitment

build :: [ByteString] -> HydraAccumulator
build = mkHydraAccumulator . Accumulator.buildAccumulator

-- | Build an accumulator from a UTxO by serializing each individual TxOut.
--
-- This is the CORRECT way to build an accumulator for partial fanout proofs.
-- Each TxOut becomes a separate element in the accumulator, allowing you to later
-- prove that a subset of TxOuts was part of the original set.
--
-- The serialization matches how `hashTxOuts` works on-chain:
-- Each element = Builtins.serialiseData (toBuiltinData plutusTxOut)
--
-- Example usage:
-- > -- Build accumulator from the full UTxO set
-- > let fullAcc = buildFromUTxO @Tx utxo
-- >
-- > -- Later, prove a subset exists
-- > let crs = crsG1Points (requiredCRSPointCount fullAcc)
-- > result <- createMembershipProofFromUTxO @Tx subsetUTxO fullAcc crs
--
-- This approach allows proving that 2 out of 5 UTxOs are part of the original set,
-- which is essential for partial fanout functionality.
buildFromUTxO ::
  forall tx.
  IsTx tx =>
  -- | The UTxO set to build the accumulator from
  UTxOType tx ->
  -- | The resulting accumulator containing one element per TxOut
  HydraAccumulator
buildFromUTxO utxo =
  let elements = utxoToElement @tx <$> outputsOfUTxO @tx utxo
   in build elements

-- | Build the two accumulators a snapshot commits to, see 'Hydra.Tx.Snapshot'.
--
-- When a snapshot is signed nobody knows yet whether its pending increment or
-- decrement will land on chain before the head is closed, and which outputs the
-- head owes at close time depends on that. So the parties sign a commitment for
-- each of the two futures:
--
--   * the /snapshot/ accumulator, over the head's content while it stays at the
--     snapshot's version: the snapshot UTxO plus a pending decommit (still inside
--     until the decrement lands); a pending commit is not yet inside.
--   * the /applied/ accumulator, over the head's content once the pending action
--     has been applied on chain: the snapshot UTxO plus a pending commit (the
--     increment absorbed it); a pending decommit has been paid out.
--
-- Close and Contest store the one matching their redeemer kind (Unused/Any and
-- Used respectively, 'Hydra.Contract.Head.checkClose'), so the on-chain
-- commitment always describes exactly the outputs a fanout must distribute.
-- With nothing pending both are the same value and are computed once.
--
-- Merging via UTxO union keeps the same canonical TxIn-sorted element order
-- used by every other accumulator call site, so the commitments stored in the
-- snapshot and all downstream proofs are built from the same element sets by
-- construction (the underlying 'HydraAccumulator' is a 'Map' keyed by element
-- bytes, so insertion order does not affect the commitment value).
buildFromSnapshotUTxOs ::
  forall tx.
  IsTx tx =>
  -- | The main snapshot UTxO set
  UTxOType tx ->
  -- | UTxOs to be committed (if any)
  Maybe (UTxOType tx) ->
  -- | UTxOs to be decommitted (if any)
  Maybe (UTxOType tx) ->
  -- | The snapshot accumulator and the applied accumulator
  (HydraAccumulator, HydraAccumulator)
buildFromSnapshotUTxOs utxo mUtxoToCommit mUtxoToDecommit =
  case (mUtxoToCommit, mUtxoToDecommit) of
    (Nothing, Nothing) -> (base, base)
    _ ->
      ( buildFromUTxO @tx $ combinedUTxO utxo Nothing mUtxoToDecommit
      , buildFromUTxO @tx $ combinedUTxO utxo mUtxoToCommit Nothing
      )
 where
  base = buildFromUTxO @tx utxo

-- | Update an accumulator from one snapshot's combined UTxO set to the next
-- by adding and removing only the changed outputs, avoiding the per-output
-- serialization and hashing of a full rebuild. Extensionally equal to
-- 'buildFromUTxO' on the new set (see the property in
-- "Hydra.Tx.AccumulatorSpec"): the underlying map tracks element multiplicity
-- and the TxIn-keyed set difference removes exactly one occurrence per
-- consumed input. Falls back to a full rebuild if a removed element is
-- missing or has lower multiplicity than the removals require, which would
-- indicate the given accumulator was not built from the given previous UTxO
-- set, or if a 'TxIn' present on both sides carries a different output, which
-- the TxIn-keyed differences cannot see.
applyUTxODelta ::
  forall tx.
  IsTx tx =>
  -- | Accumulator built from the previous combined UTxO set
  HydraAccumulator ->
  -- | The previous combined UTxO set
  UTxOType tx ->
  -- | The new combined UTxO set
  UTxOType tx ->
  HydraAccumulator
applyUTxODelta prevAcc prevUTxO nextUTxO
  | noOutputChangedUnderSameTxIn && removalsCovered =
      mkHydraAccumulator $
        foldl' (flip Accumulator.removeElement) (foldl' Accumulator.addElement prev addedEls) removedEls
  | otherwise = buildFromUTxO @tx nextUTxO
 where
  prev = unHydraAccumulator prevAcc

  -- Every 'TxIn' present on both sides still carries the same output. The
  -- TxIn-keyed differences below cannot detect a change of output, so the
  -- delta would leave the accumulator describing the old output.
  noOutputChangedUnderSameTxIn =
    restrictedTo @tx prevUTxO nextUTxO == restrictedTo @tx nextUTxO prevUTxO

  -- Multiset containment: every element must be present with at least the
  -- multiplicity about to be removed. Membership alone would let a
  -- mismatched (accumulator, previous UTxO) pair skip the fallback:
  -- 'Accumulator.removeElement' silently no-ops once a count is exhausted,
  -- yielding a commitment that does not bind 'nextUTxO'.
  removalsCovered =
    Map.isSubmapOfBy (\needed (_, held) -> needed <= held) removedCounts prev

  removedCounts = Map.fromListWith (+) [(el, 1 :: Int) | el <- removedEls]

  removedEls = utxoToElement @tx <$> outputsOfUTxO @tx (prevUTxO `withoutUTxO` nextUTxO)

  addedEls = utxoToElement @tx <$> outputsOfUTxO @tx (nextUTxO `withoutUTxO` prevUTxO)

-- | Remove one occurrence of each given output from an accumulator.
--
-- Unlike 'applyUTxODelta' the outputs to remove are given directly, so the
-- result does not depend on which 'TxIn' holds them. That matters wherever the
-- two sets are related by content rather than by 'TxIn': partial fanout accepts
-- a user selection as a sub-multiset of outputs (see
-- 'Hydra.HeadLogic.isSubMultisetOf'), so the same 'TxIn' can carry a different
-- 'TxOut' on each side and a 'TxIn'-keyed difference would then remove the
-- wrong element, or none at all.
--
-- Removing exactly the distributed outputs is also what makes the on-chain
-- split identity @A = P_K * A'@ hold by construction, for the @A@ that was
-- verified against the head datum.
--
-- 'Accumulator.removeElement' no-ops once an element's count is exhausted, so
-- an output that is not in the accumulator leaves a commitment that does not
-- bind the intended set; the on-chain identity then fails and the transaction
-- is rejected, exactly as a fresh build over an inconsistent set would be.
removeOutputs ::
  forall tx.
  IsTx tx =>
  -- | Accumulator to remove from
  HydraAccumulator ->
  -- | Outputs to remove, one occurrence each
  UTxOType tx ->
  HydraAccumulator
removeOutputs acc utxo =
  mkHydraAccumulator $
    foldl' (flip Accumulator.removeElement) (unHydraAccumulator acc) removedEls
 where
  removedEls = utxoToElement @tx <$> outputsOfUTxO @tx utxo

-- | Get a blake2b-256 hash of the accumulator commitment (compressed G1 point).
--
-- This is a pure function that returns a 32-byte deterministic hash of the
-- compressed G1 accumulator commitment. It is what gets signed by all parties
-- in the multi-signature and stored as 'accumulatorHash' in on-chain datums.
--
-- Hashing the compressed G1 point (rather than the serialized map) binds the
-- signed hash to the exact G1 point stored in 'ClosedDatum.accumulatorCommitment',
-- allowing the on-chain validator to verify their consistency.
--
-- The result is cached inside 'HydraAccumulator' as a lazy thunk and computed
-- at most once per value, regardless of how many times this function is called.
getAccumulatorHash :: HydraAccumulator -> ByteString
getAccumulatorHash = _cachedHash

-- | Number of UTxOs tracked by the accumulator.
accumulatorSize :: HydraAccumulator -> Int
accumulatorSize = sum . map snd . Map.elems . unHydraAccumulator

-- | Maximum accumulator size, re-exported from 'KZGTrustedSetup' for convenience.
maxAccumulatorSize :: Int
maxAccumulatorSize = KZG.maxAccumulatorSize

-- | An accumulator over more elements than the embedded G1 CRS can commit to.
data AccumulatorTooLarge = AccumulatorTooLarge {utxoCount :: Int, maxAllowed :: Int}
  deriving stock (Eq, Show)

-- | Largest subset a single fanout transaction can distribute, re-exported from
-- 'KZGTrustedSetup' for convenience.
deployedFanoutBatchSize :: Int
deployedFanoutBatchSize = KZG.deployedFanoutBatchSize

computeG1CommitmentBytes :: Accumulator -> ByteString
computeG1CommitmentBytes acc
  | n > KZG.maxAccumulatorSize =
      error . toText $ "getAccumulatorCommitment: accumulator has " <> show n <> " elements, exceeding the G1 CRS limit of " <> show KZG.maxAccumulatorSize
  | otherwise =
      either (\e -> error $ "computeG1CommitmentBytes: " <> e) id $
        Commitment.commit (n + 1) acc
 where
  n = sum (snd <$> Map.elems acc)

defaultItems :: Int
defaultItems = KZG.defaultItems

requiredCRSPointCount :: HydraAccumulator -> Int
requiredCRSPointCount acc = accumulatorSize acc + 1
