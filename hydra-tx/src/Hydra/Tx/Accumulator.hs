{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  HydraAccumulator,
  unHydraAccumulator,
  getAccumulatorHash,
  getAccumulatorCommitment,
  computeG1CommitmentBytes,
  accumulatorSize,
  maxAccumulatorSize,
  deployedFanoutBatchSize,
  build,
  buildFromUTxO,
  buildFromSnapshotUTxOs,
  applyUTxODelta,
  removeOutputs,

  -- * CRS (Common Reference String)
  crsG2Points,
  crsG1Points,
  requiredCRSPointCount,
  defaultItems,

  -- * Membership proofs for partial fanout
  createMembershipProof,
  createMembershipProofFromUTxO,
  createCRSG2Datum,
) where

import Hydra.Prelude hiding (show)

import Accumulator (Accumulator, Element)
import Accumulator qualified
import Bindings (getPolyCommitOverG1)
import Cardano.Api (BabbageEraOnwards (..), TxOutDatum (TxOutDatumInline))
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress)
import Cardano.Crypto.Hash (Blake2b_256)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Data.Map.Strict qualified as Map
import Hydra.Cardano.Api qualified as HApi
import Hydra.Contract.KZGTrustedSetup qualified as KZG
import Hydra.Tx.IsTx (IsTx (..))
import PlutusTx.Builtins (
  BuiltinBLS12_381_G1_Element,
  bls12_381_G1_uncompress,
  bls12_381_G2_uncompress,
  toBuiltin,
 )
import Text.Show (Show (..))

-- * HydraAccumulator

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
  cachedHash = digest (Proxy @Blake2b_256) cachedCommitment

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

-- | Build an accumulator from snapshot UTxOs, including commit and decommit UTxOs.
--
-- Combines all UTxOs that could potentially be fanned out — main snapshot,
-- commit, and decommit — and delegates to 'buildFromUTxO' on the merged set.
-- Merging via UTxO union keeps the same canonical TxIn-sorted element order
-- used by every other accumulator call site ('computeFullFanoutUTxO',
-- 'partialFanout' staleness check, 'emitNextFanoutStep'), so the commitment
-- stored in the snapshot and all downstream proofs are built from the same
-- element set by construction.
--
-- Note: the underlying 'HydraAccumulator' is a 'Map' keyed by element bytes,
-- so insertion order is irrelevant for the commitment value; the merge is done
-- here for explicit consistency with the rest of the fanout code paths.
buildFromSnapshotUTxOs ::
  forall tx.
  IsTx tx =>
  -- | The main snapshot UTxO set
  UTxOType tx ->
  -- | UTxOs to be committed (if any)
  Maybe (UTxOType tx) ->
  -- | UTxOs to be decommitted (if any)
  Maybe (UTxOType tx) ->
  -- | The resulting accumulator containing all UTxOs
  HydraAccumulator
buildFromSnapshotUTxOs utxo mUtxoToCommit mUtxoToDecommit =
  buildFromUTxO @tx $
    utxo
      <> fromMaybe mempty mUtxoToCommit
      <> fromMaybe mempty mUtxoToDecommit

-- | Update an accumulator from one snapshot's combined UTxO set to the next
-- by adding and removing only the changed outputs, avoiding the per-output
-- serialization and hashing of a full rebuild. Extensionally equal to
-- 'buildFromUTxO' on the new set (see the property in
-- "Hydra.Tx.AccumulatorSpec"): the underlying map tracks element multiplicity
-- and the TxIn-keyed set difference removes exactly one occurrence per
-- consumed input. Falls back to a full rebuild if a removed element is
-- missing or has lower multiplicity than the removals require, which would
-- indicate the given accumulator was not built from the given previous UTxO
-- set.
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
  | removalsCovered =
      mkHydraAccumulator $
        foldl' (flip Accumulator.removeElement) (foldl' Accumulator.addElement prev addedEls) removedEls
  | otherwise = buildFromUTxO @tx nextUTxO
 where
  prev = unHydraAccumulator prevAcc

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

-- | Largest subset a single fanout transaction can distribute, re-exported from
-- 'KZGTrustedSetup' for convenience.
deployedFanoutBatchSize :: Int
deployedFanoutBatchSize = KZG.deployedFanoutBatchSize

-- | Convert a 'KZG.KZGSetupError' 'Either' to the contained value, aborting
-- with a descriptive message if the setup is invalid.
--
-- This should never be reached in a correctly built binary: the trusted setup
-- bytes are embedded at compile time, integrity-checked via SHA-256, and
-- exercised by the test suite. A failure here would indicate binary tampering
-- or a corrupted build artefact.
fromKZGSetup :: Either KZG.KZGSetupError a -> a
fromKZGSetup = either (\e -> error . toText $ "KZG trusted setup invariant violated: " <> show e) id

getAccumulatorCommitment :: HydraAccumulator -> BuiltinBLS12_381_G1_Element
getAccumulatorCommitment = bls12_381_G1_uncompress . toBuiltin . _cachedCommitment

-- | Compute the compressed G1 commitment for an accumulator through the
-- rust-accumulator FFI (divide-and-conquer FFT polynomial expansion and a
-- Pippenger multi-scalar multiplication), which is orders of magnitude
-- faster than expanding the polynomial in Haskell. Bit-for-bit equal to the
-- PlutusTx reference path; see the equivalence properties and golden values
-- in "Hydra.Tx.AccumulatorSpec".
computeG1CommitmentBytes :: Accumulator -> ByteString
computeG1CommitmentBytes acc
  | n > KZG.maxAccumulatorSize =
      error . toText $ "getAccumulatorCommitment: accumulator has " <> show n <> " elements, exceeding the G1 CRS limit of " <> show KZG.maxAccumulatorSize
  | otherwise =
      either (\e -> error $ "computeG1CommitmentBytes: " <> toText e) blsCompress $
        getPolyCommitOverG1 [] acc (crsG1Points (n + 1))
 where
  n = sum (snd <$> Map.elems acc)

-- * CRS (Common Reference String)

-- | Returns the first @n@ G1 powers of tau from the EIP-4844 trusted setup.
-- Used as the off-chain CRS for building accumulator commitments and membership proofs:
-- @[G1, τ·G1, ..., τ^(n-1)·G1]@.
crsG1Points :: Int -> [Point1]
crsG1Points n = take n $ fromKZGSetup KZG.g1Points

-- | Returns the first @n@ G2 powers of tau from the EIP-4844 trusted setup.
-- Used as the on-chain CRS for verifying membership proofs:
-- @[G2, τ·G2, ..., τ^(n-1)·G2]@.
crsG2Points :: Int -> [Point2]
crsG2Points n = take n $ fromKZGSetup KZG.g2Points

-- | Number of G2 points published in the on-chain CRS UTxO datum.
--
-- This is the __deployed__ G2 CRS length. It directly caps the largest
-- subset that can be verified in a single fanout / partial-fanout pairing
-- check: a subset of N elements yields a polynomial of degree N (one
-- @(X - sᵢ)@ factor per element), and the on-chain MSM to evaluate
-- @P_S(τ)·G2@ needs N+1 G2 points, so the deployed batch limit is
-- 'deployedFanoutBatchSize'.
--
-- The trusted-setup file embeds 65 G2 points (see
-- 'KZGTrustedSetup.maxFanoutBatchSize'); only the first 'defaultItems'
-- are written into the CRS UTxO at script-registry publication time
-- (see 'Hydra.Chain.ScriptRegistry.buildScriptPublishingTxs'). The head
-- validator is compiled against the hash of that CRS datum and rejects
-- any other one, so raising 'defaultItems' means re-publishing the CRS
-- UTxO /and/ recompiling the scripts. It is bounded above by
-- @KZGTrustedSetup.maxFanoutBatchSize + 1@.
defaultItems :: Int
defaultItems = KZG.defaultItems

-- | Returns the number of G1 CRS points required for this accumulator.
-- An n-element accumulator polynomial has degree n, so needs n+1 G1 points
-- @[G1, τ·G1, ..., τⁿ·G1]@ to compute the commitment A(τ)·G1 and proofs.
-- n is the total element count including duplicates (sum of all counts).
requiredCRSPointCount :: HydraAccumulator -> Int
requiredCRSPointCount ha = sum (map snd $ Map.elems $ unHydraAccumulator ha) + 1

-- * Cryptographic Proofs for partial fanout

-- | Create a membership proof for a subset of UTxO elements.
--
-- This function uses getPolyCommitOverG1 from haskell-accumulator's Bindings module:
-- https://github.com/cardano-scaling/haskell-accumulator/blob/main/haskell-accumulator/lib/Bindings.hs
--
-- Given a subset of elements and the full accumulator, it:
-- 1. Removes the subset elements from the accumulator
-- 2. Computes a polynomial commitment over G1 for the remaining elements
-- 3. Returns the proof as a compressed G1 point
createMembershipProof ::
  -- | The subset of elements to prove membership of (e.g., UTxOs being fanned out)
  [Element] ->
  -- | The full accumulator from the confirmed snapshot
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point1] ->
  -- | Returns the compressed proof point, or an error if elements are missing or CRS is too short
  Either Text ByteString
createMembershipProof subsetElements fullAcc crs =
  bimap toText blsCompress $ getPolyCommitOverG1 subsetElements (unHydraAccumulator fullAcc) crs

-- | Create a membership proof from a UTxO subset.
--
-- This function extracts individual TxOut elements from the subset UTxO and proves
-- they exist in the full accumulator. The full accumulator must be built using
-- `buildFromUTxO` for this to work correctly.
--
-- The proof is verified on-chain via e(commitment_G1, G2) = e(proof_G1, P_S(τ)·G2).
createMembershipProofFromUTxO ::
  forall tx.
  IsTx tx =>
  -- | The subset of UTxO to prove membership of (e.g., UTxOs being fanned out)
  UTxOType tx ->
  -- | The full accumulator from the confirmed snapshot (built with buildFromUTxO)
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point1] ->
  -- | Returns the compressed proof point, or an error if elements are missing or CRS is too short
  Either Text ByteString
createMembershipProofFromUTxO subsetUTxO fullAcc crs =
  -- Extract individual TxOut elements from the subset (each TxOut -> hash).
  -- This matches how buildFromUTxO / buildFromSnapshotUTxOs serialize each TxOut.
  -- The underlying accumulator tracks element multiplicity (via Count), so duplicate
  -- elements are handled correctly.
  -- Drop mempty: TxOuts for which toPlutusTxOut returns Nothing yield mempty here;
  -- we only prove membership of outputs that convert successfully.
  let subsetElements = filter (/= mempty) $ utxoToElement @tx <$> outputsOfUTxO @tx subsetUTxO
   in -- Use the element-based proof function
      createMembershipProof subsetElements fullAcc crs

createCRSG2Datum :: Int -> HApi.TxOutDatum ctx
createCRSG2Datum n =
  TxOutDatumInline BabbageEraOnwardsConway $
    HApi.toScriptData
      (bls12_381_G2_uncompress . toBuiltin . blsCompress <$> crsG2Points n)
