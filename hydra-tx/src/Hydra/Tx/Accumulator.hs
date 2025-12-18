{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  HydraAccumulator (..),
  getAccumulatorHash,
  getAccumulatorCommitment,
  build,
  buildFromUTxO,
  buildFromSnapshotUTxOs,

  -- * CRS (Common Reference String)
  generateCRS,
  generateCRSG1,
  defaultCRS,
  defaultCRSG1,

  -- * Membership proofs for partial fanout
  createMembershipProof,
  createMembershipProofFromUTxO,
) where

import Hydra.Prelude

import Accumulator (Accumulator, Element)
import Accumulator qualified
import Bindings (getPolyCommitOverG2)
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress, blsGenerator, blsMult)
import Codec.Serialise (serialise)
import Data.Map.Strict qualified as Map
import Field qualified as F
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Tx.IsTx (IsTx (..))
import Plutus.Crypto.BlsUtils (getFinalPoly, getG2Commitment, mkScalar, unScalar)
import PlutusTx.Builtins (
  BuiltinBLS12_381_G2_Element,
  bls12_381_G2_compressed_generator,
  bls12_381_G2_scalarMul,
  bls12_381_G2_uncompress,
  byteStringToInteger,
  toBuiltin,
 )
import PlutusTx.Prelude (scale)

-- * HydraAccumulator

newtype HydraAccumulator = HydraAccumulator {unHydraAccumulator :: Accumulator}
  deriving newtype (Eq, Show)

build :: [ByteString] -> HydraAccumulator
build = HydraAccumulator . Accumulator.buildAccumulator

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
-- > result <- createMembershipProofFromUTxO @Tx subsetUTxO fullAcc defaultCRS
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
  let elements = utxoToElement @tx <$> toPairList @tx utxo
   in build elements

-- | Build an accumulator from snapshot UTxOs, including commit and decommit UTxOs.
--
-- This function combines all UTxOs that could potentially be fanned out:
-- - The main snapshot UTxO
-- - UTxOs to be committed (deposited into the Head)
-- - UTxOs to be decommitted (withdrawn from the Head)
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
  let
    -- Combine all UTxOs that could be fanned out
    -- Note: For Map-based UTxO types, `<>` performs union (left-biased for same keys)
    -- If utxoToCommit and utxoToDecommit overlap (protocol violation), union deduplicates by TxIn
    utxoToCommit = fromMaybe mempty mUtxoToCommit
    utxoToDecommit = fromMaybe mempty mUtxoToDecommit
    combinedUTxO = utxo <> utxoToCommit <> utxoToDecommit
   in
    buildFromUTxO @tx combinedUTxO

-- | Get a simple hash of the accumulator state.
--
-- This is a pure function that returns a deterministic hash of the accumulator's contents.
-- For off-chain snapshots, we just need a commitment to the UTxO set, not a cryptographic proof.
-- This hash is what gets signed by all parties in the multi-signature.
getAccumulatorHash :: HydraAccumulator -> ByteString
getAccumulatorHash (HydraAccumulator acc) =
  -- Simple serialization-based hash of the accumulator map
  toStrict . serialise $ acc

getAccumulatorCommitment :: HydraAccumulator -> BuiltinBLS12_381_G2_Element
getAccumulatorCommitment (HydraAccumulator acc) =
  let g2 = bls12_381_G2_uncompress bls12_381_G2_compressed_generator
      tau = mkScalar 22_435_875_175_126_190_499_447_740_508_185_965_837_690_552_500_527_637_822_603_658_699_938_581_184_511
      k = 1024
      crsG2 = map (\x -> bls12_381_G2_scalarMul (unScalar (scale x tau)) g2) [0 .. k + 10]
   in getG2Commitment crsG2 . getFinalPoly . map (mkScalar . byteStringToInteger BigEndian . toBuiltin . fst) $
        Map.elems
          acc

-- * CRS (Common Reference String)

-- | Generate a CRS using the "powers of tau" approach.
--
-- Based on the approach from haskell-accumulator's test suite:
-- https://github.com/cardano-scaling/haskell-accumulator/blob/main/haskell-accumulator/test/Main.hs
--
-- We only need Point2 for proof generation (getPolyCommitOverG2).
-- The CRS consists of: [g2 * tau^0, g2 * tau^1, ..., g2 * tau^n]
--
-- For testing, we use a fixed tau value. For production, this should be replaced
-- with a secure CRS from a trusted setup ceremony along with the powers of tau.
generateCRS :: Int -> [Point2]
generateCRS setSize =
  let
    -- Define a tau (a large secret value for testing)
    -- In production, this should come from a trusted setup ceremony
    tau = F.Scalar 22_435_875_175_126_190_499_447_740_508_185_965_837_690_552_500_527_637_822_603_658_699_938_581_184_511

    -- Define powers of tau (tau^0, tau^1, ..., tau^setSize over the field)
    powerOfTauField = map (F.powModScalar tau) [0 .. fromIntegral setSize]
    powerOfTauInt = map F.unScalar powerOfTauField

    -- Define the generator of G2
    g2 = blsGenerator :: Point2

    -- Map the power of tau over G2
    crsG2 = map (blsMult g2) powerOfTauInt :: [Point2]
   in
    crsG2

generateCRSG1 :: Int -> [Point1]
generateCRSG1 setSize =
  let
    -- Define a tau (a large secret value for testing)
    -- In production, this should come from a trusted setup ceremony
    tau = F.Scalar 22_435_875_175_126_190_499_447_740_508_185_965_837_690_552_500_527_637_822_603_658_699_938_581_184_511

    -- Define powers of tau (tau^0, tau^1, ..., tau^setSize over the field)
    powerOfTauField = map (F.powModScalar tau) [0 .. fromIntegral setSize]
    powerOfTauInt = map F.unScalar powerOfTauField

    -- Define the generator of G1
    g1 = blsGenerator :: Point1

    -- Map the power of tau over G1
    crsG1 = map (blsMult g1) powerOfTauInt :: [Point1]
   in
    crsG1

-- | Default CRS for testing (supports up to 1000 elements)
--
-- This is a pre-generated CRS using the powers of tau approach.
-- For production, replace with a secure CRS from perpetual powers of tau ceremony.
defaultCRS :: [Point2]
defaultCRS = generateCRS 50

defaultCRSG1 :: [Point1]
defaultCRSG1 = generateCRSG1 50

-- * Cryptographic Proofs for partial fanout

-- | Create a membership proof for a subset of UTxO elements.
--
-- This function uses getPolyCommitOverG2 from haskell-accumulator's Bindings module:
-- https://github.com/cardano-scaling/haskell-accumulator/blob/main/haskell-accumulator/lib/Bindings.hs
--
-- Given a subset of elements and the full accumulator, it:
-- 1. Removes the subset elements from the accumulator
-- 2. Computes a polynomial commitment over G2 for the remaining elements
-- 3. Returns the proof as a hex-encoded string
--
-- For testing, it prints "Success" and the hex-encoded proof.
-- For errors, it prints the error message.
--
-- Example usage:
-- > let fullElements = ["elem1", "elem2", "elem3", "elem4", "elem5"]
-- >     fullAcc = build fullElements
-- >     subset = ["elem2", "elem4"]
-- > result <- createMembershipProof subset fullAcc defaultCRS
-- > -- Prints: "Success: 0x..." or "Error: ..."
createMembershipProof ::
  -- | The subset of elements to prove membership of (e.g., UTxOs being fanned out)
  [Element] ->
  -- | The full accumulator from the confirmed snapshot
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point2] ->
  -- | Returns a string: "Success: 0x..." or "Error: ..."
  ByteString
createMembershipProof subsetElements (HydraAccumulator fullAcc) crs =
  -- Use getPolyCommitOverG2 to generate the proof
  case getPolyCommitOverG2 subsetElements fullAcc crs of
    Left err -> "Error: " <> encodeUtf8 (toText err)
    Right proof ->
      blsCompress proof

-- | Create a membership proof from a UTxO subset.
--
-- This function extracts individual TxOut elements from the subset UTxO and proves
-- they exist in the full accumulator. The full accumulator must be built using
-- `buildFromUTxO` for this to work correctly.
--
-- **How it works:**
-- 1. Each TxOut in the subset is serialized: `Builtins.serialiseData . toBuiltinData`
-- 2. These elements are passed to `getPolyCommitOverG2` which removes them from the accumulator
-- 3. A polynomial commitment proof is generated for the remaining elements
-- 4. The proof is returned as a hex-encoded string
--
-- Example usage for partial fanout:
-- > -- Build full accumulator from ALL UTxOs (do this when creating snapshot)
-- > let fullAcc = buildFromUTxO @Tx fullUTxO
-- >
-- > -- Later, when fanning out a subset
-- > let subsetUTxO = ... -- The 2 UTxOs you want to fan out
-- > result <- createMembershipProofFromUTxO @Tx subsetUTxO fullAcc defaultCRS
-- > -- Returns: "Success: 0xabc123..." (the proof)
--
-- The proof can then be verified on-chain using pairing checks.
createMembershipProofFromUTxO ::
  forall tx.
  IsTx tx =>
  -- | The subset of UTxO to prove membership of (e.g., UTxOs being fanned out)
  UTxOType tx ->
  -- | The full accumulator from the confirmed snapshot (built with buildFromUTxO)
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point2] ->
  -- | Returns a string: "Success: 0x..." or "Error: ..."
  ByteString
createMembershipProofFromUTxO subsetUTxO fullAcc crs = do
  -- Extract individual TxOut elements from the subset
  -- This matches how buildFromUTxO serializes each TxOut
  let subsetElements = utxoToElement @tx <$> toPairList @tx subsetUTxO
  -- Use the element-based proof function
  createMembershipProof subsetElements fullAcc crs
