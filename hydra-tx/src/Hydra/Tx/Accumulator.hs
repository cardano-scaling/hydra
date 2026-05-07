{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  HydraAccumulator (..),
  getAccumulatorHash,
  getAccumulatorCommitment,
  accumulatorSize,
  maxAccumulatorSize,
  build,
  buildFromUTxO,
  buildFromSnapshotUTxOs,

  -- * CRS (Common Reference String)
  crsG2Points,
  crsG1Points,
  requiredCRSPointCount,
  defaultItems,

  -- * Membership proofs for partial fanout
  createMembershipProof,
  createMembershipProofFromUTxO,
  createCRSG1Datum,
) where

import Hydra.Prelude

import Accumulator (Accumulator, Element)
import Accumulator qualified
import Bindings (getPolyCommitOverG2)
import Cardano.Api (BabbageEraOnwards (..), TxOutDatum (TxOutDatumInline))
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress)
import Cardano.Crypto.Hash (Blake2b_256)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Codec.Serialise (deserialiseOrFail, serialise)
import Data.Aeson (Value (String), withText)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.List (nub)
import Data.Map.Strict qualified as Map
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Cardano.Api qualified as HApi
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.KZGTrustedSetup qualified as KZG
import Plutus.Crypto.BlsUtils (getFinalPoly, getG2Commitment, mkScalar)
import PlutusTx.Builtins (
  BuiltinBLS12_381_G2_Element,
  bls12_381_G1_uncompress,
  bls12_381_G2_compress,
  byteStringToInteger,
  fromBuiltin,
  toBuiltin,
 )

-- * HydraAccumulator

newtype HydraAccumulator = HydraAccumulator {unHydraAccumulator :: Accumulator}
  deriving newtype (Eq, Show)

instance ToJSON HydraAccumulator where
  toJSON (HydraAccumulator acc) =
    String $ decodeUtf8 $ Base16.encode $ BSL.toStrict $ serialise acc

instance FromJSON HydraAccumulator where
  parseJSON = withText "HydraAccumulator" $ \t ->
    case Base16.decode (encodeUtf8 t) of
      Left err -> fail $ "Invalid base16 for HydraAccumulator: " <> show err
      Right bs ->
        case deserialiseOrFail (BSL.fromStrict bs) of
          Left err -> fail $ "Failed to deserialize HydraAccumulator: " <> show err
          Right acc -> pure $ HydraAccumulator acc

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

-- | Get a blake2b-256 hash of the accumulator commitment (compressed G2 point).
--
-- This is a pure function that returns a 32-byte deterministic hash of the
-- compressed G2 accumulator commitment. It is what gets signed by all parties
-- in the multi-signature and stored as 'accumulatorHash' in on-chain datums.
--
-- Hashing the compressed G2 point (rather than the serialized map) binds the
-- signed hash to the exact G2 point stored in 'ClosedDatum.accumulatorCommitment',
-- allowing the on-chain validator to verify their consistency.
getAccumulatorHash :: HydraAccumulator -> ByteString
getAccumulatorHash acc =
  digest (Proxy @Blake2b_256) . fromBuiltin . bls12_381_G2_compress $ getAccumulatorCommitment acc

-- | Number of elements in the accumulator — equals the number of UTxOs in the snapshot.
accumulatorSize :: HydraAccumulator -> Int
accumulatorSize (HydraAccumulator acc) = Map.size acc

-- | Maximum accumulator size, re-exported from 'KZGTrustedSetup' for convenience.
maxAccumulatorSize :: Int
maxAccumulatorSize = KZG.maxAccumulatorSize

getAccumulatorCommitment :: HydraAccumulator -> BuiltinBLS12_381_G2_Element
getAccumulatorCommitment (HydraAccumulator acc) =
  let n = Map.size acc
   in if n > KZG.maxAccumulatorSize
        then error $ "getAccumulatorCommitment: accumulator has " <> show n <> " elements, exceeding the G2 CRS limit of " <> show KZG.maxAccumulatorSize
        else
          let crsG2 = take (n + 1) KZG.g2BuiltinPoints
           in getG2Commitment crsG2 . getFinalPoly . map (mkScalar . byteStringToInteger BigEndian . toBuiltin . fst) $
                Map.elems acc

-- * CRS (Common Reference String)

-- | Returns the first @n@ G2 powers of tau from the EIP-4844 trusted setup.
-- Used as the off-chain CRS for building accumulator commitments and membership proofs:
-- @[G2, τ·G2, ..., τ^(n-1)·G2]@.
crsG2Points :: Int -> [Point2]
crsG2Points n = take n KZG.g2Points

-- | Returns the first @n@ G1 powers of tau from the EIP-4844 trusted setup.
-- Used as the on-chain CRS for verifying membership proofs:
-- @[G1, τ·G1, ..., τ^(n-1)·G1]@.
crsG1Points :: Int -> [Point1]
crsG1Points n = take n KZG.g1Points

defaultItems :: Int
defaultItems = 30

-- | Returns the number of G2 CRS points required for this accumulator.
-- An n-element accumulator polynomial has degree n, so needs n+1 G2 points
-- @[G2, τ·G2, ..., τⁿ·G2]@ to compute the commitment A(τ)·G2 and proofs.
requiredCRSPointCount :: HydraAccumulator -> Int
requiredCRSPointCount (HydraAccumulator acc) = Map.size acc + 1

-- * Cryptographic Proofs for partial fanout

-- | Create a membership proof for a subset of UTxO elements.
--
-- This function uses getPolyCommitOverG2 from haskell-accumulator's Bindings module:
-- https://github.com/cardano-scaling/haskell-accumulator/blob/main/haskell-accumulator/lib/Bindings.hs
--
-- Given a subset of elements and the full accumulator, it:
-- 1. Removes the subset elements from the accumulator
-- 2. Computes a polynomial commitment over G2 for the remaining elements
-- 3. Returns the proof as a compressed G2 point
--
-- Example usage:
-- > let fullElements = ["elem1", "elem2", "elem3", "elem4", "elem5"]
-- >     fullAcc = build fullElements
-- >     subset = ["elem2", "elem4"]
-- > result <- createMembershipProof subset fullAcc defaultCRS
-- > -- Prints: "Success: 0x..." or "Error: ..."
createMembershipProof ::
  HasCallStack =>
  -- | The subset of elements to prove membership of (e.g., UTxOs being fanned out)
  [Element] ->
  -- | The full accumulator from the confirmed snapshot
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point2] ->
  -- | Returns the compressed proof point
  ByteString
createMembershipProof subsetElements (HydraAccumulator fullAcc) crs =
  case getPolyCommitOverG2 subsetElements fullAcc crs of
    Left err -> error (toText err) -- FIXME: return Either?
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
-- 4. The proof is returned as a compressed G2 point
--
-- Example usage for partial fanout:
-- > -- Build full accumulator from ALL UTxOs (do this when creating snapshot)
-- > let fullAcc = buildFromUTxO @Tx fullUTxO
-- >
-- > -- Later, when fanning out a subset
-- > let subsetUTxO = ... -- The 2 UTxOs you want to fan out
-- > proof <- createMembershipProofFromUTxO @Tx subsetUTxO fullAcc crs
--
-- The proof is verified on-chain via e(acc_G1, G2) = e(proof_G1, subsetPoly(τ)·G2).
createMembershipProofFromUTxO ::
  forall tx.
  (IsTx tx, HasCallStack) =>
  -- | The subset of UTxO to prove membership of (e.g., UTxOs being fanned out)
  UTxOType tx ->
  -- | The full accumulator from the confirmed snapshot (built with buildFromUTxO)
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point2] ->
  -- | Returns a proof
  ByteString
createMembershipProofFromUTxO subsetUTxO fullAcc crs = do
  -- Extract individual TxOut elements from the subset (each TxOut -> hash)
  -- This matches how buildFromUTxO / buildFromSnapshotUTxOs serialize each TxOut.
  -- Deduplicate: the same TxOut content can appear under different TxIns (e.g. in
  -- both snapshot.utxo and utxoToCommit); the underlying accumulator stores each
  -- element at most once, so we must only pass each unique element once.
  -- Drop mempty: TxOuts for which toPlutusTxOut returns Nothing yield mempty here;
  -- we only prove membership of outputs that convert successfully.
  let subsetElements = filter (/= mempty) $ nub $ utxoToElement @tx <$> toPairList @tx subsetUTxO
  -- Use the element-based proof function
  createMembershipProof subsetElements fullAcc crs

createCRSG1Datum :: Int -> HApi.TxOutDatum ctx
createCRSG1Datum n =
  TxOutDatumInline BabbageEraOnwardsConway $
    HApi.toScriptData
      (bls12_381_G1_uncompress . toBuiltin . blsCompress <$> crsG1Points n)
