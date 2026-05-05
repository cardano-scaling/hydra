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
  generateCRSG2,
  requiredCRSSize,
  defaultItems,

  -- * Membership proofs for partial fanout
  createMembershipProof,
  createMembershipProofFromUTxO,
  createCRSG2Datum,
) where

import Hydra.Prelude

import Accumulator (Accumulator, Element)
import Accumulator qualified
import Bindings (getPolyCommitOverG1)
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
import Plutus.Crypto.BlsUtils (getFinalPoly, getG1Commitment, mkScalar)
import PlutusTx.Builtins (
  BuiltinBLS12_381_G1_Element,
  bls12_381_G2_uncompress,
  byteStringToInteger,
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
    -- Preserve the fanout output ordering (utxo, then commit, then decommit).
    orderedOutputs =
      outputsOfUTxO @tx utxo
        <> outputsOfUTxO @tx utxoToCommit
        <> outputsOfUTxO @tx utxoToDecommit
    elements = utxoToElement @tx <$> orderedOutputs
   in
    build elements

-- | Get a blake2b-256 hash of the accumulator state.
--
-- This is a pure function that returns a 32-byte deterministic hash of the
-- accumulator's contents. It is what gets signed by all parties in the
-- multi-signature and stored as 'accumulatorHash' in on-chain datums.
--
-- Using a fixed-size hash (rather than the full serialized map) keeps datum
-- and redeemer sizes constant regardless of the number of remaining UTxOs,
-- which is critical for keeping partial fanout transaction sizes bounded.
getAccumulatorHash :: HydraAccumulator -> ByteString
getAccumulatorHash (HydraAccumulator acc) =
  digest (Proxy @Blake2b_256) . BSL.toStrict . serialise $ acc

getAccumulatorCommitment :: HydraAccumulator -> BuiltinBLS12_381_G1_Element
getAccumulatorCommitment (HydraAccumulator acc) =
  let n = Map.size acc
   in if n > KZG.maxAccumulatorSize
        then error $ "getAccumulatorCommitment: accumulator has " <> show n <> " elements, exceeding the G1 CRS limit of " <> show KZG.maxAccumulatorSize
        else
          let crsG1 = take (n + 1) KZG.g1BuiltinPoints
           in getG1Commitment crsG1 . getFinalPoly . map (mkScalar . byteStringToInteger BigEndian . toBuiltin . fst) $
                Map.elems acc

-- * CRS (Common Reference String)

-- | Returns the first @n@ G1 powers of tau from the Ethereum EIP-4844 KZG trusted setup.
-- Used as the off-chain CRS for building accumulators and membership proofs.
-- The CRS consists of: [g1 * tau^0, g1 * tau^1, ..., g1 * tau^(n-1)]
-- where tau is the secret from the EIP-4844 multi-party ceremony.
-- Limited to a maximum of 'KZG.maxAccumulatorSize' + 1 points.
generateCRS :: Int -> [Point1]
generateCRS n = take n KZG.g1Points

-- | Returns the first @n@ G2 powers of tau from the Ethereum EIP-4844 KZG trusted setup.
-- Used as the on-chain CRS for verifying membership proofs.
-- Shares the same secret tau as 'generateCRS'.
-- Limited to a maximum of 'KZG.maxFanoutBatchSize' + 1 points.
generateCRSG2 :: Int -> [Point2]
generateCRSG2 n = take n KZG.g2Points

defaultItems :: Int
defaultItems = 30

requiredCRSSize :: HydraAccumulator -> Int
requiredCRSSize (HydraAccumulator acc) = Map.size acc

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
  [Point1] ->
  -- | Returns the compressed proof point
  ByteString
createMembershipProof subsetElements (HydraAccumulator fullAcc) crs =
  case getPolyCommitOverG1 subsetElements fullAcc crs of
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
-- 2. These elements are passed to `getPolyCommitOverG1` which removes them from the accumulator
-- 3. A polynomial commitment proof is generated for the remaining elements
-- 4. The proof is returned as a compressed G1 point
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
  [Point1] ->
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

createCRSG2Datum :: Int -> HApi.TxOutDatum ctx
createCRSG2Datum n =
  TxOutDatumInline BabbageEraOnwardsConway $
    HApi.toScriptData
      (bls12_381_G2_uncompress . toBuiltin . blsCompress <$> generateCRSG2 n)
