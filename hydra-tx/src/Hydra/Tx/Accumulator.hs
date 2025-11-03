{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  HydraAccumulator (..),
  -- createMembershipProof,

  build,
) where

import Hydra.Prelude

import Accumulator (Accumulator)
import Accumulator qualified

-- import Bindings (getPolyCommitOverG2)
-- import Cardano.Crypto.EllipticCurve.BLS12_381 (Point2)

-- * HydraAccumulator

newtype HydraAccumulator = HydraAccumulator {unHydraAccumulator :: Accumulator}
  deriving newtype (Eq, Show)

build :: [ByteString] -> HydraAccumulator
build = HydraAccumulator . Accumulator.buildAccumulator

-- * Cryptographic Proofs (IO functions for partial fanout)

-- | Create a membership proof for a subset of UTxO elements.
-- This function is needed where we need to prove that
-- a subset of UTxOs were actually in the confirmed snapshot.
-- createMembershipProof ::
--   forall tx.
--   IsTx tx =>
--   -- | The subset of UTxO to prove membership of (e.g., the UTxOs being fanned out)
--   UTxOType tx ->
--   -- | The full accumulator from the confirmed snapshot
--   HydraAccumulator ->
--   -- | Common Reference String (CRS) for the cryptographic proof
--   [Point2] ->
--   -- | Either an error message or the membership proof
--   IO (Either String Point2)
-- createMembershipProof partialUTxO (HydraAccumulator fullAcc) crs = do
--   -- Convert the partial UTxO to accumulator elements
--   let partialElements = utxoToElement @tx <$> toPairList partialUTxO
--   -- Generate the cryptographic proof using the Bindings module
--   getPolyCommitOverG2 partialElements fullAcc crs
