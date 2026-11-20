{-# LANGUAGE OverloadedRecordDot #-}

-- | Concrete snapshot operations for the current Cardano settlement profile.
module Hydra.HeadLogic.Cardano where

import Hydra.HeadLogic.Limits (SnapshotLimits (..), checkOutputCount)
import Hydra.Prelude
import Hydra.Tx (IsTx, UTxOType, combinedUTxO)
import Hydra.Tx.Accumulator (AccumulatorTooLarge, HydraAccumulator, accumulatorSize, maxAccumulatorSize)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature, Signature, aggregateInOrder, getSignableRepresentation)
import Hydra.Tx.Snapshot (Snapshot (..))

cardanoSnapshotLimits :: SnapshotLimits
cardanoSnapshotLimits = SnapshotLimits maxAccumulatorSize

-- | Count elements without forcing a commitment or its cached hash.
checkCommitmentCapacity :: SnapshotLimits -> HydraAccumulator -> Either AccumulatorTooLarge ()
checkCommitmentCapacity limits = checkOutputCount limits . accumulatorSize

snapshotSigningBytes :: IsTx tx => Snapshot tx -> ByteString
snapshotSigningBytes = getSignableRepresentation

assembleCertificate :: Ord party => Map party (Signature (Snapshot tx)) -> [party] -> MultiSignature (Snapshot tx)
assembleCertificate = aggregateInOrder

-- | Maintain both funding alternatives, sharing the commitment when identical.
advanceCommitments :: IsTx tx => Snapshot tx -> UTxOType tx -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> (HydraAccumulator, HydraAccumulator)
advanceCommitments previous after commit decommit = (accumulator, appliedAccumulator)
 where
  accumulator =
    Accumulator.applyUTxODelta
      previous.accumulator
      (combinedUTxO previous.utxo Nothing previous.utxoToDecommit)
      (combinedUTxO after Nothing decommit)
  appliedAccumulator
    | isNothing commit && isNothing decommit = accumulator
    | otherwise =
        Accumulator.applyUTxODelta
          previous.appliedAccumulator
          (combinedUTxO previous.utxo previous.utxoToCommit Nothing)
          (combinedUTxO after commit Nothing)
