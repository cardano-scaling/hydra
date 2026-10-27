-- | Derive a conservative immutable point from blocks actually followed. A
-- 'Tick' from the Cardano backends represents a block, not a wall-clock timer.
module Hydra.Chain.Finality (withFinalityTracking) where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (newTVarIO, writeTVar)
import Data.Sequence qualified as Seq
import Hydra.Chain (ChainCallback, ChainEvent (..))
import Hydra.Chain.ChainState (IsChainState (..), chainStateSlot)

-- | Keep the last @k@ block points, and report a point only after observing @k@
-- descendants on the same branch. Start empty on each connection: after restart
-- we conservatively wait for new descendants instead of trusting a slot age.
-- Rollback notifications may name the last relevant Hydra state before the real
-- intersection; dropping additional points in that case is conservative too.
withFinalityTracking ::
  (MonadSTM m, IsChainState tx) =>
  Natural ->
  ChainCallback tx m ->
  m (ChainCallback tx m)
withFinalityTracking k callback = do
  points <- newTVarIO Seq.empty
  pure $ \event -> do
    finalized <- atomically $ do
      known <- readTVar points
      case event of
        Tick{chainPoint} -> do
          -- Repeated delivery/intersection must not count a block twice.
          let prefix = case Seq.viewr known of
                _ Seq.:> latest | chainPointSlot latest < chainPointSlot chainPoint -> known
                _ -> Seq.takeWhileL ((< chainPointSlot chainPoint) . chainPointSlot) known
              branch = prefix Seq.|> chainPoint
          if fromIntegral (Seq.length branch) > k
            then case Seq.viewl branch of
              point Seq.:< rest -> writeTVar points rest >> pure (Just point)
              Seq.EmptyL -> pure Nothing
            else writeTVar points branch >> pure Nothing
        Rollback{rolledBackChainState} -> do
          writeTVar points $ Seq.takeWhileL ((<= chainStateSlot rolledBackChainState) . chainPointSlot) known
          pure Nothing
        _ -> pure Nothing
    callback event
    forM_ finalized $ \finalizedChainPoint -> callback Finalized{finalizedChainPoint}
