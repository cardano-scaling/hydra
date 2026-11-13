{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Independent, bounded checks on the owner boundary, not production guards.
-- SimpleTx has only spending inputs; these checks do not model Cardano scripts,
-- reference inputs, collateral, time validity or rollback eligibility.
module Test.Hydra.HeadLogic.CoordinatedAudit (reservationHeld, decommitReady) where

import Hydra.Prelude

import Data.Set qualified as Set
import Hydra.HeadLogic.State (CoordinatedHeadState (..))
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

reservationHeld :: CoordinatedHeadState SimpleTx -> Bool
reservationHeld chs = case chs.decommitTx of
  Nothing -> True
  Just tx -> Set.disjoint tx.txInputs chs.localUTxO

decommitReady :: CoordinatedHeadState SimpleTx -> SimpleTx -> Bool
decommitReady chs tx =
  carried chs tx
    || tx.txInputs `Set.isSubsetOf` available
 where
  previous = getSnapshot chs.confirmedSnapshot
  available = previous.utxo <> if chs.version > previous.version then fold previous.utxoToCommit else mempty

carried :: CoordinatedHeadState SimpleTx -> SimpleTx -> Bool
carried chs tx = chs.version == previous.version && previous.utxoToDecommit == Just tx.txOutputs
 where
  previous = getSnapshot chs.confirmedSnapshot
