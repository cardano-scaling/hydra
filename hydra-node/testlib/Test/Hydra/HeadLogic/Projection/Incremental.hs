{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Independent state projection for comparison with the bounded Agda model.
module Test.Hydra.HeadLogic.Projection.Incremental (Phase (..), phase) where

import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..))
import Hydra.Prelude

data Phase = OutsideHead | NoIncrement | DepositSelected | DecommitPending | ConflictingSelection
  deriving stock (Eq, Show)

phase :: HeadState tx -> Phase
phase = \case
  Open OpenState{coordinatedHeadState = chs} -> case (chs.currentDepositTxId, chs.decommitTx) of
    (Nothing, Nothing) -> NoIncrement
    (Just _, Nothing) -> DepositSelected
    (Nothing, Just _) -> DecommitPending
    (Just _, Just _) -> ConflictingSelection
  _ -> OutsideHead
