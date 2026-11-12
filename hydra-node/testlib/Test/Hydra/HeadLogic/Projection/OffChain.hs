{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Independent state projection for comparison with the bounded Agda model.
module Test.Hydra.HeadLogic.Projection.OffChain (Phase (..), phase) where

import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (..))
import Hydra.Prelude

data Phase = OutsideHead | NoRequestSeen | SnapshotIdle | RequestSent | CollectingSignatures
  deriving stock (Eq, Show)

phase :: HeadState tx -> Phase
phase = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{seenSnapshot}} -> case seenSnapshot of
    NoSeenSnapshot -> NoRequestSeen
    LastSeenSnapshot{} -> SnapshotIdle
    RequestedSnapshot{} -> RequestSent
    SeenSnapshot{} -> CollectingSignatures
  _ -> OutsideHead
