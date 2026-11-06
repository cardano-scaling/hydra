{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Open-head context around the composed snapshot and transaction owners.
-- Lifecycle delegates here while open and reads the projection when closing.
module Hydra.HeadLogic.Machine.Open (OpenHead, start, view, step) where

import Hydra.Chain.ChainState (IsChainState (ChainStateType))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Machine.Types (DepositOutput (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (OpenState (..))
import Hydra.Tx (HeadId, HeadSeed)
import Hydra.Tx.HeadParameters (HeadParameters)

data OpenHead tx = OpenHead HeadParameters (ChainStateType tx) HeadId HeadSeed (Coordinated.Runtime tx)

start :: IsChainState tx => OpenState tx -> OpenHead tx
start os = OpenHead os.parameters os.chainState os.headId os.headSeed (Coordinated.start os)

view :: OpenHead tx -> OpenState tx
view (OpenHead parameters chainState headId headSeed runtime) =
  OpenState{parameters, chainState, headId, headSeed, coordinatedHeadState = Coordinated.currentState runtime}

step :: IsChainState tx => OpenHead tx -> DepositOutput tx -> OpenHead tx
step (OpenHead parameters chainState headId headSeed runtime) input =
  let next = Coordinated.replayEvent input.settledDeposit input.event runtime
      chainState' = case input.event of
        CommitFinalized{chainState = observed} -> observed
        DecommitFinalized{chainState = observed} -> observed
        ChainRolledBack{chainState = observed} -> observed
        _ -> chainState
   in OpenHead parameters chainState' headId headSeed next
