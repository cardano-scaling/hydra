{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Recover a registered deposit unless a retained finalized increment claims
-- it. The chain checks the deadline; this rule forwards the current slot.
module Hydra.HeadLogic.Rules.Recover (Phase (..), RecoverTopology, machine) where

import Crem.Topology (
  Topology (Topology),
 )
import Data.Map.Strict qualified as Map
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Error (LogicError (..), RequirementFailure (..))
import Hydra.HeadLogic.Outcome (Effect (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
  workflow,
 )
import Hydra.Node.State (Deposit (..), PendingDeposits)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, TxIdType)

data Phase = CheckDeposit | PostRecovery | Finished

type RecoverTopology =
  'Topology
    '[ '( 'CheckDeposit, '[ 'PostRecovery])
     , '( 'PostRecovery, '[ 'Finished])
     ]

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase

data State tx phase where
  DepositCheck :: State tx 'CheckDeposit
  Recoverable :: Deposit tx -> State tx 'PostRecovery
  Done :: State tx 'Finished

statePhase :: State tx phase -> Phase
statePhase = \case
  DepositCheck -> CheckDeposit
  Recoverable{} -> PostRecovery
  Done -> Finished

machine :: forall tx. IsTx tx => ChainSlot -> PendingDeposits tx -> Maybe (TxIdType tx) -> TxIdType tx -> Workflow tx RecoverTopology
machine currentSlot pendingDeposits finalizedDeposit recoverTxId =
  workflow (show . statePhase) DepositCheck step
 where
  step :: State tx from -> Transition tx RecoverTopology (State tx) from
  step = \case
    DepositCheck -> continue Recoverable $ do
      let found = Map.lookup recoverTxId pendingDeposits
      void $ Program.decide "deposit is registered" (isJust found)
      case found of
        Nothing -> Program.reject $ RequireFailed NoMatchingDeposit
        Just deposit -> do
          unclaimed <- Program.decide "deposit is not claimed by a finalized increment" (finalizedDeposit /= Just recoverTxId)
          unless unclaimed $ Program.reject $ RequireFailed RecoverBlockedByFinalizedCommit{depositTxId = recoverTxId}
          pure deposit
    Recoverable Deposit{headId, deposited} ->
      continue (const Done) $
        Program.emit OnChainEffect{postChainTx = RecoverTx{headId, recoverTxId, deadline = currentSlot, recoverUTxO = deposited}}
    Done -> finish Done
