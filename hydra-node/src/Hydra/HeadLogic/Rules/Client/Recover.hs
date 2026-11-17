-- | Request recovery unless a retained finalized increment claims the deposit.
module Hydra.HeadLogic.Rules.Client.Recover (handle) where

import Data.Map.Strict qualified as Map (lookup)
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
 )
import Hydra.HeadLogic.Outcome (Effect (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  decide,
  emit,
  reject,
 )
import Hydra.Node.State (Deposit (..), PendingDeposits)
import Hydra.Prelude hiding (State, handle, state)
import Hydra.Tx (IsTx, TxIdType)

handle :: IsTx tx => ChainSlot -> PendingDeposits tx -> Maybe (TxIdType tx) -> TxIdType tx -> Program tx ()
handle currentSlot pendingDeposits finalizedDeposit recoverTxId = do
  let found = Map.lookup recoverTxId pendingDeposits
  void $ Program.decide "deposit is registered" (isJust found)
  deposit <- case found of
    Nothing -> Program.reject $ RequireFailed NoMatchingDeposit
    Just deposit -> do
      unclaimed <- Program.decide "deposit is not claimed by a finalized increment" (finalizedDeposit /= Just recoverTxId)
      unless unclaimed $ Program.reject $ RequireFailed RecoverBlockedByFinalizedCommit{depositTxId = recoverTxId}
      pure deposit
  let Deposit{headId, deposited} = deposit
  Program.emit OnChainEffect{postChainTx = RecoverTx{headId, recoverTxId, deadline = currentSlot, recoverUTxO = deposited}}
