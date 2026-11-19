module Hydra.HeadLogic.Rules.Client.Recover (handleRecover) where

import Data.Map.Strict qualified as Map (lookup)
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
 )
import Hydra.HeadLogic.Outcome (Effect (..))
import Hydra.HeadLogic.Program (
  Program,
  decide,
  emit,
  reject,
 )
import Hydra.Node.State (Deposit (..), PendingDeposits)
import Hydra.Prelude hiding (State, handle, state)
import Hydra.Tx (IsTx, TxIdType)

-- | Request a recovery unless a retained finalized increment claims the
-- deposit.
handleRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  TxIdType tx ->
  Program tx ()
handleRecover currentSlot pendingDeposits finalizedDeposit recoverTxId = do
  let found = Map.lookup recoverTxId pendingDeposits

  void $ decide "deposit is registered" (isJust found)

  deposit <- case found of
    Nothing -> reject $ RequireFailed NoMatchingDeposit
    Just deposit -> do
      unclaimed <-
        decide
          "deposit is not claimed by a finalized increment"
          (finalizedDeposit /= Just recoverTxId)

      unless unclaimed $
        reject $
          RequireFailed RecoverBlockedByFinalizedCommit{depositTxId = recoverTxId}

      pure deposit

  let Deposit{headId, deposited} = deposit

  emit
    OnChainEffect
      { postChainTx =
          RecoverTx
            { headId
            , recoverTxId
            , deadline = currentSlot
            , recoverUTxO = deposited
            }
      }
