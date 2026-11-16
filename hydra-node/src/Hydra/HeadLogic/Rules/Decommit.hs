{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shared decommit admission. Callers choose how to report or retry a refusal;
-- the order and meaning of the validation checks are the same for both inputs.
module Hydra.HeadLogic.Rules.Decommit (Stage (..), AdmissionTopology, Failure (..), ValidatedDecommit, transaction, validate, workflow) where

import Crem.Topology (Topology (Topology))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Limits (SnapshotLimits)
import Hydra.HeadLogic.Program (Program, applyTransaction, decide)
import Hydra.HeadLogic.Snapshot.Capacity (checkDecommitCapacity)
import Hydra.HeadLogic.Snapshot.Settlement (settledUTxO, settlingDecommit)
import Hydra.HeadLogic.State (CoordinatedHeadState (..))
import Hydra.HeadLogic.Validation (ValidatedDecommit (..))
import Hydra.HeadLogic.Workflow (Transition, Workflow, choose, continue, finish, next)
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger (ValidationError (..))
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (getSnapshot)

-- | Only availability and ledger applicability can become valid on retry.
-- Empty outputs and capacity failures are reported immediately by both callers.
data Failure tx = NotApplicable (DecommitInvalidReason tx) | Invalid (DecommitInvalidReason tx)

data Stage = CheckPending | ApplyLedger | CheckOutputs | CheckCapacity | Accepted | Refused

type AdmissionTopology =
  'Topology
    '[ '( 'CheckPending, '[ 'ApplyLedger, 'Refused])
     , '( 'ApplyLedger, '[ 'CheckOutputs, 'Refused])
     , '( 'CheckOutputs, '[ 'CheckCapacity, 'Refused])
     , '( 'CheckCapacity, '[ 'Accepted, 'Refused])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  Pending :: State tx 'CheckPending
  Apply :: State tx 'ApplyLedger
  Outputs :: ~(UTxOType tx) -> State tx 'CheckOutputs
  Capacity :: ~(UTxOType tx) -> State tx 'CheckCapacity
  Accept :: State tx 'Accepted
  Refuse :: Failure tx -> State tx 'Refused

stateStage :: State tx phase -> Stage
stateStage = \case
  Pending -> CheckPending
  Apply -> ApplyLedger
  Outputs{} -> CheckOutputs
  Capacity{} -> CheckCapacity
  Accept -> Accepted
  Refuse{} -> Refused

transaction :: ValidatedDecommit tx -> tx
transaction (ValidatedDecommit tx) = tx

validate :: IsTx tx => SnapshotLimits -> ChainSlot -> CoordinatedHeadState tx -> tx -> Program tx (Either (Failure tx) (ValidatedDecommit tx))
validate limits slot state tx = Workflow.run $ workflow limits slot state tx

workflow :: forall tx. IsTx tx => SnapshotLimits -> ChainSlot -> CoordinatedHeadState tx -> tx -> Workflow tx AdmissionTopology (Either (Failure tx) (ValidatedDecommit tx))
workflow limits slot state tx = Workflow.workflow (show . stateStage) Pending step
 where
  step :: State tx from -> Transition tx AdmissionTopology (State tx) from (Either (Failure tx) (ValidatedDecommit tx))
  step = \case
    Pending -> case state.decommitTx of
      Just existing ->
        continue (const $ Refuse $ NotApplicable DecommitAlreadyInFlight{otherDecommitTxId = txId existing}) $
          decide "no decommit is in flight" False
      Nothing
        | settlingDecommit state.version state.confirmedSnapshot ->
            continue (const $ Refuse $ NotApplicable $ invalid "confirmed decommit awaits settlement") $
              decide "no decommit is in flight" False
      Nothing -> continue (const Apply) $ decide "no decommit is in flight" True
    Apply -> choose $ do
      applied <- applyTransaction slot state.localUTxO tx
      pure $ case applied of
        Left validationError -> next $ Refuse $ NotApplicable DecommitTxInvalid{localUTxO = state.localUTxO, validationError}
        Right after -> next $ Outputs after
    Outputs after
      | utxoFromTx tx == mempty ->
          continue (const $ Refuse $ Invalid $ invalid "decommit transaction has no outputs") $
            decide "decommit produces outputs" False
      | otherwise -> continue (const $ Capacity after) $ decide "decommit produces outputs" True
    Capacity after ->
      let capacity = checkDecommitCapacity limits after (settledUTxO state.version $ getSnapshot state.confirmedSnapshot) tx
       in case capacity of
            Left validationError ->
              continue (const $ Refuse $ Invalid DecommitTxInvalid{localUTxO = state.localUTxO, validationError}) $
                decide "decommit fits local and confirmed capacity" False
            Right () -> continue (const Accept) $ decide "decommit fits local and confirmed capacity" True
    Accept -> finish Accept $ Right $ ValidatedDecommit tx
    Refuse failure -> finish (Refuse failure) $ Left failure

  invalid :: Text -> DecommitInvalidReason tx
  invalid message = DecommitTxInvalid{localUTxO = state.localUTxO, validationError = ValidationError message}
