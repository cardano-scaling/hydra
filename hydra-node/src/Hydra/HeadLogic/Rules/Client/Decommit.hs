{-# LANGUAGE UndecidableInstances #-}

-- | Client admission for a decommit. Receiving ReqDec reserves its inputs later.
module Hydra.HeadLogic.Rules.Client.Decommit (Stage (..), DecommitTopology, handle, workflow) where

import Crem.Topology (Topology (Topology))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Limits (SnapshotLimits)
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  change,
  emit,
 )
import Hydra.HeadLogic.Rules.Decommit qualified as Admission
import Hydra.HeadLogic.State (CoordinatedHeadState (..))
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (
  run,
  workflow,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Prelude hiding (State, handle, state)
import Hydra.Tx (HeadId, IsTx)

data Stage = ValidateDecommit | Broadcast | Finished

type DecommitTopology =
  'Topology
    '[ '( 'ValidateDecommit, '[ 'Broadcast, 'Finished])
     , '( 'Broadcast, '[ 'Finished])
     ]
deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  Validate :: State tx 'ValidateDecommit
  Send :: Admission.ValidatedDecommit tx -> State tx 'Broadcast
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  Validate -> ValidateDecommit
  Send{} -> Broadcast
  Done -> Finished

workflow :: forall tx. IsTx tx => SnapshotLimits -> HeadId -> ChainSlot -> CoordinatedHeadState tx -> tx -> Workflow tx DecommitTopology ()
workflow limits headId currentSlot coordinatedState decommitTx =
  Workflow.workflow (show . statePhase) Validate step
 where
  step :: State tx from -> Transition tx DecommitTopology (State tx) from ()
  step = \case
    Validate -> choose $ do
      result <- Admission.validate limits currentSlot coordinatedState decommitTx
      case result of
        Right checked -> pure $ next $ Send checked
        Left (Admission.NotApplicable reason) -> invalid reason $> next Done
        Left (Admission.Invalid reason) -> invalid reason $> next Done
    Send checked -> continue (const Done) $ Program.emit $ NetworkEffect ReqDec{transaction = Admission.transaction checked}
    Done -> finish Done ()

  invalid :: DecommitInvalidReason tx -> Program tx ()
  invalid reason = Program.change DecommitInvalid{headId, decommitTx, decommitInvalidReason = reason}

-- | Handle one input through the inspectable workflow.
handle :: forall tx. IsTx tx => SnapshotLimits -> HeadId -> ChainSlot -> CoordinatedHeadState tx -> tx -> Program tx ()
handle limits headId currentSlot coordinatedState decommitTx = Workflow.run $ workflow limits headId currentSlot coordinatedState decommitTx
