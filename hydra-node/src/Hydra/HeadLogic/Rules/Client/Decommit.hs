{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Rules.Client.Decommit (
  Stage (..),
  DecommitTopology,
  Args (..),
  workflow,
) where

import Crem.Topology (Topology (Topology))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Limits (SnapshotLimits)
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program,
  change,
  emit,
 )
import Hydra.HeadLogic.Rules.Decommit qualified as Admission
import Hydra.HeadLogic.State (CoordinatedHeadState)
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (
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

data Args tx = Args
  { snapshotLimits :: ~SnapshotLimits
  , headId :: ~HeadId
  , currentSlot :: ~ChainSlot
  , coordinatedHeadState :: ~(CoordinatedHeadState tx)
  , decommitTx :: ~tx
  }

workflow :: forall tx. IsTx tx => Args tx -> Workflow tx DecommitTopology ()
workflow Args{..} =
  Workflow.workflow (show . statePhase) Validate step
 where
  step :: State tx from -> Transition tx DecommitTopology (State tx) from ()
  step = \case
    Validate -> choose $ do
      result <- Admission.validate snapshotLimits currentSlot coordinatedHeadState decommitTx
      case result of
        Right checked -> pure $ next $ Send checked
        Left (Admission.NotApplicable reason) -> invalid reason $> next Done
        Left (Admission.Invalid reason) -> invalid reason $> next Done
    Send checked -> continue (const Done) $ emit $ NetworkEffect ReqDec{transaction = Admission.transaction checked}
    Done -> finish Done ()

  invalid :: DecommitInvalidReason tx -> Program tx ()
  invalid reason = change DecommitInvalid{headId, decommitTx, decommitInvalidReason = reason}
