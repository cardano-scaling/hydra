{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | One deposit's recorded lifecycle, including consumption and restoration.
module Hydra.HeadLogic.Machine.Deposit (Phase (..), DepositTopology, depositTopology, sDepositTopology, Change (..), phase, machine, transition) where

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), pureResult, runBaseMachineT)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology
import Data.Singletons.Base.TH
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.Node.Deposit (Deposit (..), DepositStatus (..), TrackedDeposit (..))
import Hydra.Prelude

$( singletons
    [d|
      data Phase = MissingDeposit | InactiveDeposit | ActiveDeposit | ExpiredDeposit | ConsumedDeposit

      depositTopology :: Topology Phase
      depositTopology =
        Topology
          [ (MissingDeposit, [InactiveDeposit, ActiveDeposit, ExpiredDeposit])
          , (InactiveDeposit, [MissingDeposit, ActiveDeposit, ExpiredDeposit, ConsumedDeposit])
          , (ActiveDeposit, [MissingDeposit, InactiveDeposit, ExpiredDeposit, ConsumedDeposit])
          , (ExpiredDeposit, [MissingDeposit, InactiveDeposit, ActiveDeposit, ConsumedDeposit])
          , (ConsumedDeposit, [MissingDeposit, InactiveDeposit, ActiveDeposit, ExpiredDeposit])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase

deriving via AllVertices Phase instance RenderableVertices Phase

data Change tx
  = Observe ChainSlot (Deposit tx)
  | Update (Deposit tx)
  | Consume ChainSlot
  | Rollback ChainSlot

data DepositState tx (phase :: Phase) where
  Missing :: DepositState tx 'MissingDeposit
  InactiveState :: TrackedDeposit tx -> DepositState tx 'InactiveDeposit
  ActiveState :: TrackedDeposit tx -> DepositState tx 'ActiveDeposit
  ExpiredState :: TrackedDeposit tx -> DepositState tx 'ExpiredDeposit
  ConsumedState :: TrackedDeposit tx -> DepositState tx 'ConsumedDeposit

phase :: Maybe (TrackedDeposit tx) -> Phase
phase = \case
  Nothing -> MissingDeposit
  Just tracked
    | isJust tracked.consumedAt -> ConsumedDeposit
    | otherwise -> case tracked.deposit.status of
        Inactive -> InactiveDeposit
        Active -> ActiveDeposit
        Expired -> ExpiredDeposit

machine :: forall m tx. Applicative m => Maybe (TrackedDeposit tx) -> BaseMachineT m DepositTopology (Change tx) (Maybe (TrackedDeposit tx))
machine initial = BaseMachineT{initialState, action}
 where
  initialState :: InitialState (DepositState tx)
  initialState = case initial of
    Nothing -> InitialState Missing
    Just tracked
      | isJust tracked.consumedAt -> InitialState $ ConsumedState tracked
      | otherwise -> case tracked.deposit.status of
          Inactive -> InitialState $ InactiveState tracked
          Active -> InitialState $ ActiveState tracked
          Expired -> InitialState $ ExpiredState tracked

  action :: forall from. DepositState tx from -> Change tx -> ActionResult m DepositTopology (DepositState tx) from (Maybe (TrackedDeposit tx))
  action = \case
    Missing -> \case
      Observe recordedAt deposit -> observed TrackedDeposit{deposit, recordedAt, consumedAt = Nothing}
      _ -> pureResult Nothing Missing
    InactiveState tracked -> present tracked
    ActiveState tracked -> present tracked
    ExpiredState tracked -> present tracked
    ConsumedState tracked -> present tracked

-- | Unknown deposits ignore updates and consumption. Observation always starts
-- a fresh lifecycle; an update preserves both recorded and consumed slots.
present ::
  forall m from tx.
  ( Applicative m
  , AllowedTransition DepositTopology from 'MissingDeposit
  , AllowedTransition DepositTopology from 'InactiveDeposit
  , AllowedTransition DepositTopology from 'ActiveDeposit
  , AllowedTransition DepositTopology from 'ExpiredDeposit
  , AllowedTransition DepositTopology from 'ConsumedDeposit
  ) =>
  TrackedDeposit tx ->
  Change tx ->
  ActionResult m DepositTopology (DepositState tx) from (Maybe (TrackedDeposit tx))
present tracked = \case
  Observe recordedAt deposit -> observed TrackedDeposit{deposit, recordedAt, consumedAt = Nothing}
  Update deposit -> retained tracked{deposit}
  Consume slot -> retained tracked{consumedAt = Just slot}
  Rollback slot
    | tracked.recordedAt > slot -> pureResult Nothing Missing
    | otherwise -> retained tracked{consumedAt = mfilter (<= slot) tracked.consumedAt}
 where
  retained :: TrackedDeposit tx -> ActionResult m DepositTopology (DepositState tx) from (Maybe (TrackedDeposit tx))
  retained value
    | isJust value.consumedAt = pureResult (Just value) $ ConsumedState value
    | otherwise = observed value

observed ::
  ( Applicative m
  , AllowedTransition DepositTopology from 'InactiveDeposit
  , AllowedTransition DepositTopology from 'ActiveDeposit
  , AllowedTransition DepositTopology from 'ExpiredDeposit
  ) =>
  TrackedDeposit tx ->
  ActionResult m DepositTopology (DepositState tx) from (Maybe (TrackedDeposit tx))
observed tracked = case tracked.deposit.status of
  Inactive -> pureResult (Just tracked) $ InactiveState tracked
  Active -> pureResult (Just tracked) $ ActiveState tracked
  Expired -> pureResult (Just tracked) $ ExpiredState tracked

-- | Apply one persisted change to the canonical tracked value. Keeping the
-- returned crem continuation also supports processing a sequence in memory.
transition :: Change tx -> Maybe (TrackedDeposit tx) -> Maybe (TrackedDeposit tx)
transition change initial = fst $ runIdentity $ runBaseMachineT (machine initial) change
