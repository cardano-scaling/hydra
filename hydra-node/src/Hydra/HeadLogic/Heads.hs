{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.HeadLogic.Heads where

import Hydra.Prelude

import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (ChainEvent (..))
import Hydra.HeadLogic.Event (Event (..))
import Hydra.HeadLogic.Outcome (Effect (..), Outcome (..))
import Hydra.HeadLogic.State (HeadState)

update ::
  (HeadState tx -> Event tx -> Outcome tx) ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update fn headState = \case
  OnChainEvent IgnoredInitTx{headId, parties} ->
    Effects [ClientEffect $ IgnoredHeadInitializing{headId, parties}]
  event ->
    fn headState event
