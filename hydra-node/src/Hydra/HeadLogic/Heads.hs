{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.HeadLogic.Heads where

import Hydra.Prelude

import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (ChainEvent (..), OtherChainEvent (..))
import Hydra.HeadLogic.Event (Event (..))
import Hydra.HeadLogic.Outcome (Effect (..), Outcome (..))
import Hydra.HeadLogic.State (HeadState)

update ::
  (HeadState tx -> Event tx -> Outcome tx) ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update fn headState = \case
  OnChainEvent OtherChainEvent{otherChainEvent = SomeHeadObserved{headId, pubKeyHashes}} ->
    Effects [ClientEffect $ IgnoredHeadInitializing{headId, pubKeyHashes}]
  event ->
    fn headState event
