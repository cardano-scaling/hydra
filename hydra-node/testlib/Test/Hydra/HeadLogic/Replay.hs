{-# LANGUAGE OverloadedRecordDot #-}

-- | Record and replay primitive tapes against an explicit starting world.
module Test.Hydra.HeadLogic.Replay (recordScenario, replayScenario) where

import Data.List (delete)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.Prelude hiding (delete)
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.Explorer (
  actions,
  advance,
  applyExecutionMode,
  installOutcome,
  programFor,
 )
import Test.Hydra.HeadLogic.Scenario (
  Action (Deliver, Retry),
  Disturbances (AllowInvalidAcknowledgement),
  Exploration (executionMode),
  Limits,
  Scenario (
    Scenario,
    exploration,
    primitiveTapes,
    schedule,
    schemaVersion,
    startingWorld
  ),
  World (pending, retries),
  actionLabel,
  validateWorld,
 )

recordScenario :: Limits -> Exploration -> World -> [Action] -> Either Text (Scenario, [Debug.Step])
recordScenario bounds settings start route = do
  validateWorld start
  (tapes, views) <- go start route
  pure (Scenario 2 settings start bounds route tapes, views)
 where
  go _ [] = Right ([], [])
  go world (action : rest) = do
    next <- advance settings.executionMode world action
    (tape, view) <- case action of
      Deliver d -> record world d
      Retry d -> record world d
      _ -> Right ([], [])
    (tapes, views) <- go next rest
    pure (tape : tapes, Debug.Step Debug.InputStep (actionLabel action) Nothing (toJSON action) : view <> views)
  record world d = case applyExecutionMode settings.executionMode $ programFor world d of
    Interpreter.Execution config program -> do
      prepared <- Debug.prepare Debug.RecordReplies config program
      pure (prepared.queries, prepared.steps)

replayScenario :: Scenario -> Either Text (World, [Debug.Step])
replayScenario scenario
  | scenario.schemaVersion /= 2 = Left "unsupported scenario version"
  | length scenario.schedule /= length scenario.primitiveTapes = Left "scenario tape count mismatch"
  | otherwise = validateWorld scenario.startingWorld >> go scenario.startingWorld (zip scenario.schedule scenario.primitiveTapes)
 where
  go world [] = Right (world, [])
  go world ((action, tape) : rest) = do
    unless (action `elem` actions AllowInvalidAcknowledgement world) $ Left "replay action is not enabled"
    (next, view) <- case action of
      Deliver d -> consume world{pending = delete d world.pending} d tape
      Retry d -> consume world{retries = delete d world.retries} d tape
      _ -> do
        unless (null tape) $ Left "non-delivery action has primitive replies"
        next <- advance scenario.exploration.executionMode world action
        pure (next, [])
    (final, views) <- go next rest
    pure (final, Debug.Step Debug.InputStep (actionLabel action) Nothing (toJSON action) : view <> views)
  consume world d tape = case applyExecutionMode scenario.exploration.executionMode $ programFor world d of
    Interpreter.Execution config program -> do
      prepared <- Debug.prepare (Debug.RecordedReplies tape) config program
      let outcome = Interpreter.runProgramWith config prepared.lowered
      pure (installOutcome world d outcome, prepared.steps)
