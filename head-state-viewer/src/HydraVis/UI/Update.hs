{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | State transitions and subscriptions: the Miso 'updateModel', the pure
-- 'applyAction' used by multi-party views, and the background 'playSub' /
-- 'followSub' loops.
module HydraVis.UI.Update (
  updateModel,
  applyAction,
  applyInputPure,
  playSub,
  followSub,
) where

import Control.Concurrent (forkIO)

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Vector qualified as V
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventId)
import Hydra.HeadLogic qualified as HL
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (Outcome (..))
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger (Ledger)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (initialChainTime)
import HydraVis.History (HistoryStep (..), buildHistory, extendHistory, loadEventsAfter)
import HydraVis.UI.Model (
  Action (..),
  AuthoringCtx (..),
  Model (..),
  clampSpeed,
  speedParams,
  visibleHistory,
 )
import Miso (Effect, Sub, noEff)
import Miso.String qualified as MS
import Miso.Subscription.Keyboard (Arrows (..))

updateModel ::
  forall tx.
  (IsChainState tx, FromJSON (Input tx)) =>
  Action tx ->
  Model tx ->
  Effect (Action tx) (Model tx)
updateModel a m = case a of
  NoOp -> noEff m
  Next -> noEff $ m{cursor = clamp m (cursor m + 1)}
  Prev -> noEff $ m{cursor = clamp m (cursor m - 1)}
  GoFirst -> noEff $ m{cursor = 0}
  GoLast -> noEff $ m{cursor = lastIndex m}
  SetCursor s ->
    case readMaybe (MS.unpack s) :: Maybe Int of
      Just n -> noEff $ m{cursor = clamp m n}
      Nothing -> noEff m
  AppendEvents events ->
    let existing = V.toList (history m)
        extended = extendHistory (initial m) existing events
        followingTail = cursor m == max 0 (V.length (visibleHistory m) - 1)
        newModel = m{history = V.fromList extended}
        newCursor =
          if followingTail
            then max 0 (V.length (visibleHistory newModel) - 1)
            else cursor m
     in noEff newModel{cursor = newCursor}
  SetInputDraft s -> noEff $ m{inputDraft = s, inputError = Nothing}
  LoadPreset s -> noEff $ m{inputDraft = s, inputError = Nothing}
  ApplyDraft -> applyDraft m
  ToggleHideTicks -> noEff $ m{hideTicks = not (hideTicks m), cursor = 0}
  TogglePlay -> noEff $ m{playing = not (playing m), playFrame = 0}
  SetSpeed i -> noEff $ m{speedIdx = clampSpeed i, playFrame = 0}
  ArrowSeek arrows -> noEff (arrowSeek arrows m)
  MaybeAdvance -> noEff (stepPlayback (lastIndex m) m)
 where
  clamp :: Model tx -> Int -> Int
  clamp model n = max 0 (min (lastIndex model) n)
  lastIndex :: Model tx -> Int
  lastIndex model = max 0 (V.length (visibleHistory model) - 1)

-- | A periodic tick that drives the Play button. Always installed; the
-- update handler checks 'playing' and decides whether to advance. Ticks at the
-- base rate; 'speedTable' slows playback below this by skipping ticks.
playSub :: Sub (Action tx)
playSub sink =
  liftIO $
    void $
      forkIO $
        forever $ do
          threadDelay 0.2
          sink MaybeAdvance

-- | Seek one step with the keyboard arrows, but only while paused: left
-- ('arrowX' < 0) steps back, right ('arrowX' > 0) steps forward. Released keys
-- and up/down arrows are no-ops. While playing, arrows do nothing.
arrowSeek :: Arrows -> Model tx -> Model tx
arrowSeek Arrows{arrowX} m
  | playing m = m
  | arrowX < 0 = m{cursor = max 0 (cursor m - 1)}
  | arrowX > 0 = m{cursor = min lastIndex (cursor m + 1)}
  | otherwise = m
 where
  lastIndex = max 0 (V.length (visibleHistory m) - 1)

-- | One 'playSub' tick of playback: advance the cursor when enough ticks have
-- elapsed for the current speed, stopping (and clearing 'playing') at the end.
stepPlayback :: Int -> Model tx -> Model tx
stepPlayback lastIndex m
  | not (playing m) = m
  | otherwise =
      let (period, stride) = speedParams (speedIdx m)
          frame = playFrame m + 1
       in if frame < period
            then m{playFrame = frame}
            else
              let next = cursor m + stride
               in if next >= lastIndex
                    then m{cursor = lastIndex, playing = False, playFrame = 0}
                    else m{cursor = next, playFrame = 0}

applyDraft ::
  (IsChainState tx, FromJSON (Input tx)) =>
  Model tx ->
  Effect (Action tx) (Model tx)
applyDraft m = case ctx m of
  Nothing ->
    noEff m{inputError = Just "no authoring context (run with --simple)"}
  Just AuthoringCtx{ctxEnvironment, ctxLedger} ->
    case Aeson.eitherDecodeStrict' (encodeUtf8 (MS.fromMisoString (inputDraft m) :: Text)) of
      Left err ->
        noEff m{inputError = Just (MS.ms ("parse error: " <> err))}
      Right input -> noEff (applyInput ctxEnvironment ctxLedger input m)

-- | Pure version of the @ApplyDraft@ branch of 'updateModel': run
-- 'HeadLogic.update' on @input@ against the state at the cursor (or the
-- model's initial state when the history is empty) and append the resulting
-- 'StateChanged's as new 'HistoryStep's.
applyInputPure ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  Input tx ->
  Model tx ->
  Model tx
applyInputPure = applyInput

applyInput ::
  forall tx.
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  Input tx ->
  Model tx ->
  Model tx
applyInput env ledger input m =
  let priorState = case nonEmpty (V.toList (history m)) of
        -- We append to the *raw* history, so the prior state is the last raw
        -- entry (not the visible one), which respects all events including
        -- the ticks the user may have filtered out.
        Just ne -> stateAfter (last ne)
        Nothing -> initial m
      nextEventId :: Word64
      nextEventId = case nonEmpty (V.toList (history m)) of
        Nothing -> 0
        Just ne -> 1 + eventId (event (last ne))
      walltime =
        addUTCTime
          (fromIntegral (V.length (history m)))
          initialChainTime
      outcome :: Outcome tx
      outcome = HL.update env ledger walltime priorState input
      synthEvents = case outcome of
        Continue{stateChanges} -> withIds nextEventId walltime stateChanges
        Wait{stateChanges} -> withIds nextEventId walltime stateChanges
        Error{} -> []
      newSteps = buildHistory priorState synthEvents
      err = case outcome of
        Error{error = e} ->
          Just (MS.ms ("update returned Error: " <> show (e :: HL.LogicError tx) :: Text))
        _ -> Nothing
   in m
        { history = history m <> V.fromList newSteps
        , cursor =
            if null newSteps
              then cursor m
              else V.length (history m) + length newSteps - 1
        , inputError = err
        , inputDraft = if isNothing err then "" else inputDraft m
        }
 where
  withIds startId t =
    zipWith
      (\i sc -> StateEvent{eventId = i, stateChanged = sc, time = t})
      [startId ..]

-- | Pure variant of the action handler used by multi-party views that don't
-- want to thread 'Miso.Effect'. ApplyDraft falls through to 'applyInput' when
-- the textarea parses; everything else is straightforward field tweaks.
applyAction ::
  forall tx.
  (IsChainState tx, FromJSON (Input tx)) =>
  Action tx ->
  Model tx ->
  Model tx
applyAction a m = case a of
  NoOp -> m
  Next -> m{cursor = clamp (cursor m + 1)}
  Prev -> m{cursor = clamp (cursor m - 1)}
  GoFirst -> m{cursor = 0}
  GoLast -> m{cursor = lastIndex}
  SetCursor s -> case readMaybe (MS.unpack s) :: Maybe Int of
    Just n -> m{cursor = clamp n}
    Nothing -> m
  AppendEvents events ->
    let existing = V.toList (history m)
        extended = extendHistory (initial m) existing events
        followingTail = cursor m == max 0 (V.length (visibleHistory m) - 1)
        newModel = m{history = V.fromList extended}
        newCursor =
          if followingTail
            then max 0 (V.length (visibleHistory newModel) - 1)
            else cursor m
     in newModel{cursor = newCursor}
  SetInputDraft s -> m{inputDraft = s, inputError = Nothing}
  LoadPreset s -> m{inputDraft = s, inputError = Nothing}
  ApplyDraft -> case ctx m of
    Nothing -> m{inputError = Just "no authoring context"}
    Just AuthoringCtx{ctxEnvironment, ctxLedger} ->
      case Aeson.eitherDecodeStrict' (encodeUtf8 (MS.fromMisoString (inputDraft m) :: Text)) of
        Left err -> m{inputError = Just (MS.ms ("parse error: " <> err))}
        Right input -> applyInput ctxEnvironment ctxLedger input m
  ToggleHideTicks -> m{hideTicks = not (hideTicks m), cursor = 0}
  TogglePlay -> m{playing = not (playing m), playFrame = 0}
  SetSpeed i -> m{speedIdx = clampSpeed i, playFrame = 0}
  ArrowSeek arrows -> arrowSeek arrows m
  MaybeAdvance -> stepPlayback lastIndex m
 where
  lastIndex = max 0 (V.length (visibleHistory m) - 1)
  clamp n = max 0 (min lastIndex n)

-- | Background poll loop. Reads any new rows from the SQLite file at a
-- fixed cadence and dispatches them as 'AppendEvents'.
followSub ::
  FromJSON (StateEvent tx) =>
  FilePath ->
  Maybe EventId ->
  Sub (Action tx)
followSub path initialLastSeen sink =
  liftIO $ do
    seenRef <- newIORef initialLastSeen
    _ <- forkIO $ forever $ do
      threadDelay 1.5
      seen <- readIORef seenRef
      newEvents <- loadEventsAfter path seen
      case nonEmpty newEvents of
        Nothing -> pure ()
        Just ne -> do
          modifyIORef' seenRef (\_ -> Just (eventId (last ne)))
          putTextLn $
            "follow: picked up "
              <> show (length ne)
              <> " new events (last eventId = "
              <> show (eventId (last ne))
              <> ")"
          sink (AppendEvents (toList ne))
    pure ()
