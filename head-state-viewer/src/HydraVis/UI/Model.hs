{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | The visualizer's 'Model' and 'Action' types plus the pure state accessors
-- and the playback-speed table. No view or effect code lives here.
module HydraVis.UI.Model (
  Model (..),
  Action (..),
  AuthoringCtx (..),
  mkModel,
  withTraceLog,
  lastSeenIn,
  visibleHistory,
  isTickEvent,
  currentStep,
  speedTable,
  defaultSpeedIdx,
  clampSpeed,
  speedParams,
) where

import Hydra.Prelude

import Data.Vector (Vector)
import Data.Vector qualified as V
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventId)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger (Ledger)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (NodeState)
import HydraVis.History (HistoryStep (..))
import HydraVis.Trace (TraceEntry)
import Miso.String (MisoString)
import Miso.Subscription.Keyboard (Arrows)

data Model tx = Model
  { history :: Vector (HistoryStep tx)
  , cursor :: Int
  -- ^ Index into the *visible* history (i.e. respects 'hideTicks').
  , initial :: NodeState tx
  -- ^ Folded-from state used when the history is empty (i.e. when the first
  -- batch of events arrives in follow mode against a fresh DB).
  , follow :: Bool
  -- ^ Are we currently polling the event store for new rows?
  , dbPath :: Maybe FilePath
  -- ^ Echoed in the UI so the user can see what they pointed at.
  , inputDraft :: MisoString
  -- ^ Current contents of the authoring textarea.
  , inputError :: Maybe MisoString
  -- ^ Last parse/apply error to surface to the user.
  , ctx :: Maybe (AuthoringCtx tx)
  -- ^ When 'Just', the UI exposes a panel for authoring 'Input' values and
  -- stepping 'HeadLogic.update' manually.
  , hideTicks :: Bool
  -- ^ When true, 'TickObserved' events are filtered out of the time
  -- slider, the JSON panes, and the state-transition counts. Real Hydra
  -- logs are otherwise dominated by ticks.
  , playing :: Bool
  -- ^ Auto-advance the cursor once per 'playSub' tick.
  , speedIdx :: Int
  -- ^ Index into 'speedTable' controlling playback speed.
  , playFrame :: Int
  -- ^ Counts 'playSub' ticks since the last advance; used to slow playback
  -- below the base tick rate (see 'speedTable').
  , traceLog :: [TraceEntry]
  -- ^ Optional node trace-log entries (from @--log@), correlated to the
  -- cursor by wall-clock time to surface Wait reasons the event store omits.
  }
  deriving stock (Generic)

deriving stock instance (IsChainState tx, Eq (NodeState tx)) => Eq (Model tx)

-- | Everything 'HeadLogic.update' needs to run a step that does not come out
-- of a SQLite file. Two of the three fields are first-class values
-- ('Environment' and 'NodeState'); 'Ledger' is a record-of-functions so we
-- treat it as opaque for diffing.
data AuthoringCtx tx = AuthoringCtx
  { ctxEnvironment :: Environment
  , ctxLedger :: Ledger tx
  , ctxPresets :: [(MisoString, Input tx)]
  -- ^ Named pre-canned inputs the UI shows as one-click buttons.
  }

-- | Always-equal so that the Miso diff does not bail on us; the ctx is set
-- once at startup and never changes.
instance Eq (AuthoringCtx tx) where
  _ == _ = True

data Action tx
  = NoOp
  | Next
  | Prev
  | GoFirst
  | GoLast
  | SetCursor MisoString
  | AppendEvents [StateEvent tx]
  | SetInputDraft MisoString
  | LoadPreset MisoString
  | ApplyDraft
  | ToggleHideTicks
  | TogglePlay
  | SetSpeed Int
  | -- | Keyboard arrow keys; seek one step left/right while paused.
    ArrowSeek Arrows
  | -- | Fired by 'playSub' once per tick; the update handler decides whether
    -- to advance based on 'playing'.
    MaybeAdvance
  deriving stock (Generic)

mkModel ::
  Maybe (AuthoringCtx tx) ->
  Maybe FilePath ->
  Bool ->
  NodeState tx ->
  Int ->
  [HistoryStep tx] ->
  Model tx
mkModel authoring path isFollowing initialState startCursor steps =
  let hs = V.fromList steps
      hideT = True
      visN =
        V.length $
          if hideT then V.filter (not . isTickEvent . event) hs else hs
   in Model
        { history = hs
        , cursor = max 0 (min (max 0 (visN - 1)) startCursor)
        , initial = initialState
        , follow = isFollowing
        , dbPath = path
        , inputDraft = ""
        , inputError = Nothing
        , ctx = authoring
        , hideTicks = hideT
        , playing = False
        , speedIdx = defaultSpeedIdx
        , playFrame = 0
        , traceLog = []
        }

-- | Attach parsed node trace-log entries to a model (see "HydraVis.Trace").
withTraceLog :: [TraceEntry] -> Model tx -> Model tx
withTraceLog entries m = m{traceLog = entries}

lastSeenIn :: Model tx -> Maybe EventId
lastSeenIn m =
  if V.null (history m)
    then Nothing
    else Just (eventId (event (V.last (history m))))

-- | The history vector after the optional Tick filter.
visibleHistory :: Model tx -> Vector (HistoryStep tx)
visibleHistory m
  | hideTicks m = V.filter (not . isTickEvent . event) (history m)
  | otherwise = history m

isTickEvent :: StateEvent tx -> Bool
isTickEvent e = case stateChanged e of
  TickObserved{} -> True
  _ -> False

currentStep :: Model tx -> Maybe (HistoryStep tx)
currentStep m = visibleHistory m V.!? cursor m

-- | Playback speeds: @(label, ticksPerStep, stride)@. With the 0.2s base tick,
-- @ticksPerStep@ stretches the interval (for slow motion) and @stride@ jumps
-- multiple events per step (for fast-forward).
speedTable :: [(MisoString, Int, Int)]
speedTable =
  [ ("0.5x", 4, 1)
  , ("1x", 2, 1)
  , ("2x", 1, 1)
  , ("4x", 1, 2)
  , ("8x", 1, 4)
  ]

defaultSpeedIdx :: Int
defaultSpeedIdx = 1

clampSpeed :: Int -> Int
clampSpeed = max 0 . min (length speedTable - 1)

speedParams :: Int -> (Int, Int)
speedParams i = case drop (clampSpeed i) speedTable of
  ((_, period, stride) : _) -> (period, stride)
  [] -> (1, 1)
