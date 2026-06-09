{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | The Miso application driving the visualizer UI.
--
-- The 'Model' is a precomputed list of 'HistoryStep's plus a cursor. Scrubbing
-- the cursor selects a step; the view renders the 'StateChanged' that produced
-- this step, and the resulting 'NodeState' as pretty JSON. No protocol logic
-- runs in 'updateModel' itself; 'HeadLogic.update' was already applied when
-- the history was built (see "HydraVis.History").
module HydraVis.UI (
  Model (..),
  Action (..),
  AuthoringCtx (..),
  mkApp,
  mkModel,
  viewModel,
  applyAction,
  applyInputPure,
) where

import Control.Concurrent (forkIO)

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventId)
import Hydra.HeadLogic (update)
import Hydra.HeadLogic qualified as HL
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (Outcome (..), StateChanged (..))
import Hydra.HeadLogic.State qualified as HS
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger (Ledger)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Tx.IsTx (IsTx)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import HydraVis.History (HistoryStep (..), buildHistory, extendHistory, loadEventsAfter)
import Miso (App (..), Effect, Sub, View, defaultEvents, noEff)
import Miso.Html (
  button_,
  code_,
  div_,
  h1_,
  h2_,
  input_,
  onClick,
  onInput,
  pre_,
  span_,
  text,
  textarea_,
 )
import Miso.Html.Property (max_, min_, rows_, step_, styleInline_, type_, value_)
import Miso.String (MisoString, ms)
import Miso.String qualified as MS
import Miso.Svg.Attribute qualified as SvgA
import Miso.Svg.Element qualified as Svg
import Miso.Types (LogLevel (..))

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
        }

mkApp ::
  ( IsChainState tx
  , FromJSON (StateEvent tx)
  , FromJSON (Input tx)
  , ToJSON (Input tx)
  , ToJSON (NodeState tx)
  , ToJSON (StateChanged tx)
  ) =>
  Model tx ->
  App (Model tx) (Action tx)
mkApp initialModel =
  App
    { initialAction = NoOp
    , model = initialModel
    , update = updateModel
    , view = viewModel
    , subs =
        playSub
          : case (follow initialModel, dbPath initialModel) of
            (True, Just path) -> [followSub path (lastSeenIn initialModel)]
            _ -> []
    , events = defaultEvents
    , mountPoint = Nothing
    , logLevel = Off
    }

lastSeenIn :: Model tx -> Maybe EventId
lastSeenIn m =
  if V.null (history m)
    then Nothing
    else Just (eventId (event (V.last (history m))))

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
  TogglePlay -> noEff $ m{playing = not (playing m)}
  MaybeAdvance ->
    if playing m && cursor m < lastIndex m
      then noEff $ m{cursor = cursor m + 1}
      else
        if playing m && cursor m >= lastIndex m
          then noEff $ m{playing = False}
          else noEff m
 where
  clamp :: Model tx -> Int -> Int
  clamp model n = max 0 (min (lastIndex model) n)
  lastIndex :: Model tx -> Int
  lastIndex model = max 0 (V.length (visibleHistory model) - 1)

-- | The history vector after the optional Tick filter.
visibleHistory :: Model tx -> Vector (HistoryStep tx)
visibleHistory m
  | hideTicks m = V.filter (not . isTickEvent . event) (history m)
  | otherwise = history m

isTickEvent :: StateEvent tx -> Bool
isTickEvent e = case stateChanged e of
  TickObserved{} -> True
  _ -> False

-- | A periodic tick that drives the Play button. Always installed; the
-- update handler checks 'playing' and decides whether to advance.
playSub :: Sub (Action tx)
playSub sink =
  liftIO $
    void $
      forkIO $
        forever $ do
          threadDelay 0.6
          sink MaybeAdvance

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
          (fromIntegral (V.length (history m)) * 1)
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
  withIds startId t scs =
    zipWith
      (\i sc -> StateEvent{eventId = i, stateChanged = sc, time = t})
      [startId ..]
      scs

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
  TogglePlay -> m{playing = not (playing m)}
  MaybeAdvance ->
    if playing m && cursor m < lastIndex
      then m{cursor = cursor m + 1}
      else
        if playing m && cursor m >= lastIndex
          then m{playing = False}
          else m
 where
  lastIndex = max 0 (V.length (visibleHistory m) - 1)
  clamp n = max 0 (min lastIndex n)

-- | Background poll loop. Reads any new rows from the SQLite file at a
-- fixed cadence and dispatches them as 'AppendEvents'.
followSub ::
  (IsChainState tx, FromJSON (StateEvent tx)) =>
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

viewModel ::
  (IsChainState tx, ToJSON (Input tx), ToJSON (NodeState tx), ToJSON (StateChanged tx)) =>
  Model tx ->
  View (Action tx)
viewModel m =
  div_
    [styleInline_ "font-family: system-ui, sans-serif; max-width: 900px; margin: 1rem auto; padding: 0 1rem;"]
    [ h1_ [] [text "hydra-vis"]
    , viewToolbar m
    , viewFlow m
    , viewSnapshot m
    , viewAuthoring m
    , viewEvent m
    , viewState m
    ]

viewToolbar :: Model tx -> View (Action tx)
viewToolbar m =
  div_
    [styleInline_ "display: flex; gap: 0.5rem; align-items: center; margin-bottom: 1rem; flex-wrap: wrap;"]
    [ button_ [onClick GoFirst] [text "|<"]
    , button_ [onClick Prev] [text "<"]
    , button_
        [ onClick TogglePlay
        , styleInline_ $
            "padding: 0.25rem 0.6rem; "
              <> if playing m
                then "background: #ffd8d8; border: 1px solid #c33;"
                else "background: #d8ffd8; border: 1px solid #393;"
        ]
        [text (if playing m then "Pause" else "Play")]
    , input_
        [ type_ "range"
        , min_ "0"
        , max_ (ms (show (max 0 (totalSteps - 1)) :: Text))
        , step_ "1"
        , value_ (ms (show (cursor m) :: Text))
        , onInput SetCursor
        , styleInline_ "flex: 1; min-width: 12rem;"
        ]
    , button_ [onClick Next] [text ">"]
    , button_ [onClick GoLast] [text ">|"]
    , button_
        [ onClick ToggleHideTicks
        , styleInline_ $
            "padding: 0.25rem 0.6rem; "
              <> if hideTicks m
                then "background: #cce5ff; border: 1px solid #6ab;"
                else "background: #fff; border: 1px solid #ccc;"
        ]
        [text (if hideTicks m then "Show Ticks" else "Hide Ticks")]
    , span_
        [styleInline_ "min-width: 7ch; text-align: right; font-variant-numeric: tabular-nums;"]
        [ text $
            ms (show oneBased :: Text)
              <> "/"
              <> ms (show totalSteps :: Text)
        ]
    ]
 where
  totalSteps :: Int
  totalSteps = V.length (visibleHistory m)
  oneBased :: Int
  oneBased = if totalSteps == 0 then 0 else cursor m + 1

viewEvent :: ToJSON (StateChanged tx) => Model tx -> View (Action tx)
viewEvent m = case currentStep m of
  Nothing ->
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text "no events loaded"]
      , text "Pass --db PATH or --gen-db PATH on startup."
      ]
  Just step ->
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text "event"]
      , span_
          [styleInline_ "color: #555;"]
          [ text $
              "eventId="
                <> ms (show (eventId (event step)) :: Text)
                <> " time="
                <> ms (show (time (event step)) :: Text)
          ]
      , renderJson (stateChanged (event step))
      ]

-- | Surface the head's current snapshot so it's immediately visible without
-- having to scroll through the full 'NodeState' JSON. Updates live as
-- 'PartySignedSnapshot' events accumulate, because each one mutates the
-- 'seenSnapshot' record we read from here.
viewSnapshot :: IsChainState tx => Model tx -> View (Action tx)
viewSnapshot m = case currentStep m of
  Nothing -> div_ [] []
  Just step ->
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text "current snapshot"]
      , snapshotBody (headState (stateAfter step))
      ]

snapshotBody :: forall tx. IsChainState tx => HS.HeadState tx -> View (Action tx)
snapshotBody = \case
  HS.Idle _ ->
    italic "head is Idle; no snapshot yet."
  HS.Open HS.OpenState{HS.coordinatedHeadState = chs} ->
    let HS.CoordinatedHeadState{HS.seenSnapshot = seen, HS.confirmedSnapshot = cs} = chs
        confirmedNum = number (getSnapshot @tx cs)
     in div_
          []
          [ row "confirmed snapshot #" (showT confirmedNum)
          , seenSnapshotRows seen
          ]
  HS.Closed HS.ClosedState{HS.confirmedSnapshot = cs} ->
    let snap = getSnapshot @tx cs
     in div_
          []
          [ row "confirmed snapshot #" (showT (number snap))
          , row "confirmed txs" (showT (length (confirmed snap)))
          ]
 where
  showT :: Show a => a -> Text
  showT = show

seenSnapshotRows :: IsTx tx => HS.SeenSnapshot tx -> View (Action tx)
seenSnapshotRows = \case
  HS.NoSeenSnapshot ->
    row "in flight" "none"
  HS.LastSeenSnapshot{HS.lastSeen} ->
    div_
      []
      [row "last seen #" (show lastSeen :: Text), row "in flight" "none"]
  HS.RequestedSnapshot{HS.lastSeen, HS.requested} ->
    div_
      []
      [ row "last seen #" (show lastSeen :: Text)
      , row "requested #" (show requested :: Text)
      , row "in flight" "ReqSn sent, awaiting acks"
      ]
  HS.SeenSnapshot{HS.snapshot = snap, HS.signatories} ->
    div_
      []
      [ row "in flight #" (show (number snap) :: Text)
      , row "signatories" (sigSummary signatories)
      , row "transactions" (show (length (confirmed snap)) :: Text)
      ]
 where
  sigSummary :: Map.Map Party a -> Text
  sigSummary m =
    let n = Map.size m
        shortP p = T.take 8 (T.drop (T.length "Party {vkey = \"") (show p))
        names = T.intercalate ", " (shortP <$> Map.keys m)
     in show n <> "  [" <> names <> "]"

row :: MisoString -> Text -> View (Action tx)
row k v =
  div_
    [styleInline_ "display: flex; gap: 0.75rem; margin: 0.15rem 0;"]
    [ span_ [styleInline_ "min-width: 12rem; color: #666;"] [text k]
    , span_ [styleInline_ "font-family: ui-monospace, monospace;"] [text (ms v)]
    ]

italic :: MisoString -> View (Action tx)
italic t = div_ [styleInline_ "color: #777; font-style: italic;"] [text t]

viewState :: ToJSON (NodeState tx) => Model tx -> View (Action tx)
viewState m = case currentStep m of
  Nothing -> div_ [] []
  Just step ->
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text "node state after this event"]
      , renderJson (stateAfter step)
      ]

panelStyle :: MisoString
panelStyle =
  "border: 1px solid #ddd; border-radius: 4px; padding: 0.75rem 1rem; \
  \margin-bottom: 1rem; background: #fafafa;"

-- | The authoring panel. Hidden when 'ctx' is 'Nothing' (i.e. in Cardano
-- mode, where hand-crafting an 'Input Tx' as JSON is impractical).
viewAuthoring :: ToJSON (Input tx) => Model tx -> View (Action tx)
viewAuthoring m = case ctx m of
  Nothing -> div_ [] []
  Just _ ->
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text "author input"]
      , div_
          [styleInline_ "color: #555; margin-bottom: 0.5rem;"]
          [ text $
              "Pick a preset or type a JSON-encoded Input tx, then click "
                <> "Apply to call HeadLogic.update against the state at "
                <> "the current cursor."
          ]
      , div_
          [styleInline_ "display: flex; gap: 0.5rem; flex-wrap: wrap; margin-bottom: 0.5rem;"]
          (presetButtons (presetInputs m))
      , textarea_
          [ rows_ "6"
          , value_ (inputDraft m)
          , onInput SetInputDraft
          , styleInline_
              "width: 100%; font-family: ui-monospace, monospace; \
              \font-size: 0.85rem; box-sizing: border-box;"
          ]
          []
      , div_
          [styleInline_ "display: flex; gap: 0.5rem; align-items: center; margin-top: 0.5rem;"]
          [ button_ [onClick ApplyDraft] [text "Apply"]
          , case inputError m of
              Nothing -> span_ [styleInline_ "color: #2a7;"] [text "ready"]
              Just err -> span_ [styleInline_ "color: #a33;"] [text err]
          ]
      ]
 where
  presetButtons :: [(MisoString, MisoString)] -> [View (Action tx)]
  presetButtons = map $ \(lbl, json) ->
    button_ [onClick (LoadPreset json)] [text lbl]

presetInputs :: ToJSON (Input tx) => Model tx -> [(MisoString, MisoString)]
presetInputs m = case ctx m of
  Nothing -> []
  Just c -> [(lbl, encodeAsMiso inp) | (lbl, inp) <- ctxPresets c]
 where
  encodeAsMiso :: ToJSON a => a -> MisoString
  encodeAsMiso = ms . TE.decodeUtf8 . BSL.toStrict . encodePretty

-- * Flow chart

-- | Macro states visible in the diagram, matching the spec's main-chain
-- diagram: Idle / Open / Closed / fanoutProgress / Final. fanoutProgress is
-- a synthetic node (no distinct 'HeadState' constructor backs it; we
-- classify a NodeState as 'SFanoutProgress' when we know a partial fanout is
-- in flight). Final is also synthetic: the implementation reverts the head
-- to Idle on 'HeadFannedOut', but the spec models the destination of
-- @fanout@ / @finalPartialFanout@ as a separate terminal node.
data StateKind = SIdle | SOpen | SClosed | SFanoutProgress | SFinal
  deriving stock (Eq, Show)

stateKindOf :: NodeState tx -> StateKind
stateKindOf n = case headState n of
  HS.Idle{} -> SIdle
  HS.Open{} -> SOpen
  HS.Closed{} -> SClosed

-- | Counts of transitions and within-state events accumulated by walking the
-- history in order. We do not have access to the truly-initial 'NodeState'
-- here, so the very first event is treated as a self-loop on whatever state
-- it lands in (real Hydra logs start in 'Idle' and stay there for the first
-- few catch-up events, so this is rarely visible).
data TransitionCounts = TransitionCounts
  { idleToOpen :: Int
  -- ^ \"init\" in spec terminology.
  , openToClosed :: Int
  -- ^ \"close\".
  , idleSelf :: Int
  , openSelf :: Int
  , closedSelf :: Int
  , openIncrement :: Int
  -- ^ 'CommitFinalized' events: increment self-loop on Open.
  , openDecrement :: Int
  -- ^ 'DecommitFinalized' events: decrement self-loop on Open.
  , closedContest :: Int
  -- ^ 'HeadContested' events: contest self-loop on Closed.
  , closedToFanoutProgress :: Int
  -- ^ First 'HeadPartialFannedOut' in a fanout sequence: Closed -> fanoutProgress.
  , fanoutProgressSelf :: Int
  -- ^ Subsequent 'HeadPartialFannedOut' while already in a partial sequence:
  -- fanoutProgress -> fanoutProgress.
  , fanoutProgressToFinal :: Int
  -- ^ 'HeadFannedOut' after one or more partials: fanoutProgress -> Final.
  , closedToFinal :: Int
  -- ^ 'HeadFannedOut' without any preceding partial: Closed -> Final
  -- (single-tx fanout).
  , unexpected :: Int
  -- ^ Anything we did not expect (e.g. Closed -> Open). Surfacing this is
  -- useful for spotting corrupt or surprising logs.
  }
  deriving stock (Eq, Show)

emptyCounts :: TransitionCounts
emptyCounts = TransitionCounts 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Walking state for 'countTransitions'. Tracks whether the head is
-- currently mid-partial-fanout so we can classify the subsequent
-- 'HeadFannedOut' as a fresh fanout vs. the closing step of a partial chain.
data WalkState = WalkState
  { wsPrevKind :: StateKind
  , wsInPartial :: Bool
  , wsCounts :: TransitionCounts
  }

-- | Walk the history and return the full 'WalkState' (counts plus the
-- final 'wsInPartial' flag), so the view can read that flag to decide
-- whether the current macro state is 'SFanoutProgress' rather than 'SClosed'.
walkHistory :: Vector (HistoryStep tx) -> WalkState
walkHistory steps = case V.toList steps of
  [] -> WalkState SIdle False emptyCounts
  (s0 : rest) ->
    let kind0 = stateKindOf (stateAfter s0)
        ws0 = bump (WalkState kind0 False emptyCounts) kind0 s0
     in foldl' stepFold ws0 rest
 where
  stepFold :: WalkState -> HistoryStep tx -> WalkState
  stepFold ws s = bump ws (stateKindOf (stateAfter s)) s

  bump :: WalkState -> StateKind -> HistoryStep tx -> WalkState
  bump ws curr step =
    let prev = wsPrevKind ws
        inPartial = wsInPartial ws
        c = wsCounts ws
     in case stateChanged (event step) of
          CommitFinalized{} ->
            ws{wsPrevKind = curr, wsCounts = c{openIncrement = openIncrement c + 1}}
          DecommitFinalized{} ->
            ws{wsPrevKind = curr, wsCounts = c{openDecrement = openDecrement c + 1}}
          HeadContested{} ->
            ws{wsPrevKind = curr, wsCounts = c{closedContest = closedContest c + 1}}
          HeadPartialFannedOut{} ->
            let c' =
                  if inPartial
                    then c{fanoutProgressSelf = fanoutProgressSelf c + 1}
                    else c{closedToFanoutProgress = closedToFanoutProgress c + 1}
             in ws{wsPrevKind = curr, wsInPartial = True, wsCounts = c'}
          HeadFannedOut{} ->
            let c' =
                  if inPartial
                    then c{fanoutProgressToFinal = fanoutProgressToFinal c + 1}
                    else c{closedToFinal = closedToFinal c + 1}
             in ws{wsPrevKind = curr, wsInPartial = False, wsCounts = c'}
          _ ->
            let c' = case (prev, curr) of
                  (SIdle, SIdle) -> c{idleSelf = idleSelf c + 1}
                  (SOpen, SOpen) -> c{openSelf = openSelf c + 1}
                  (SClosed, SClosed) -> c{closedSelf = closedSelf c + 1}
                  (SIdle, SOpen) -> c{idleToOpen = idleToOpen c + 1}
                  (SOpen, SClosed) -> c{openToClosed = openToClosed c + 1}
                  _ -> c{unexpected = unexpected c + 1}
             in ws{wsPrevKind = curr, wsCounts = c'}

viewFlow :: Model tx -> View (Action tx)
viewFlow m =
  let visible = V.take (cursor m + 1) (visibleHistory m)
      ws = walkHistory visible
      counts = wsCounts ws
      currentNode :: Maybe StateKind
      currentNode = case currentStep m of
        Nothing -> Nothing
        Just step ->
          Just $
            if wsInPartial ws
              then SFanoutProgress
              else stateKindOf (stateAfter step)
      is k = currentNode == Just k
   in div_
        [styleInline_ panelStyle]
        [ h2_ [] [text "state transitions"]
        , Svg.svg_
            [ SvgA.viewBox_ "0 0 920 440"
            , SvgA.width_ "100%"
            , styleInline_ "max-width: 920px; display: block; margin: 0 auto;"
            ]
            ( markerDefs
                : [ -- generic self-loops above the main row
                    selfLoop 90 200 (idleSelf counts)
                  , selfLoop 270 200 (openSelf counts)
                  , selfLoopWithLabel 460 200 "contest" (closedContest counts) "#777"
                  , -- horizontal transitions across the main row
                    arrow 135 200 225 200 (idleToOpen counts) "init"
                  , arrow 315 200 415 200 (openToClosed counts) "close"
                  , arrow 505 200 755 200 (closedToFinal counts) "fanout"
                  , -- Open side-loops for increment / decrement
                    sideLoopOnOpen 270 OnLeft "decrement" (openDecrement counts) "#c66"
                  , sideLoopOnOpen 270 OnRight "increment" (openIncrement counts) "#393"
                  , -- partial fanout: Closed -> fanoutProgress (diagonal down)
                    diagArrow 490 240 565 305 (closedToFanoutProgress counts) "partial fanout" "#a37" False
                  , -- partial fanout: fanoutProgress self-loop (more partials)
                    fanoutProgressSelfLoop 620 (fanoutProgressSelf counts)
                  , -- finalPartialFanout: fanoutProgress -> Final (diagonal up)
                    diagArrow 675 305 750 240 (fanoutProgressToFinal counts) "final partial fanout" "#c83" True
                  , -- nodes
                    stateNode 90 200 "Idle" (is SIdle)
                  , stateNode 270 200 "Open" (is SOpen)
                  , stateNode 460 200 "Closed" (is SClosed)
                  , fanoutProgressNode 620 340 (is SFanoutProgress)
                  , stateNode 800 200 "Final" (is SFinal)
                  ]
                  <> [unexpectedNote (unexpected counts) | unexpected counts > 0]
            )
        ]

data Side = OnLeft | OnRight

markerDefs :: View (Action tx)
markerDefs =
  Svg.defs_
    []
    [ Svg.marker_
        [ SvgA.id_ "arrowhead"
        , SvgA.viewBox_ "0 0 10 10"
        , SvgA.refX_ "9"
        , SvgA.refY_ "5"
        , SvgA.markerWidth_ "7"
        , SvgA.markerHeight_ "7"
        , SvgA.orient_ "auto-start-reverse"
        ]
        [ Svg.path_
            [ SvgA.d_ "M0,0 L10,5 L0,10 z"
            , SvgA.fill_ "#444"
            ]
            []
        ]
    ]

stateNode :: Int -> Int -> MisoString -> Bool -> View (Action tx)
stateNode cx cy nodeLabel isCurrent =
  Svg.g_
    []
    [ Svg.circle_
        [ SvgA.cx_ (mInt cx)
        , SvgA.cy_ (mInt cy)
        , SvgA.r_ "45"
        , SvgA.fill_ (if isCurrent then "#cce5ff" else "#fff")
        , SvgA.stroke_ "#444"
        , SvgA.strokeWidth_ "2"
        ]
        []
    , Svg.text_
        [ SvgA.x_ (mInt cx)
        , SvgA.y_ (mInt (cy + 5))
        , SvgA.textAnchor_ "middle"
        , SvgA.fontSize_ "18"
        , SvgA.fontFamily_ "system-ui, sans-serif"
        , SvgA.fill_ "#111"
        ]
        [text nodeLabel]
    ]

arrow :: Int -> Int -> Int -> Int -> Int -> MisoString -> View (Action tx)
arrow x1 y1 x2 y2 n lbl =
  Svg.g_
    []
    [ Svg.line_
        [ SvgA.x1_ (mInt x1)
        , SvgA.y1_ (mInt y1)
        , SvgA.x2_ (mInt (x2 - 8))
        , SvgA.y2_ (mInt y2)
        , SvgA.stroke_ "#444"
        , SvgA.strokeWidth_ "2"
        , SvgA.markerEnd_ "url(#arrowhead)"
        ]
        []
    , Svg.text_
        [ SvgA.x_ (mInt ((x1 + x2) `div` 2))
        , SvgA.y_ (mInt (y1 - 18))
        , SvgA.textAnchor_ "middle"
        , SvgA.fontSize_ "11"
        , SvgA.fontFamily_ "system-ui, sans-serif"
        , SvgA.fill_ "#777"
        ]
        [text lbl]
    , edgeLabel ((x1 + x2) `div` 2) (y1 - 4) n
    ]

-- | A straight diagonal arrow with a label and count badge. Used for the
-- Closed -> fanoutProgress and fanoutProgress -> Final edges.
diagArrow :: Int -> Int -> Int -> Int -> Int -> MisoString -> MisoString -> Bool -> View (Action tx)
diagArrow x1 y1 x2 y2 n lbl color dashed =
  let mx = (x1 + x2) `div` 2
      my = (y1 + y2) `div` 2
      lineAttrs =
        [ SvgA.x1_ (mInt x1)
        , SvgA.y1_ (mInt y1)
        , SvgA.x2_ (mInt x2)
        , SvgA.y2_ (mInt y2)
        , SvgA.stroke_ color
        , SvgA.strokeWidth_ "1.8"
        , SvgA.markerEnd_ "url(#arrowhead)"
        ]
          <> [SvgA.strokeDasharray_ "5 4" | dashed]
   in Svg.g_
        []
        [ Svg.line_ lineAttrs []
        , Svg.text_
            [ SvgA.x_ (mInt mx)
            , SvgA.y_ (mInt (my - 6))
            , SvgA.textAnchor_ "middle"
            , SvgA.fontSize_ "11"
            , SvgA.fontFamily_ "system-ui, sans-serif"
            , SvgA.fill_ color
            ]
            [text lbl]
        , edgeLabel mx (my + 14) n
        ]

-- | Self-loop drawn below the fanoutProgress ellipse for the @partialFanout@
-- transitions that keep the head in fanoutProgress.
fanoutProgressSelfLoop :: Int -> Int -> View (Action tx)
fanoutProgressSelfLoop cx n =
  Svg.g_
    []
    [ Svg.path_
        [ SvgA.d_
            ( "M "
                <> mInt (cx - 25)
                <> " 372 C "
                <> mInt (cx - 70)
                <> " 425 "
                <> mInt (cx + 70)
                <> " 425 "
                <> mInt (cx + 25)
                <> " 372"
            )
        , SvgA.fill_ "none"
        , SvgA.stroke_ "#a37"
        , SvgA.strokeWidth_ "1.8"
        , SvgA.markerEnd_ "url(#arrowhead)"
        ]
        []
    , Svg.text_
        [ SvgA.x_ (mInt cx)
        , SvgA.y_ "418"
        , SvgA.textAnchor_ "middle"
        , SvgA.fontSize_ "12"
        , SvgA.fontFamily_ "system-ui, sans-serif"
        , SvgA.fill_ "#a37"
        ]
        [text "partial fanout"]
    , edgeLabel cx 398 n
    ]

-- | An elliptical node for fanoutProgress (drawn wider than the circular
-- states so the longer label fits inside).
fanoutProgressNode :: Int -> Int -> Bool -> View (Action tx)
fanoutProgressNode cx cy isCurrent =
  Svg.g_
    []
    [ Svg.ellipse_
        [ SvgA.cx_ (mInt cx)
        , SvgA.cy_ (mInt cy)
        , SvgA.rx_ "75"
        , SvgA.ry_ "32"
        , SvgA.fill_ (if isCurrent then "#cce5ff" else "#fff")
        , SvgA.stroke_ "#444"
        , SvgA.strokeWidth_ "2"
        ]
        []
    , Svg.text_
        [ SvgA.x_ (mInt cx)
        , SvgA.y_ (mInt (cy + 5))
        , SvgA.textAnchor_ "middle"
        , SvgA.fontSize_ "15"
        , SvgA.fontFamily_ "system-ui, sans-serif"
        , SvgA.fill_ "#111"
        ]
        [text "fanoutProgress"]
    ]

-- | A curved arc from the bottom of the Open node out to one side and back,
-- drawn below the main horizontal row so it never crosses the open/close
-- arrows. Used for the increment ('CommitFinalized') and decommit
-- ('DecommitFinalized') self-transitions, which keep the head in Open but
-- represent real protocol events worth surfacing distinctly.
sideLoopOnOpen :: Int -> Side -> MisoString -> Int -> MisoString -> View (Action tx)
sideLoopOnOpen cx side lbl n color =
  let sign :: Int
      sign = case side of OnRight -> 1; OnLeft -> -1
      -- The node now sits at (cx, 200) with radius 45. Anchor below center,
      -- curve out to ~70px sideways and ~50px down.
      x0 = cx + sign * 20
      y0 = 244
      x1 = cx + sign * 70
      y1 = 270
      x2 = cx + sign * 70
      y2 = 290
      x3 = cx + sign * 30
      y3 = 244
      d =
        "M "
          <> mInt x0
          <> " "
          <> mInt y0
          <> " C "
          <> mInt x1
          <> " "
          <> mInt y1
          <> " "
          <> mInt x2
          <> " "
          <> mInt y2
          <> " "
          <> mInt x3
          <> " "
          <> mInt y3
      labelX = cx + sign * 70
      labelY = 280
   in Svg.g_
        []
        [ Svg.path_
            [ SvgA.d_ d
            , SvgA.fill_ "none"
            , SvgA.stroke_ color
            , SvgA.strokeWidth_ "1.8"
            , SvgA.markerEnd_ "url(#arrowhead)"
            ]
            []
        , Svg.text_
            [ SvgA.x_ (mInt labelX)
            , SvgA.y_ (mInt (labelY - 22))
            , SvgA.textAnchor_ "middle"
            , SvgA.fontSize_ "12"
            , SvgA.fontFamily_ "system-ui, sans-serif"
            , SvgA.fill_ color
            ]
            [text lbl]
        , edgeLabel labelX (labelY - 8) n
        ]

selfLoop :: Int -> Int -> Int -> View (Action tx)
selfLoop cx cy n =
  Svg.g_
    []
    [ Svg.path_
        [ SvgA.d_
            ( "M "
                <> mInt (cx - 20)
                <> " "
                <> mInt (cy - 45)
                <> " C "
                <> mInt (cx - 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 20)
                <> " "
                <> mInt (cy - 45)
            )
        , SvgA.fill_ "none"
        , SvgA.stroke_ "#888"
        , SvgA.strokeWidth_ "1.5"
        , SvgA.strokeDasharray_ "4 3"
        , SvgA.markerEnd_ "url(#arrowhead)"
        ]
        []
    , edgeLabel cx (cy - 80) n
    ]

-- | Like 'selfLoop' but with a coloured text label sitting next to the
-- count, used for the 'contest' self-loop on Closed.
selfLoopWithLabel :: Int -> Int -> MisoString -> Int -> MisoString -> View (Action tx)
selfLoopWithLabel cx cy lbl n color =
  Svg.g_
    []
    [ Svg.path_
        [ SvgA.d_
            ( "M "
                <> mInt (cx - 20)
                <> " "
                <> mInt (cy - 45)
                <> " C "
                <> mInt (cx - 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 20)
                <> " "
                <> mInt (cy - 45)
            )
        , SvgA.fill_ "none"
        , SvgA.stroke_ color
        , SvgA.strokeWidth_ "1.5"
        , SvgA.markerEnd_ "url(#arrowhead)"
        ]
        []
    , Svg.text_
        [ SvgA.x_ (mInt cx)
        , SvgA.y_ (mInt (cy - 100))
        , SvgA.textAnchor_ "middle"
        , SvgA.fontSize_ "12"
        , SvgA.fontFamily_ "system-ui, sans-serif"
        , SvgA.fill_ color
        ]
        [text lbl]
    , edgeLabel cx (cy - 80) n
    ]

edgeLabel :: Int -> Int -> Int -> View (Action tx)
edgeLabel cx cy n =
  Svg.g_
    []
    [ Svg.rect_
        [ SvgA.x_ (mInt (cx - 18))
        , SvgA.y_ (mInt (cy - 13))
        , SvgA.width_ "36"
        , SvgA.height_ "20"
        , SvgA.rx_ "10"
        , SvgA.fill_ "#fff"
        , SvgA.stroke_ "#ccc"
        ]
        []
    , Svg.text_
        [ SvgA.x_ (mInt cx)
        , SvgA.y_ (mInt (cy + 3))
        , SvgA.textAnchor_ "middle"
        , SvgA.fontSize_ "13"
        , SvgA.fontFamily_ "system-ui, sans-serif"
        , SvgA.fill_ "#333"
        ]
        [text (ms (show n :: Text))]
    ]

unexpectedNote :: Int -> View (Action tx)
unexpectedNote n =
  Svg.text_
    [ SvgA.x_ "360"
    , SvgA.y_ "210"
    , SvgA.textAnchor_ "middle"
    , SvgA.fontSize_ "12"
    , SvgA.fill_ "#a33"
    ]
    [text $ "(+" <> ms (show n :: Text) <> " unexpected transitions)"]

mInt :: Int -> MisoString
mInt n = ms (show n :: Text)

currentStep :: Model tx -> Maybe (HistoryStep tx)
currentStep m = visibleHistory m V.!? cursor m

renderJson :: ToJSON a => a -> View (Action tx)
renderJson x =
  pre_
    [ styleInline_
        "background: white; border: 1px solid #eee; border-radius: 3px; \
        \padding: 0.5rem; overflow-x: auto; font-size: 0.85rem; \
        \max-height: 30rem; overflow-y: auto;"
    ]
    [ code_ [] [text (ms (jsonText x))]
    ]

jsonText :: ToJSON a => a -> Text
jsonText = TE.decodeUtf8 . BSL.toStrict . encodePretty
