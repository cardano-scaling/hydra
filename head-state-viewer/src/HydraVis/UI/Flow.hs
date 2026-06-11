{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | The state-transition flow chart: an SVG of the head's macro states
-- (Idle / Open / Closed / fanoutProgress / Final) with edge counts accumulated
-- up to the cursor and the cursor's current edge highlighted.
module HydraVis.UI.Flow (viewFlow) where

import Hydra.Prelude

import Data.Vector (Vector)
import Data.Vector qualified as V
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State qualified as HS
import Hydra.HeadLogic.StateEvent (stateChanged)
import Hydra.Node.State (NodeState, headState)
import HydraVis.History (HistoryStep (..))
import HydraVis.UI.Model (Action, Model (..), currentStep, visibleHistory)
import HydraVis.UI.Widgets (mInt, panelStyle)
import Miso (View)
import Miso.Html (div_, h2_, text)
import Miso.Html.Property (styleInline_)
import Miso.String (MisoString, ms)
import Miso.Svg.Attribute qualified as SvgA
import Miso.Svg.Element qualified as Svg

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
      -- Walking-state /before/ the cursor's own event: used to decide which
      -- partial-fanout bucket the cursor's event lands in (and therefore
      -- which arrow to highlight).
      priorInPartial = wsInPartial (walkHistory (V.take (cursor m) (visibleHistory m)))
      active = activeEdgeFor (currentStep m) priorInPartial
      isE e = active == Just e
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
                  , selfLoopWithLabel 460 200 "contest" (closedContest counts) "#777" (isE EContest)
                  , -- horizontal transitions across the main row
                    arrow 135 200 225 200 (idleToOpen counts) "init" (isE EInit)
                  , arrow 315 200 415 200 (openToClosed counts) "close" (isE EClose)
                  , arrow 505 200 755 200 (closedToFinal counts) "fanout" (isE EFanout)
                  , -- Open side-loops for increment / decrement
                    sideLoopOnOpen 270 OnLeft "decrement" (openDecrement counts) "#c66" (isE EDecrement)
                  , sideLoopOnOpen 270 OnRight "increment" (openIncrement counts) "#393" (isE EIncrement)
                  , -- partial fanout: Closed -> fanoutProgress (diagonal down).
                    -- Endpoints sit on the Closed circle (r=45) and the
                    -- fanoutProgress ellipse (rx=75, ry=32) along the
                    -- centre-to-centre line so the arrowhead touches both
                    -- nodes cleanly.
                    diagArrow 494 230 587 311 (closedToFanoutProgress counts) "partial fanout" "#a37" False (isE EClosedToFanoutProgress)
                  , -- partial fanout: fanoutProgress self-loop (more partials)
                    fanoutProgressSelfLoop 620 (fanoutProgressSelf counts) (isE EFanoutProgressSelf)
                  , -- finalPartialFanout: fanoutProgress -> Final (diagonal up)
                    diagArrow 656 312 765 228 (fanoutProgressToFinal counts) "final partial fanout" "#c83" True (isE EFinalPartialFanout)
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

-- | Which animatable edge (if any) the cursor's event is currently taking.
data ActiveEdge
  = EInit
  | EClose
  | EContest
  | EIncrement
  | EDecrement
  | EClosedToFanoutProgress
  | EFanoutProgressSelf
  | EFinalPartialFanout
  | EFanout
  deriving stock (Eq, Show)

-- | Decide which edge the event at the cursor is taking. The @priorInPartial@
-- argument is the walking-state's @wsInPartial@ flag BEFORE this event, used
-- to distinguish the first partial fanout (Closed -> fanoutProgress) from
-- subsequent partials (fanoutProgress self-loop), and similarly fresh fanout
-- (Closed -> Final) from the closing step of a partial sequence
-- (fanoutProgress -> Final).
activeEdgeFor :: Maybe (HistoryStep tx) -> Bool -> Maybe ActiveEdge
activeEdgeFor Nothing _ = Nothing
activeEdgeFor (Just step) priorInPartial = case stateChanged (event step) of
  HeadOpened{} -> Just EInit
  HeadClosed{} -> Just EClose
  HeadContested{} -> Just EContest
  CommitFinalized{} -> Just EIncrement
  DecommitFinalized{} -> Just EDecrement
  HeadPartialFannedOut{}
    | priorInPartial -> Just EFanoutProgressSelf
    | otherwise -> Just EClosedToFanoutProgress
  HeadFannedOut{}
    | priorInPartial -> Just EFinalPartialFanout
    | otherwise -> Just EFanout
  _ -> Nothing

-- | Edge stroke colour: bright blue when this edge is the one the cursor's
-- event is currently traversing, otherwise the caller's chosen colour.
activeStroke :: MisoString -> Bool -> MisoString
activeStroke _ True = "#06c"
activeStroke c False = c

-- | Edge stroke width: thicker when active.
activeWidth :: MisoString -> Bool -> MisoString
activeWidth _ True = "3.5"
activeWidth w False = w

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

arrow :: Int -> Int -> Int -> Int -> Int -> MisoString -> Bool -> View (Action tx)
arrow x1 y1 x2 y2 n lbl isActive =
  Svg.g_
    []
    [ Svg.line_
        [ SvgA.x1_ (mInt x1)
        , SvgA.y1_ (mInt y1)
        , SvgA.x2_ (mInt x2)
        , SvgA.y2_ (mInt y2)
        , SvgA.stroke_ (activeStroke "#444" isActive)
        , SvgA.strokeWidth_ (activeWidth "2" isActive)
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
diagArrow :: Int -> Int -> Int -> Int -> Int -> MisoString -> MisoString -> Bool -> Bool -> View (Action tx)
diagArrow x1 y1 x2 y2 n lbl color dashed isActive =
  let mx = (x1 + x2) `div` 2
      my = (y1 + y2) `div` 2
      lineAttrs =
        [ SvgA.x1_ (mInt x1)
        , SvgA.y1_ (mInt y1)
        , SvgA.x2_ (mInt x2)
        , SvgA.y2_ (mInt y2)
        , SvgA.stroke_ (activeStroke color isActive)
        , SvgA.strokeWidth_ (activeWidth "1.8" isActive)
        , SvgA.markerEnd_ "url(#arrowhead)"
        ]
          <> [SvgA.strokeDasharray_ "5 4" | dashed && not isActive]
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
fanoutProgressSelfLoop :: Int -> Int -> Bool -> View (Action tx)
fanoutProgressSelfLoop cx n isActive =
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
        , SvgA.stroke_ (activeStroke "#a37" isActive)
        , SvgA.strokeWidth_ (activeWidth "1.8" isActive)
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

-- | A self-loop drawn below the Open node, jutting out to one side. Used
-- for the increment and decrement edges. Anchors at ~45deg below the node
-- centre so the arc starts/ends on the node boundary, not floating off it.
sideLoopOnOpen :: Int -> Side -> MisoString -> Int -> MisoString -> Bool -> View (Action tx)
sideLoopOnOpen cx side lbl n color isActive =
  let sign :: Int
      sign = case side of OnRight -> 1; OnLeft -> -1
      -- Open node is at (cx, 200) with radius 45. Anchor the arc start and
      -- end on the bottom arc of the node (~30deg apart) so both ends touch
      -- the boundary cleanly. Curve out to ~70px sideways and ~50px down.
      x0 = cx + sign * 22
      y0 = 240
      x1 = cx + sign * 70
      y1 = 270
      x2 = cx + sign * 70
      y2 = 290
      x3 = cx + sign * 40
      y3 = 222
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
            , SvgA.stroke_ (activeStroke color isActive)
            , SvgA.strokeWidth_ (activeWidth "1.8" isActive)
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
        [ -- Anchors are at ~45 degrees on the top arc of a radius-45 circle
          -- (so the start/end lie on the node edge, not floating outside).
          SvgA.d_
            ( "M "
                <> mInt (cx - 32)
                <> " "
                <> mInt (cy - 32)
                <> " C "
                <> mInt (cx - 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 32)
                <> " "
                <> mInt (cy - 32)
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
selfLoopWithLabel :: Int -> Int -> MisoString -> Int -> MisoString -> Bool -> View (Action tx)
selfLoopWithLabel cx cy lbl n color isActive =
  Svg.g_
    []
    [ Svg.path_
        [ SvgA.d_
            ( "M "
                <> mInt (cx - 32)
                <> " "
                <> mInt (cy - 32)
                <> " C "
                <> mInt (cx - 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 60)
                <> " "
                <> mInt (cy - 100)
                <> " "
                <> mInt (cx + 32)
                <> " "
                <> mInt (cy - 32)
            )
        , SvgA.fill_ "none"
        , SvgA.stroke_ (activeStroke color isActive)
        , SvgA.strokeWidth_ (activeWidth "1.5" isActive)
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
