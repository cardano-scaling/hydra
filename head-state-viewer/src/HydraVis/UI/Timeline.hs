{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | The transport toolbar: play/seek controls, a playback-speed selector and a
-- marker track that draws a coloured line at each big event (open, increment,
-- decrement, close, fanout, peer joined/left) aligned with the slider.
module HydraVis.UI.Timeline (viewToolbar, eventTimeline, markerLegend) where

import Hydra.Prelude

import Data.Vector (Vector)
import Data.Vector qualified as V
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.StateEvent (stateChanged)
import HydraVis.History (HistoryStep (..))
import HydraVis.UI.Model (Action (..), Model (..), speedTable, visibleHistory)
import HydraVis.UI.Widgets (mInt)
import Miso (View)
import Miso.Html (button_, div_, input_, onClick, onInput, span_, text)
import Miso.Html.Property (max_, min_, step_, styleInline_, type_, value_)
import Miso.String (MisoString, ms)
import Miso.Svg.Attribute qualified as SvgA
import Miso.Svg.Element qualified as Svg

viewToolbar :: Model tx -> View (Action tx)
viewToolbar m =
  div_
    [styleInline_ "margin-bottom: 1rem;"]
    [ div_
        [styleInline_ "display: flex; gap: 0.5rem; align-items: center; flex-wrap: wrap; margin-bottom: 0.4rem;"]
        ( [ button_ [onClick GoFirst] [text "|<"]
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
          , button_ [onClick Next] [text ">"]
          , button_ [onClick GoLast] [text ">|"]
          , span_ [styleInline_ "color: #888; margin-left: 0.5rem;"] [text "speed"]
          ]
            <> speedButtons
            <> [ button_
                  [ onClick ToggleHideTicks
                  , styleInline_ $
                      "margin-left: 0.5rem; padding: 0.25rem 0.6rem; "
                        <> if hideTicks m
                          then "background: #cce5ff; border: 1px solid #6ab;"
                          else "background: #fff; border: 1px solid #ccc;"
                  ]
                  [text (if hideTicks m then "Show Ticks" else "Hide Ticks")]
               , span_
                  [styleInline_ "margin-left: auto; min-width: 7ch; text-align: right; font-variant-numeric: tabular-nums;"]
                  [text $ ms (show oneBased :: Text) <> "/" <> ms (show totalSteps :: Text)]
               ]
        )
    , eventTimeline (visibleHistory m) (cursor m)
    , input_
        [ type_ "range"
        , min_ "0"
        , max_ (ms (show (max 0 (totalSteps - 1)) :: Text))
        , step_ "1"
        , value_ (ms (show (cursor m) :: Text))
        , onInput SetCursor
        , styleInline_ "width: 100%; box-sizing: border-box; margin: 0;"
        ]
    , markerLegend
    ]
 where
  totalSteps :: Int
  totalSteps = V.length (visibleHistory m)
  oneBased :: Int
  oneBased = if totalSteps == 0 then 0 else cursor m + 1
  speedButtons :: [View (Action tx)]
  speedButtons =
    [ button_
      [ onClick (SetSpeed i)
      , styleInline_ $
          "padding: 0.15rem 0.4rem; font-size: 0.8rem; "
            <> if i == speedIdx m
              then "background: #cce5ff; border: 1px solid #6ab;"
              else "background: #fff; border: 1px solid #ccc;"
      ]
      [text lbl]
    | (i, (lbl, _, _)) <- zip [0 ..] speedTable
    ]

-- | Marker kinds surfaced as labelled lines on the timeline.
data MarkerKind = MOpen | MIncrement | MDecrement | MClose | MFanout | MPeerUp | MPeerDown | MTxInvalid
  deriving stock (Eq, Show)

-- | Classify an event as a "big" timeline marker, if it is one.
markerOf :: StateChanged tx -> Maybe MarkerKind
markerOf = \case
  HeadOpened{} -> Just MOpen
  CommitFinalized{} -> Just MIncrement
  DecommitFinalized{} -> Just MDecrement
  HeadClosed{} -> Just MClose
  HeadFannedOut{} -> Just MFanout
  PeerConnected{} -> Just MPeerUp
  PeerDisconnected{} -> Just MPeerDown
  TxInvalid{} -> Just MTxInvalid
  _ -> Nothing

markerColor :: MarkerKind -> MisoString
markerColor = \case
  MOpen -> "#06c"
  MIncrement -> "#393"
  MDecrement -> "#c66"
  MClose -> "#a33"
  MFanout -> "#c83"
  MPeerUp -> "#27a"
  MPeerDown -> "#999"
  MTxInvalid -> "#e33"

markerLabel :: MarkerKind -> MisoString
markerLabel = \case
  MOpen -> "open"
  MIncrement -> "increment"
  MDecrement -> "decrement"
  MClose -> "close"
  MFanout -> "fanout"
  MPeerUp -> "peer joined"
  MPeerDown -> "peer left"
  MTxInvalid -> "tx invalid"

-- | The big events along the (Tick-filtered) timeline, by slider index.
bigEvents :: Vector (HistoryStep tx) -> [(Int, MarkerKind)]
bigEvents steps =
  [(i, k) | (i, s) <- zip [0 ..] (V.toList steps), Just k <- [markerOf (stateChanged (event s))]]

-- | A horizontal marker track: a coloured line at each big event, a blue line
-- at @cursorIdx@, and translucent bands over the windows where a deposit or
-- decommit is in-flight (so e.g. a @tx invalid@ marker landing inside a
-- decommit band is obvious, cf. #1526). Polymorphic in the action type (it has
-- no handlers), so it is reused by the single-node toolbar and the multi-node
-- compare view (one track per node).
eventTimeline :: Vector (HistoryStep tx) -> Int -> View action
eventTimeline steps cursorIdx =
  let n = V.length steps
      xOf i = if n <= 1 then 0 else (i * 1000) `div` (n - 1)
      markers = bigEvents steps
      cur = xOf cursorIdx
      bandRect col (lo, hi) =
        Svg.rect_
          [ SvgA.x_ (mInt (xOf lo))
          , SvgA.y_ "0"
          , SvgA.width_ (mInt (max 4 (xOf hi - xOf lo)))
          , SvgA.height_ "36"
          , SvgA.fill_ col
          , SvgA.opacity_ "0.15"
          ]
          []
      bands =
        map (bandRect "#94c") (pendingIntervals isDepositOpen isDepositClose steps)
          <> map (bandRect "#0aa") (pendingIntervals isDecommitOpen isDecommitClose steps)
   in Svg.svg_
        [ SvgA.viewBox_ "0 0 1000 36"
        , SvgA.width_ "100%"
        , SvgA.preserveAspectRatio_ "none"
        , styleInline_ "display: block; height: 34px;"
        ]
        ( bands
            <> ( Svg.rect_
                  [ SvgA.x_ "0"
                  , SvgA.y_ "14"
                  , SvgA.width_ "1000"
                  , SvgA.height_ "8"
                  , SvgA.fill_ "#eee"
                  ]
                  []
                  : [ Svg.line_
                      [ SvgA.x1_ (mInt (xOf i))
                      , SvgA.y1_ "2"
                      , SvgA.x2_ (mInt (xOf i))
                      , SvgA.y2_ "34"
                      , SvgA.stroke_ (markerColor k)
                      , SvgA.strokeWidth_ "2"
                      ]
                      []
                    | (i, k) <- markers
                    ]
                    <> [ Svg.line_
                          [ SvgA.x1_ (mInt cur)
                          , SvgA.y1_ "0"
                          , SvgA.x2_ (mInt cur)
                          , SvgA.y2_ "36"
                          , SvgA.stroke_ "#06c"
                          , SvgA.strokeWidth_ "3"
                          ]
                          []
                       ]
               )
        )

isDepositOpen, isDepositClose, isDecommitOpen, isDecommitClose :: StateChanged tx -> Bool
isDepositOpen = \case DepositRecorded{} -> True; _ -> False
isDepositClose = \case CommitFinalized{} -> True; _ -> False
isDecommitOpen = \case DecommitRecorded{} -> True; _ -> False
isDecommitClose = \case DecommitFinalized{} -> True; _ -> False

-- | Index ranges (in slider space) where something opened by @opens@ is still
-- in-flight (not yet matched by @closes@). Both the opening and closing event
-- are included in the band.
pendingIntervals :: (StateChanged tx -> Bool) -> (StateChanged tx -> Bool) -> Vector (HistoryStep tx) -> [(Int, Int)]
pendingIntervals opens closes steps = runs [i | (i, True) <- go (0 :: Int) (zip [0 ..] (V.toList steps))]
 where
  go _ [] = []
  go depth ((i, s) : rest) =
    let sc = stateChanged (event s)
        depth' = max 0 (depth + (if opens sc then 1 else 0) - (if closes sc then 1 else 0))
        here = depth > 0 || depth' > 0
     in (i, here) : go depth' rest
  runs [] = []
  runs (x : xs) = grow x x xs
  grow lo hi [] = [(lo, hi)]
  grow lo hi (y : ys)
    | y == hi + 1 = grow lo y ys
    | otherwise = (lo, hi) : grow y y ys

-- | Colour key for the timeline markers and the pending bands.
markerLegend :: View action
markerLegend =
  div_
    [styleInline_ "display: flex; gap: 0.9rem; flex-wrap: wrap; margin-top: 0.35rem; font-size: 0.78rem; color: #555;"]
    ( [swatch (markerColor k) (markerLabel k) | k <- allMarkerKinds]
        <> [ swatch "#94c" "deposit pending"
           , swatch "#0aa" "decommit pending"
           ]
    )
 where
  allMarkerKinds = [MOpen, MIncrement, MDecrement, MClose, MFanout, MPeerUp, MPeerDown, MTxInvalid]
  swatch col lbl =
    span_
      [styleInline_ "display: inline-flex; align-items: center; gap: 0.3rem;"]
      [ span_
          [styleInline_ ("display: inline-block; width: 10px; height: 10px; border-radius: 2px; background: " <> col <> ";")]
          []
      , text lbl
      ]
