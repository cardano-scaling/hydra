{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Multi-node comparison: load several nodes' event logs and, at a shared
-- wall-clock cursor, show each node's snapshot progress and AckSn vectors
-- side by side, flagging divergence. This is the view the stuck-head
-- post-mortems (#1374 / #1415 / #1773) built by hand.
module HydraVis.Compare (
  CompareModel,
  CompareAction,
  mkCompareModel,
  mkCompareApp,
) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State qualified as HS
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Node.State (NodeState, headState)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Hydra.Tx.Snapshot qualified as Snap
import HydraVis.History (HistoryStep (..))
import HydraVis.UI.Timeline (eventTimeline, markerLegend)
import HydraVis.UI.Widgets (UtxoSummary (..), fmtTime, lovelaceText, shortParty, summariseUtxo)
import Miso (App (..), Effect, View, defaultEvents, noEff)
import Miso.Html (button_, div_, h1_, input_, onClick, onInput, span_, text)
import Miso.Html.Property (max_, min_, step_, styleInline_, type_, value_)
import Miso.String (MisoString, ms)
import Miso.String qualified as MS
import Miso.Subscription.Keyboard (Arrows (..), arrowsSub)
import Miso.Types (LogLevel (..))

data CompareModel tx = CompareModel
  { cmNodes :: [(Text, Vector (HistoryStep tx))]
  , cmTimes :: Vector UTCTime
  -- ^ Merged, sorted, de-duplicated event times across all nodes; the cursor
  -- indexes this so scrubbing advances a shared wall-clock.
  , cmCursor :: Int
  }
  deriving stock (Generic)

deriving stock instance (IsChainState tx, Eq (NodeState tx)) => Eq (CompareModel tx)

data CompareAction
  = CNoOp
  | CFirst
  | CPrev
  | CNext
  | CLast
  | CSetCursor MisoString
  | -- | Keyboard arrow keys seek the shared cursor left/right.
    CArrowSeek Arrows
  deriving stock (Generic)

mkCompareModel :: [(Text, [HistoryStep tx])] -> CompareModel tx
mkCompareModel labelled =
  let nodes = [(lbl, V.fromList steps) | (lbl, steps) <- labelled]
      times = sort (ordNub [time (event s) | (_, steps) <- labelled, s <- steps])
   in CompareModel
        { cmNodes = nodes
        , cmTimes = V.fromList times
        , cmCursor = max 0 (length times - 1)
        }

mkCompareApp ::
  (IsChainState tx, ToJSON (StateChanged tx)) =>
  CompareModel tx ->
  App (CompareModel tx) CompareAction
mkCompareApp initialModel =
  App
    { initialAction = CNoOp
    , model = initialModel
    , update = updateCompare
    , view = viewCompare
    , subs = [arrowsSub CArrowSeek]
    , events = defaultEvents
    , mountPoint = Nothing
    , logLevel = Off
    }

updateCompare :: CompareAction -> CompareModel tx -> Effect CompareAction (CompareModel tx)
updateCompare a m = noEff $ case a of
  CNoOp -> m
  CFirst -> m{cmCursor = 0}
  CPrev -> m{cmCursor = clamp (cmCursor m - 1)}
  CNext -> m{cmCursor = clamp (cmCursor m + 1)}
  CLast -> m{cmCursor = lastIdx}
  CSetCursor s -> case readMaybe (MS.unpack s) :: Maybe Int of
    Just n -> m{cmCursor = clamp n}
    Nothing -> m
  CArrowSeek (Arrows ax _) -> m{cmCursor = clamp (cmCursor m + signum ax)}
 where
  lastIdx = max 0 (V.length (cmTimes m) - 1)
  clamp n = max 0 (min lastIdx n)

-- * Per-node summary at a point in time

data NodeSummary = NodeSummary
  { nsTag :: Text
  , nsConfirmed :: Text
  , nsRequested :: Text
  , nsInflight :: Text
  , nsSeen :: Text
  , nsBalance :: Text
  , nsAcks :: [(Text, Int)]
  }

emptySummary :: NodeSummary
emptySummary = NodeSummary "(no events yet)" "-" "-" "-" "-" "-" []

-- | The latest step a node had at or before time @t@.
stepAt :: Vector (HistoryStep tx) -> UTCTime -> Maybe (HistoryStep tx)
stepAt steps t =
  viaNonEmpty last [s | s <- V.toList steps, time (event s) <= t]

-- | The node's event index at or before time @t@ (for positioning its
-- timeline cursor), or 0 when it has no events yet.
stepIndexAt :: Vector (HistoryStep tx) -> UTCTime -> Int
stepIndexAt steps t = max 0 (length [() | s <- V.toList steps, time (event s) <= t] - 1)

nodeSummary :: forall tx. (IsChainState tx, ToJSON (StateChanged tx)) => Vector (HistoryStep tx) -> UTCTime -> NodeSummary
nodeSummary steps t = case stepAt steps t of
  Nothing -> emptySummary
  Just step ->
    let (confN, reqN, sigs, seen, bal) = headInfo (headState (stateAfter step))
     in NodeSummary
          { nsTag = jsonTag (stateChanged (event step))
          , nsConfirmed = confN
          , nsRequested = reqN
          , nsInflight = sigs
          , nsSeen = seen
          , nsBalance = bal
          , nsAcks = acksUpTo steps t
          }
 where
  headInfo :: HS.HeadState tx -> (Text, Text, Text, Text, Text)
  headInfo = \case
    HS.Idle _ -> ("-", "-", "-", "-", "-")
    HS.Open HS.OpenState{HS.coordinatedHeadState = chs} ->
      let HS.CoordinatedHeadState{HS.seenSnapshot = ss, HS.confirmedSnapshot = cs} = chs
          snap = getSnapshot @tx cs
          (reqN, sigs, seenPhase) = case ss of
            HS.NoSeenSnapshot -> ("-", "0", "none")
            HS.LastSeenSnapshot{HS.lastSeen} -> (show lastSeen, "0", "last seen #" <> show lastSeen)
            HS.RequestedSnapshot{HS.requested} -> (show requested, "0", "ReqSn sent, awaiting acks")
            HS.SeenSnapshot{HS.snapshot = s, HS.signatories} ->
              (show (number s), show (Map.size signatories), "in flight #" <> show (number s))
       in (show (number snap), reqN, sigs, seenPhase, balanceText (Snap.utxo snap))
    HS.Closed HS.ClosedState{HS.confirmedSnapshot = cs} ->
      let snap = getSnapshot @tx cs
       in (show (number snap), "-", "-", "closed", balanceText (Snap.utxo snap))
  balanceText u =
    let s = summariseUtxo u
     in if usValued s then lovelaceText (usTotalLovelace s) else "-"

acksUpTo :: Vector (HistoryStep tx) -> UTCTime -> [(Text, Int)]
acksUpTo steps t =
  let counted = foldl' bump Map.empty [s | s <- V.toList steps, time (event s) <= t]
   in [(shortParty p, n) | (p, n) <- Map.toList counted]
 where
  bump :: Map Party Int -> HistoryStep tx -> Map Party Int
  bump acc s = case stateChanged (event s) of
    PartySignedSnapshot{party} -> Map.insertWith (+) party 1 acc
    _ -> acc

jsonTag :: ToJSON a => a -> Text
jsonTag x = case Aeson.toJSON x of
  Aeson.Object o -> case KM.lookup "tag" o of
    Just (Aeson.String t) -> t
    _ -> "?"
  _ -> "?"

-- * View

viewCompare :: (IsChainState tx, ToJSON (StateChanged tx)) => CompareModel tx -> View CompareAction
viewCompare m =
  let n = V.length (cmTimes m)
      curT = cmTimes m V.!? cmCursor m
      summaries = [(lbl, maybe emptySummary (nodeSummary hist) curT) | (lbl, hist) <- cmNodes m]
   in div_
        [styleInline_ "font-family: system-ui, sans-serif; max-width: 1100px; margin: 1rem auto; padding: 0 1rem;"]
        [ h1_ [] [text "hydra-vis - multi-node compare"]
        , toolbar n (cmCursor m) curT
        , timelines (cmNodes m) curT
        , table summaries
        ]

-- | One marker timeline per node, each with its cursor at that node's event
-- index for the shared wall-clock time, so divergent progress is visible at a
-- glance. Reuses 'eventTimeline' from the single-node view.
timelines :: [(Text, Vector (HistoryStep tx))] -> Maybe UTCTime -> View CompareAction
timelines nodes curT =
  div_
    [styleInline_ "border: 1px solid #ddd; border-radius: 4px; padding: 0.75rem 1rem; background: #fafafa; margin-bottom: 1rem;"]
    ( [ div_
        [styleInline_ "display: flex; align-items: center; gap: 0.75rem; margin: 0.25rem 0;"]
        [ span_
            [styleInline_ "min-width: 8rem; color: #666; font-family: ui-monospace, monospace;"]
            [text (ms lbl)]
        , div_
            [styleInline_ "flex: 1; min-width: 0;"]
            [eventTimeline hist (maybe 0 (stepIndexAt hist) curT)]
        ]
      | (lbl, hist) <- nodes
      ]
        <> [markerLegend]
    )

toolbar :: Int -> Int -> Maybe UTCTime -> View CompareAction
toolbar n cur curT =
  div_
    [styleInline_ "display: flex; gap: 0.5rem; align-items: center; flex-wrap: wrap; margin-bottom: 1rem;"]
    [ button_ [onClick CFirst] [text "|<"]
    , button_ [onClick CPrev] [text "<"]
    , input_
        [ type_ "range"
        , min_ "0"
        , max_ (ms (show (max 0 (n - 1)) :: Text))
        , step_ "1"
        , value_ (ms (show cur :: Text))
        , onInput CSetCursor
        , styleInline_ "flex: 1; min-width: 16rem;"
        ]
    , button_ [onClick CNext] [text ">"]
    , button_ [onClick CLast] [text ">|"]
    , span_
        [styleInline_ "min-width: 24ch; text-align: right; color: #555; font-variant-numeric: tabular-nums;"]
        [text (maybe "no events" (ms . fmtTime) curT)]
    ]

-- | The comparison grid: one column per node, one row per metric, with
-- divergent confirmed/requested cells highlighted.
table :: [(Text, NodeSummary)] -> View CompareAction
table summaries =
  let labels = map fst summaries
      sums = map snd summaries
      divergedOn f = diverged (map f sums)
   in div_
        [styleInline_ "border: 1px solid #ddd; border-radius: 4px; padding: 0.75rem 1rem; background: #fafafa; overflow-x: auto;"]
        [ headerRow labels
        , metricRow "last event" (map (\s -> (nsTag s, False)) sums)
        , metricRow "confirmed snapshot #" (map (\s -> (nsConfirmed s, divergedOn nsConfirmed)) sums)
        , metricRow "requested snapshot #" (map (\s -> (nsRequested s, divergedOn nsRequested)) sums)
        , metricRow "seen snapshot" (map (\s -> (nsSeen s, False)) sums)
        , metricRow "in-flight signatories" (map (\s -> (nsInflight s, False)) sums)
        , metricRow "head balance" (map (\s -> (nsBalance s, divergedOn nsBalance)) sums)
        , acksRow sums
        ]
 where
  diverged xs = length (ordNub (filter (/= "-") xs)) > 1

cellStyle :: MisoString
cellStyle = "min-width: 16rem; padding: 0.2rem 0.5rem; font-family: ui-monospace, monospace;"

labelCellStyle :: MisoString
labelCellStyle = "min-width: 13rem; padding: 0.2rem 0.5rem; color: #666;"

headerRow :: [Text] -> View CompareAction
headerRow labels =
  div_
    [styleInline_ "display: flex; border-bottom: 1px solid #ccc; font-weight: 600; margin-bottom: 0.25rem;"]
    ( span_ [styleInline_ labelCellStyle] [text "node"]
        : [span_ [styleInline_ cellStyle] [text (ms lbl)] | lbl <- labels]
    )

metricRow :: MisoString -> [(Text, Bool)] -> View CompareAction
metricRow lbl cells =
  div_
    [styleInline_ "display: flex; align-items: baseline; margin: 0.1rem 0;"]
    ( span_ [styleInline_ labelCellStyle] [text lbl]
        : [ span_
            [styleInline_ (cellStyle <> if hl then " color: #a33; font-weight: 600;" else "")]
            [text (ms v)]
          | (v, hl) <- cells
          ]
    )

acksRow :: [NodeSummary] -> View CompareAction
acksRow sums =
  div_
    [styleInline_ "display: flex; margin: 0.1rem 0;"]
    ( span_ [styleInline_ labelCellStyle] [text "AckSn by party"]
        : [ div_
            [styleInline_ cellStyle]
            [ div_
              [styleInline_ "display: flex; gap: 0.5rem;"]
              [span_ [] [text (ms p)], span_ [styleInline_ "font-weight: 600;"] [text (ms (show c :: Text))]]
            | (p, c) <- nsAcks s
            ]
          | s <- sums
          ]
    )
