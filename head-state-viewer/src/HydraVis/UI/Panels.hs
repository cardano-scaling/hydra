{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | The information panels stacked below the flow chart: peers, the current
-- snapshot and head balance, node sync, network-message activity, and the raw
-- event / node-state JSON plus the input-authoring form.
module HydraVis.UI.Panels (
  viewStuckBanner,
  viewWaitingBanner,
  viewPeers,
  viewSnapshot,
  viewFanout,
  viewSync,
  viewMessages,
  viewTrace,
  viewEvent,
  viewState,
  viewAuthoring,
) where

import Hydra.Prelude

import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State qualified as HS
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Network (Host)
import Hydra.Network qualified as Net
import Hydra.Node.Environment qualified as Env
import Hydra.Node.State (ChainPointTime (..), NodeState, SyncedStatus (..), chainPointTime, headState, syncedStatus)
import Hydra.Tx.HeadParameters qualified as HP
import Hydra.Tx.IsTx (UTxOType)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Hydra.Tx.Snapshot qualified as Snap
import HydraVis.History (HistoryStep (..))
import HydraVis.Trace (TraceEntry (..), TraceKind (..))
import HydraVis.UI.Model (Action (..), AuthoringCtx (..), Model (..), currentStep, visibleHistory)
import HydraVis.UI.Widgets (UtxoSummary (..), datumText, fmtTime, italic, jsonStringy, lovelaceText, panelStyle, renderJson, row, shortAddr, shortParty, summariseUtxo)
import Miso (View)
import Miso.Html (button_, div_, h2_, onClick, onInput, span_, text, textarea_)
import Miso.Html.Property (rows_, styleInline_, value_)
import Miso.String (MisoString, ms)

-- * Peers

-- | Surface who is in the head: the parties from the head parameters (plus
-- the head id and contestation period), marking our own party when we know it
-- from an 'Environment'. Before the head opens there are no parties on-chain,
-- so we fall back to whatever the 'Environment' configured.
viewPeers :: Model tx -> View (Action tx)
viewPeers m = case currentStep m of
  Nothing -> div_ [] []
  Just step ->
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text "peers"]
      , peersBody (ctx m) (headState (stateAfter step))
      ]

peersBody :: Maybe (AuthoringCtx tx) -> HS.HeadState tx -> View (Action tx)
peersBody mctx = \case
  HS.Idle _ -> case mctx of
    Nothing -> italic "participants are known once the head opens."
    Just c -> fromEnv (ctxEnvironment c)
  HS.Open HS.OpenState{HS.parameters = ps, HS.headId = hid} -> openClosed ps hid
  HS.Closed HS.ClosedState{HS.parameters = ps, HS.headId = hid} -> openClosed ps hid
 where
  me = Env.party . ctxEnvironment <$> mctx

  openClosed ps hid =
    div_
      []
      [ row "head id" (jsonStringy hid)
      , row "contestation period" (show (HP.contestationPeriod ps))
      , row "parties" (show (length (HP.parties ps)))
      , partyList me (HP.parties ps)
      ]

  fromEnv env =
    div_
      []
      [ row "configured peers" (Env.configuredPeers env)
      , row "parties" (show (1 + length (Env.otherParties env)))
      , partyList (Just (Env.party env)) (Env.party env : Env.otherParties env)
      ]

partyList :: Maybe Party -> [Party] -> View (Action tx)
partyList me parties =
  div_
    [styleInline_ "margin-top: 0.25rem;"]
    (map partyRow parties)
 where
  partyRow p =
    div_
      [styleInline_ "display: flex; gap: 0.5rem; margin: 0.1rem 0 0.1rem 1rem; font-family: ui-monospace, monospace;"]
      ( span_ [] [text (ms (shortParty p))]
          : [span_ [styleInline_ "color: #06c;"] [text "(you)"] | Just p == me]
      )

-- * Current snapshot and head balance

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
        snap = getSnapshot @tx cs
     in div_
          []
          [ row "confirmed snapshot #" (showT (number snap))
          , headBalance snap
          , seenSnapshotRows seen
          ]
  HS.Closed HS.ClosedState{HS.confirmedSnapshot = cs, HS.contestationDeadline = deadline, HS.readyToFanoutSent = ready} ->
    let snap = getSnapshot @tx cs
     in div_
          []
          [ row "confirmed snapshot #" (showT (number snap))
          , row "confirmed txs" (showT (length (confirmed snap)))
          , row "contestation deadline" (showT deadline)
          , row "ready to fanout" (showT ready)
          , headBalance snap
          ]
 where
  showT :: Show a => a -> Text
  showT = show
  -- The lovelace held in the head and to whom it is allocated, plus any
  -- pending deposit/decommit, derived from the confirmed snapshot's UTxO.
  headBalance :: Snapshot tx -> View (Action tx)
  headBalance snap =
    utxoSummaryView @tx (Snap.utxo snap) (Snap.utxoToCommit snap) (Snap.utxoToDecommit snap)

seenSnapshotRows :: HS.SeenSnapshot tx -> View (Action tx)
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
  sigSummary mp =
    let n = Map.size mp
        names = T.intercalate ", " (shortParty <$> Map.keys mp)
     in show n <> "  [" <> names <> "]"

utxoSummaryView ::
  forall tx.
  ToJSON (UTxOType tx) =>
  UTxOType tx ->
  Maybe (UTxOType tx) ->
  Maybe (UTxOType tx) ->
  View (Action tx)
utxoSummaryView u toCommit toDecommit =
  let s = summariseUtxo u
   in div_
        [styleInline_ "margin-top: 0.4rem;"]
        ( [ row "outputs in head" (show (usEntries s))
          ]
            <> ( if usValued s
                  then
                    [ row "total in head" (lovelaceText (usTotalLovelace s))
                    , row "datums" (datumText s)
                    , allocationBlock (usByAddress s)
                    ]
                  else [italic "value-less ledger (SimpleTx); no lovelace to show."]
               )
            <> pendingRow "pending deposit (in)" toCommit
            <> pendingRow "pending decommit (out)" toDecommit
        )
 where
  pendingRow lbl = \case
    Nothing -> []
    Just pu ->
      let ps = summariseUtxo pu
          detail =
            show (usEntries ps)
              <> " outputs"
              <> (if usValued ps then ", " <> lovelaceText (usTotalLovelace ps) else "")
       in [row lbl detail]

allocationBlock :: [(Text, Integer)] -> View (Action tx)
allocationBlock [] = div_ [] []
allocationBlock xs =
  div_
    [styleInline_ "margin-top: 0.25rem;"]
    ( span_ [styleInline_ "color: #666;"] [text "allocation"]
        : map addrRow xs
    )
 where
  addrRow (addr, love) =
    div_
      [styleInline_ "display: flex; gap: 0.75rem; margin: 0.1rem 0 0.1rem 1rem;"]
      [ span_
          [styleInline_ "min-width: 22rem; color: #444; font-family: ui-monospace, monospace;"]
          [text (ms (shortAddr addr))]
      , span_
          [styleInline_ "font-family: ui-monospace, monospace;"]
          [text (ms (lovelaceText love))]
      ]

-- * Fanout outputs

-- | Shown only when the cursor's event is a fanout: summarises the outputs the
-- node distributed back to L1, including their datum tally, so it can be
-- compared against the confirmed snapshot's datums above. A drop from inline
-- datums to none is exactly the symptom of #2569 / #1598.
viewFanout :: IsChainState tx => Model tx -> View (Action tx)
viewFanout m = case currentStep m of
  Nothing -> div_ [] []
  Just step -> case stateChanged (event step) of
    HeadFannedOut{finalizedOutputs} -> panel "fanned-out outputs" (summariseUtxo finalizedOutputs)
    HeadPartialFannedOut{distributedOutputs} -> panel "partially fanned-out outputs" (summariseUtxo distributedOutputs)
    _ -> div_ [] []
 where
  panel title s =
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text title]
      , row "outputs" (show (usEntries s))
      , row "total" (lovelaceText (usTotalLovelace s))
      , row "datums" (datumText s)
      , div_
          [styleInline_ "color: #888; font-size: 0.8rem; margin-top: 0.3rem;"]
          [text "Compare datums with the confirmed snapshot above; inline -> none means a datum was dropped at fanout."]
      ]

-- * Stuck-head heuristic

-- | A heuristic warning banner: the head is Open and the confirmed snapshot
-- number has not advanced for a while even though transactions keep arriving
-- (the shape of #1773 / #1415). Counts are over the visible (tick-filtered)
-- history up to the cursor.
viewStuckBanner :: Model tx -> View (Action tx)
viewStuckBanner m = case currentStep m of
  Just step
    | isOpen (headState (stateAfter step)) && stuck ->
        div_
          [styleInline_ "border: 1px solid #c83; background: #fff3e0; color: #843; border-radius: 4px; padding: 0.5rem 0.75rem; margin-bottom: 1rem;"]
          [ text $
              "Possible stuck head: the confirmed snapshot has not advanced in "
                <> ms (show sinceN :: Text)
                <> " events while "
                <> ms (show txSince :: Text)
                <> " transaction(s) arrived."
          ]
  _ -> div_ [] []
 where
  vis = V.toList (V.take (cursor m + 1) (visibleHistory m))
  idxs = zip [0 :: Int ..] vis
  lastConfirmedIdx = case nonEmpty [i | (i, s) <- idxs, isConfirmed (stateChanged (event s))] of
    Just ne -> last ne
    Nothing -> -1
  sinceN = (length vis - 1) - lastConfirmedIdx
  txSince = length [() | (i, s) <- idxs, i > lastConfirmedIdx, isReqTx (stateChanged (event s))]
  stuck = sinceN >= stuckThreshold && txSince > 0
  isConfirmed = \case SnapshotConfirmed{} -> True; _ -> False
  isReqTx = \case TransactionReceived{} -> True; _ -> False
  isOpen = \case HS.Open{} -> True; _ -> False

stuckThreshold :: Int
stuckThreshold = 12

-- * Node trace (from --log)

-- | Trace-log entries at or before the cursor's wall-clock time.
traceUpToCursor :: Model tx -> [TraceEntry]
traceUpToCursor m = case time . event <$> currentStep m of
  Nothing -> []
  Just t -> [e | e <- traceLog m, teTime e <= t]

-- | A prominent banner when the node's latest head-logic outcome at the cursor
-- is a Wait or Error - the thing the event store cannot show (#1374 / #1415).
viewWaitingBanner :: Model tx -> View (Action tx)
viewWaitingBanner m = case latestLogic of
  Just (TWait reason) -> banner ("Node is waiting (trace log): " <> reason)
  Just (TError err) -> banner ("Head-logic Error (trace log): " <> err)
  _ -> div_ [] []
 where
  latestLogic = viaNonEmpty last [teKind e | e <- traceUpToCursor m, isLogic (teKind e)]
  isLogic = \case TReliabilityFail -> False; _ -> True
  banner t =
    div_
      [styleInline_ "border: 1px solid #c33; background: #fdecec; color: #a11; border-radius: 4px; padding: 0.5rem 0.75rem; margin-bottom: 1rem;"]
      [text (ms t)]

-- | Counts of the head-logic outcomes (and reliability failures) seen in the
-- trace log up to the cursor, plus the latest wait/error reason. Hidden when
-- no @--log@ was given.
viewTrace :: Model tx -> View (Action tx)
viewTrace m
  | null (traceLog m) = div_ [] []
  | otherwise =
      let es = traceUpToCursor m
          continues = length [() | TraceEntry{teKind = TContinue} <- es]
          waits = length [() | TraceEntry{teKind = TWait _} <- es]
          errors = length [() | TraceEntry{teKind = TError _} <- es]
          relf = length [() | TraceEntry{teKind = TReliabilityFail} <- es]
          lastReason = viaNonEmpty last [r | e <- es, Just r <- [reasonOf (teKind e)]]
       in div_
            [styleInline_ panelStyle]
            ( [ h2_ [] [text "node trace (from --log)"]
              , row "logic outcomes" (show continues <> " continue, " <> show waits <> " wait, " <> show errors <> " error")
              , row "reliability failures" (show relf)
              ]
                <> [row "latest wait/error" r | Just r <- [lastReason]]
            )
 where
  reasonOf = \case TWait r -> Just r; TError r -> Just r; _ -> Nothing

-- * Node sync

-- | Show whether the node is caught up with the chain, the slot/time it last
-- observed, and how far its clock has drifted, plus running counts of the
-- chain/connectivity events seen up to the cursor.
viewSync :: Model tx -> View (Action tx)
viewSync m = case currentStep m of
  Nothing -> div_ [] []
  Just step ->
    let counts = countActivity (historyUpToCursor m)
        ns = stateAfter step
        ChainPointTime{currentSlot = ChainSlot slot, currentChainTime, drift} =
          chainPointTime ns
        (statusText, statusColor) = case syncedStatus ns of
          InSync -> ("in sync", "#2a7")
          CatchingUp -> ("catching up", "#c83")
     in div_
          [styleInline_ panelStyle]
          [ h2_ [] [text "node sync"]
          , div_
              [styleInline_ "display: flex; gap: 0.75rem; margin: 0.15rem 0;"]
              [ span_ [styleInline_ "min-width: 12rem; color: #666;"] [text "status"]
              , span_
                  [styleInline_ ("font-weight: 600; color: " <> statusColor <> ";")]
                  [text statusText]
              ]
          , row "current slot" (show slot)
          , row "chain time" (fmtTime currentChainTime)
          , row "clock drift" (show drift)
          , div_
              [styleInline_ "display: flex; gap: 0.75rem; margin: 0.15rem 0;"]
              [ span_ [styleInline_ "min-width: 12rem; color: #666;"] [text "network"]
              , let (t, col) = case acConnected counts of
                      Just True -> ("connected", "#2a7")
                      Just False -> ("not connected", "#a33")
                      Nothing -> ("unknown", "#999")
                 in span_ [styleInline_ ("font-weight: 600; color: " <> col <> ";")] [text t]
              ]
          , row "peers connected" (connectedPeersText (acConnectedPeers counts))
          , row "ticks observed" (show (acTicks counts))
          , row "rollbacks" (show (acRollbacks counts))
          , row "sync transitions" (show (acSynced counts) <> " synced, " <> show (acUnsynced counts) <> " unsynced")
          ]

-- * Network messages

-- | Network message activity, grouped by peer where the log records a sender.
-- Raw network messages are not persisted; we count the 'StateChanged' each
-- message produces when applied. Only @AckSn@ (-> 'PartySignedSnapshot')
-- carries its sender, so that is the one we can attribute per peer; the rest
-- (@ReqTx@, @ReqSn@, @ReqDec@) are shown as node-wide totals with a note.
viewMessages :: Model tx -> View (Action tx)
viewMessages m = case currentStep m of
  Nothing -> div_ [] []
  Just step ->
    let c = countActivity (historyUpToCursor m)
        parties = headParties (headState (stateAfter step))
        peers = if null parties then Map.keys (acAckByParty c) else parties
     in div_
          [styleInline_ panelStyle]
          [ h2_ [] [text "network messages"]
          , div_
              [styleInline_ "color: #888; font-size: 0.85rem; margin-bottom: 0.5rem;"]
              [ text
                  "Counted from the persisted state changes each message \
                  \produces; raw sends are not logged. Only AckSn records its \
                  \sender, so it is grouped per peer below; the rest are \
                  \node-wide received counts."
              ]
          , span_ [styleInline_ "color: #666;"] [text "AckSn (snapshot signatures) by peer"]
          , peerAcks peers (acAckByParty c)
          , div_ [styleInline_ "margin-top: 0.6rem; color: #666;"] [text "node-wide (sender not recorded)"]
          , msgRow "ReqTx" "received" (acReqTx c)
          , msgRow "ReqSn" "sent" (acReqSnDecided c)
          , msgRow "ReqSn" "received" (acReqSnRecv c)
          , msgRow "ReqDec" "received" (acReqDec c)
          , msgRow "snapshots" "confirmed" (acSnapConfirmed c)
          , div_ [styleInline_ "margin-top: 0.6rem; color: #666;"] [text "local validation"]
          , msgRow "TxInvalid" "" (acTxInvalid c)
          , invalidErrorRow c
          ]
 where
  invalidErrorRow c = case acLastInvalidError c of
    Just err
      | acTxInvalid c > 0 ->
          div_
            [styleInline_ "margin: 0.1rem 0 0 1rem; color: #a33; font-size: 0.8rem; font-family: ui-monospace, monospace; overflow-wrap: anywhere;"]
            [text ("latest: " <> ms (T.take 200 err))]
    _ -> div_ [] []
  peerAcks [] _ = div_ [styleInline_ "margin-left: 1rem;"] [italic "no peers yet"]
  peerAcks peers byParty =
    div_
      [styleInline_ "margin: 0.1rem 0 0.2rem 1rem;"]
      [ div_
        [styleInline_ "display: flex; gap: 0.75rem; margin: 0.1rem 0; align-items: baseline;"]
        [ span_
            -- All party labels are the same width, so a min-width keeps the
            -- counts in one column sitting just after them.
            [styleInline_ "min-width: 10rem; font-family: ui-monospace, monospace; color: #335;"]
            [text (ms (shortParty p))]
        , numCell (Map.findWithDefault 0 p byParty)
        ]
      | p <- peers
      ]
  msgRow name dir n =
    div_
      [styleInline_ "display: flex; gap: 0.75rem; margin: 0.15rem 0 0.15rem 1rem; align-items: baseline;"]
      [ span_
          [styleInline_ "min-width: 6rem; font-family: ui-monospace, monospace; color: #335;"]
          [text name]
      , span_ [styleInline_ "min-width: 6rem; color: #888;"] [text dir]
      , numCell n
      ]
  -- Counts sit immediately after their (fixed-width) label column and are
  -- left-aligned, so within each area the numbers line up just past the text.
  numCell n =
    span_
      [styleInline_ "font-family: ui-monospace, monospace; font-weight: 600; font-variant-numeric: tabular-nums;"]
      [text (ms (show n :: Text))]

-- | Parties (peers) in the head, from the head parameters; empty before open.
headParties :: HS.HeadState tx -> [Party]
headParties = \case
  HS.Open HS.OpenState{HS.parameters = ps} -> HP.parties ps
  HS.Closed HS.ClosedState{HS.parameters = ps} -> HP.parties ps
  HS.Idle _ -> []

-- | Render the currently-connected peer set as @host:port@, comma separated.
connectedPeersText :: Set Host -> Text
connectedPeersText hs
  | Set.null hs = "none"
  | otherwise = T.intercalate ", " (hostText <$> Set.toList hs)

hostText :: Host -> Text
hostText h = Net.hostname h <> ":" <> show (Net.port h)

-- | Running counts of chain/connectivity events and applied consensus
-- messages, accumulated by walking the history up to the cursor.
data ActivityCounts = ActivityCounts
  { acTicks :: Int
  , acRollbacks :: Int
  , acSynced :: Int
  , acUnsynced :: Int
  , acConnected :: Maybe Bool
  -- ^ Latest network connectivity seen: 'Just True' connected, 'Just False'
  -- disconnected, 'Nothing' if no connectivity event observed yet.
  , acConnectedPeers :: Set Host
  -- ^ Peers currently connected (PeerConnected adds, PeerDisconnected removes).
  , acReqTx :: Int
  , acReqSnRecv :: Int
  , acReqSnDecided :: Int
  , acAckByParty :: Map Party Int
  -- ^ AckSn signatures recorded, grouped by the signing party.
  , acReqDec :: Int
  , acSnapConfirmed :: Int
  , acTxInvalid :: Int
  -- ^ 'TxInvalid' events: a tx the node could not apply to its local UTxO.
  , acLastInvalidError :: Maybe Text
  -- ^ The most recent 'TxInvalid' validation error, for display.
  }

emptyActivity :: ActivityCounts
emptyActivity =
  ActivityCounts
    { acTicks = 0
    , acRollbacks = 0
    , acSynced = 0
    , acUnsynced = 0
    , acConnected = Nothing
    , acConnectedPeers = mempty
    , acReqTx = 0
    , acReqSnRecv = 0
    , acReqSnDecided = 0
    , acAckByParty = mempty
    , acReqDec = 0
    , acSnapConfirmed = 0
    , acTxInvalid = 0
    , acLastInvalidError = Nothing
    }

-- | The prefix of the *full* history (ticks included) up to and including the
-- cursor's event. Counts derived from this stay correct even while ticks are
-- hidden from the slider, which filters 'visibleHistory'.
historyUpToCursor :: Model tx -> Vector (HistoryStep tx)
historyUpToCursor m = case currentStep m of
  Nothing -> V.empty
  Just step ->
    let eid = eventId (event step)
     in V.takeWhile (\s -> eventId (event s) <= eid) (history m)

countActivity :: Vector (HistoryStep tx) -> ActivityCounts
countActivity = V.foldl' bump emptyActivity
 where
  bump c step = case stateChanged (event step) of
    TickObserved{} -> c{acTicks = acTicks c + 1}
    ChainRolledBack{} -> c{acRollbacks = acRollbacks c + 1}
    NodeSynced{} -> c{acSynced = acSynced c + 1}
    NodeUnsynced{} -> c{acUnsynced = acUnsynced c + 1}
    NetworkConnected{} -> c{acConnected = Just True}
    NetworkDisconnected{} -> c{acConnected = Just False}
    PeerConnected{peer} -> c{acConnectedPeers = Set.insert peer (acConnectedPeers c)}
    PeerDisconnected{peer} -> c{acConnectedPeers = Set.delete peer (acConnectedPeers c)}
    TransactionReceived{} -> c{acReqTx = acReqTx c + 1}
    SnapshotRequested{} -> c{acReqSnRecv = acReqSnRecv c + 1}
    SnapshotRequestDecided{} -> c{acReqSnDecided = acReqSnDecided c + 1}
    PartySignedSnapshot{party} -> c{acAckByParty = Map.insertWith (+) party 1 (acAckByParty c)}
    DecommitRecorded{} -> c{acReqDec = acReqDec c + 1}
    SnapshotConfirmed{} -> c{acSnapConfirmed = acSnapConfirmed c + 1}
    TxInvalid{validationError} ->
      c{acTxInvalid = acTxInvalid c + 1, acLastInvalidError = Just (show validationError)}
    _ -> c

-- * Raw event / node state / authoring

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
                <> ms (fmtTime (time (event step)))
          ]
      , renderJson (stateChanged (event step))
      ]

viewState :: ToJSON (NodeState tx) => Model tx -> View (Action tx)
viewState m = case currentStep m of
  Nothing -> div_ [] []
  Just step ->
    div_
      [styleInline_ panelStyle]
      [ h2_ [] [text "node state after this event"]
      , renderJson (stateAfter step)
      ]

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
