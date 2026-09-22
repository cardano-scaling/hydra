{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- Checkout [Hydra
-- Documentation](https://hydra.family/head-protocol/docs/dev/architecture)
-- for some details about the overall architecture of the `Node`.
module Hydra.Node where

import Hydra.Prelude

import Conduit (MonadUnliftIO, ZipSink (..), foldMapC, foldlC, mapC, runConduitRes, sinkList, (.|))
import Control.Concurrent.Class.MonadSTM (
  stateTVar,
  writeTVar,
 )
import Control.Monad.Trans.Writer (execWriter, tell)
import Control.Tracer.JSON (Tracer, traceWith)
import Data.EventSource (EventId, EventSink (..), EventSource (..), getEventId, putEventsToSinks)
import Data.EventSource.Rotation (EventStore (..))
import Data.Secret (mkSecret)
import Data.Text (pack)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.Server (Server, sendMessage)
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Cardano.Api (
  getCardanoPaymentVerificationKey,
 )
import Hydra.Chain (Chain (..), ChainEvent (..), ChainStateHistory (lastKnown), PostTxError, initHistory)
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic (
  Effect (..),
  HeadState (..),
  Input (..),
  Outcome (..),
  TTL,
  aggregateChainStateHistory,
  aggregateNodeState,
  aggregateState,
 )
import Hydra.HeadLogic qualified as HeadLogic
import Hydra.HeadLogic.Outcome (StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger (Ledger)
import Hydra.Network (Host (..), Network (..), NetworkCallback (..))
import Hydra.Network qualified as Network
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.InputQueue (InputQueue (..), Queued (..), createInputQueue)
import Hydra.Node.Outbox (Outbox (..), StallBounds (..), newOutbox)
import Hydra.Node.ParameterMismatch (ParamMismatch (..), ParameterMismatch (..))
import Hydra.Node.State (NodeState (..), initNodeState)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..))
import Hydra.Node.Util (readFileTextEnvelopeThrow, readSigningKey, readVerificationKey)
import Hydra.Options (CardanoChainConfig (..), ChainConfig (..), RunOptions (..), defaultContestationPeriod, defaultDepositActivation, defaultDepositPeriod)
import Hydra.Tx (HeadParameters (..), Party (..), deriveParty)
import Hydra.Tx.Utils (verificationKeyToOnChainId)

-- * Environment Handling

-- | Initialize the 'Environment' from command line options.
initEnvironment :: RunOptions -> IO Environment
initEnvironment options = do
  -- Wrap the raw key as soon as it leaves disk: every in-process holder
  -- after this point sees only a 'Secret'.
  sk <- mkSecret <$> readFileTextEnvelopeThrow hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  participants <- getParticipants
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
      , participants
      , contestationPeriod
      , depositPeriod
      , depositActivation
      , unsyncedPeriod
      , configuredPeers
      }
 where
  -- XXX: This is mostly a cardano-specific initialization step of loading
  -- --cardano-verification-key options and deriving 'OnChainId's from it. We should be able to call out to the various chain layer
  getParticipants =
    case chainConfig of
      Offline{} -> pure []
      Cardano
        CardanoChainConfig
          { cardanoVerificationKeys
          , cardanoSigningKey
          } -> do
          ownSigningKey <- readSigningKey cardanoSigningKey
          otherVerificationKeys <- mapM readVerificationKey cardanoVerificationKeys
          pure $ verificationKeyToOnChainId <$> (getCardanoPaymentVerificationKey ownSigningKey : otherVerificationKeys)

  contestationPeriod = case chainConfig of
    Offline{} -> defaultContestationPeriod
    Cardano CardanoChainConfig{contestationPeriod = cp} -> cp
  depositPeriod = case chainConfig of
    Offline{} -> defaultDepositPeriod
    Cardano CardanoChainConfig{depositPeriod = dp} -> dp
  depositActivation = case chainConfig of
    Offline{} -> defaultDepositActivation
    Cardano CardanoChainConfig{depositActivation = da} -> da
  -- In offline mode, there's no real chain to sync with, so we use a very large
  -- unsynced period to effectively disable the unsynced check.
  unsyncedPeriod = case chainConfig of
    Offline{} -> UnsyncedPeriod (fromIntegral (maxBound :: Int))
    Cardano CardanoChainConfig{unsyncedPeriod = up} -> up

  loadParty p =
    Party <$> readFileTextEnvelopeThrow p

  httpUrl (Host h p) = "http://" <> toString h <> ":" <> show p

  configuredPeers =
    pack
      $ intercalate ","
        . map (\h -> show h <> "=" <> httpUrl h)
      $ (maybeToList advertise <> peers)

  RunOptions
    { hydraSigningKey
    , hydraVerificationKeys
    , chainConfig
    , advertise
    , peers
    } = options

-- | Checks that command line options match a given 'HeadState'. This function
-- takes 'Environment' because it is derived from 'RunOptions' via
-- 'initEnvironment'.
--
-- Throws: 'ParameterMismatch' when state not matching the environment.
checkHeadState ::
  MonadThrow m =>
  Tracer m (HydraNodeLog tx) ->
  Environment ->
  HeadState tx ->
  m ()
checkHeadState tracer env headState = do
  unless (null paramsMismatch) $ do
    traceWith tracer (Misconfiguration paramsMismatch)
    throwIO $ ParameterMismatch paramsMismatch
 where
  paramsMismatch =
    maybe [] validateParameters $ getHeadParameters headState

  validateParameters HeadParameters{contestationPeriod = loadedCp, depositPeriod = loadedDp, parties} =
    execWriter $ do
      when (loadedCp /= configuredCp) $
        tell [ContestationPeriodMismatch{loadedCp, configuredCp}]

      when (loadedDp /= configuredDp) $
        tell [DepositPeriodMismatch{loadedDp, configuredDp}]

      let loadedParties = sort parties
          configuredParties = sort (party : otherParties)
      when (loadedParties /= configuredParties) $
        tell [PartiesMismatch{loadedParties, configuredParties}]

  Environment{contestationPeriod = configuredCp, depositPeriod = configuredDp, otherParties, party} = env

-- * Create and run a hydra node

-- | A draft version of the 'HydraNode' that holds state, but is not yet
-- connected (see 'connect'). This is commonly created by the 'hydrate' smart
-- constructor.
data DraftHydraNode tx m = DraftHydraNode
  { tracer :: Tracer m (HydraNodeLog tx)
  , env :: Environment
  , ledger :: Ledger tx
  , nodeStateHandler :: NodeStateHandler tx m
  , inputQueue :: InputQueue m (Input tx)
  , eventSource :: EventSource (StateEvent tx) m
  , eventSinks :: [EventSink (StateEvent tx) m]
  , networkOutbox :: Outbox m
  -- ^ Hand-off for 'NetworkEffect's, see 'Outbox'. Created here rather than
  -- in 'connect' so that the API server, which starts first, can report the
  -- live broadcast status in its 'Hydra.API.ServerOutput.Greetings'.
  , -- XXX: This is an odd field in here, but needed for the chain layer to
    -- bootstrap. Maybe move to NodeStateHandler or make it differently accessible?
    chainStateHistory :: ChainStateHistory tx
  }

-- | Hydrate a 'DraftHydraNode' by loading events from source, re-aggregate node
-- state and sending events to sinks while doing so.
hydrate ::
  (IsChainState tx, MonadDelay m, MonadLabelledSTM m, MonadAsync m, MonadThrow m, MonadUnliftIO m) =>
  Tracer m (HydraNodeLog tx) ->
  Environment ->
  Ledger tx ->
  ChainStateType tx ->
  EventStore (StateEvent tx) m ->
  [EventSink (StateEvent tx) m] ->
  m (DraftHydraNode tx m)
hydrate tracer env ledger initialChainState EventStore{eventSource, eventSink} eventSinks = do
  traceWith tracer LoadingState
  (lastEventId, (nodeState, chainStateHistory)) <-
    runConduitRes $
      sourceEvents eventSource
        .| getZipSink
          ( (,)
              <$> ZipSink (foldMapC (Last . pure . getEventId))
              <*> ZipSink recoverNodeStateC
          )
  traceWith tracer $ LoadedChainState{lastKnownChainPoint = lastKnown chainStateHistory}
  traceWith tracer $ LoadedState{lastEventId, nodeState}
  -- Check whether the loaded state matches our configuration (env)
  -- XXX: re-stream events just for this?
  checkHeadState tracer env (headState nodeState)
  -- (Re-)submit events to sinks; de-duplication is handled by the sinks
  traceWith tracer ReplayingState
  replayedEvents <- runConduitRes $ sourceEvents eventSource .| sinkList
  putEventsToSinks eventSinks replayedEvents

  nodeStateHandler <- createNodeStateHandler (getLast lastEventId) nodeState
  inputQueue <- createInputQueue
  networkOutbox <- newOutbox broadcastStallBounds "network-outbox"
  pure
    DraftHydraNode
      { tracer
      , env
      , ledger
      , nodeStateHandler
      , inputQueue
      , eventSource
      , eventSinks = eventSink : eventSinks
      , networkOutbox
      , chainStateHistory
      }
 where
  initialState = initNodeState initialChainState

  recoverNodeStateC =
    mapC stateChanged
      .| getZipSink
        ( (,)
            <$> ZipSink (foldlC aggregateNodeState initialState)
            <*> ZipSink (foldlC aggregateChainStateHistory $ initHistory initialChainState)
        )

wireChainInput :: DraftHydraNode tx m -> (ChainEvent tx -> m ())
wireChainInput node = enqueue . ChainInput
 where
  DraftHydraNode{inputQueue = InputQueue{enqueue}} = node

wireClientInput :: DraftHydraNode tx m -> (ClientInput tx -> m ())
wireClientInput node = enqueue . ClientInput
 where
  DraftHydraNode{inputQueue = InputQueue{enqueue}} = node

wireNetworkInput :: DraftHydraNode tx m -> NetworkCallback (Authenticated (Message tx)) m
wireNetworkInput node =
  NetworkCallback
    { deliver = \Authenticated{party = sender, payload = msg} ->
        enqueue $ mkNetworkInput sender msg
    , onConnectivity =
        enqueue . NetworkInput 1 . ConnectivityEvent
    }
 where
  DraftHydraNode{inputQueue = InputQueue{enqueue}} = node

-- | Create a network input with corresponding default ttl from given sender.
mkNetworkInput :: Party -> Message tx -> Input tx
mkNetworkInput sender msg =
  case msg of
    ReqTx{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    ReqDec{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    _ -> NetworkInput defaultTTL $ ReceivedMessage{sender, msg}

-- | Connect chain, network and API to a hydrated 'DraftHydraNode' to get a fully
-- connected 'HydraNode'.
connect ::
  Monad m =>
  Chain tx m ->
  Network m (Message tx) ->
  Server tx m ->
  DraftHydraNode tx m ->
  m (HydraNode tx m)
connect chain network server node =
  pure HydraNode{tracer, env, ledger, nodeStateHandler, inputQueue, eventSource, eventSinks, oc = chain, hn = network, server, networkOutbox}
 where
  DraftHydraNode{tracer, env, ledger, nodeStateHandler, inputQueue, eventSource, eventSinks, networkOutbox} = node

-- | Fully connected hydra node with everything wired in.
data HydraNode tx m = HydraNode
  { tracer :: Tracer m (HydraNodeLog tx)
  , env :: Environment
  , ledger :: Ledger tx
  , nodeStateHandler :: NodeStateHandler tx m
  , inputQueue :: InputQueue m (Input tx)
  , eventSource :: EventSource (StateEvent tx) m
  , eventSinks :: [EventSink (StateEvent tx) m]
  , oc :: Chain tx m
  , hn :: Network m (Message tx)
  , server :: Server tx m
  , networkOutbox :: Outbox m
  -- ^ Hand-off for 'NetworkEffect's, see 'Outbox'.
  }

-- | Run the network hand-off and its stall monitor concurrently with the
-- given action, stopping when any of them does. An effect throwing therefore
-- still takes the node down, as it did when effects ran on the main loop.
--
-- NOTE: it takes it down asynchronously, though, where running inline threw
-- synchronously from between two effects. The main loop is now cancelled
-- wherever it happens to be, which can be mid-'processStateChanges'. That is
-- the same class of interruption the surrounding 'withChain'/'withNetwork'
-- brackets could already deliver, but this is a new source of it.
withNetworkOutbox :: (MonadAsync m, MonadDelay m) => HydraNode tx m -> m () -> m ()
withNetworkOutbox node action =
  raceLabelled_
    ("network-outbox", runOutbox networkOutbox)
    ("hydra-node", raceLabelled_ ("broadcast-monitor", monitorBroadcast node) ("main-loop", action))
 where
  HydraNode{networkOutbox} = node

-- | Report the network hand-off stalling and recovering as 'Connectivity'
-- events, which reach the event log and from there clients already listening.
-- A client connecting mid-stall is told instead by
-- 'Hydra.API.ServerOutput.NetworkInfo', which reads the outbox live rather
-- than replaying these - past outputs are only replayed on request, and a
-- replayed stall may long since have ended.
--
-- These reports travel as network inputs, so while the node is catching up
-- 'updateCatchingUpHead' parks them and a stalled/resumed pair only reaches
-- clients once it is in sync, by which time the stall it describes may be
-- over. 'NetworkInfo' is unaffected, being read live.
--
-- Only a status seen on two consecutive polls is reported, so clients are not
-- flooded (as with the sync status, see #2749). Without that, a network
-- completing something every so often but less often than the stall period
-- flaps between the two reports forever: with completions 15s apart and a 10s
-- poll, the observed gaps cycle 9s, 4s, 14s, giving a report every ~30s
-- indefinitely.
monitorBroadcast :: (MonadDelay m, MonadSTM m) => HydraNode tx m -> m ()
monitorBroadcast HydraNode{tracer, networkOutbox, inputQueue = InputQueue{enqueue}} =
  -- Starting from "not stalled" rather than "unknown" keeps a healthy node
  -- silent: there is nothing to correct, because 'Greetings' reads the status
  -- live rather than replaying these.
  go (Just False) Nothing False
 where
  -- Polling at the outbox's own stall period bounds how late a report can be
  -- at three times that period, which is ample for something an operator
  -- reads. Floored at a second because 'StallBounds' is caller-supplied and a
  -- zero period would turn this into a busy loop that, under io-sim, stops
  -- virtual time advancing. Below a second the floor is what bounds the
  -- latency, so the "three times" only holds from one second up.
  period = max 1 . noProgressFor $ stallBounds networkOutbox

  go reported lastSeen hadBacklog = do
    threadDelay period
    stalled <- outboxStalled networkOutbox
    hasBacklog <- traceBacklog hadBacklog
    let seen = isJust stalled
    if lastSeen == Just seen && reported /= Just seen
      then do
        report $ case stalled of
          Just (stallReason, pendingBroadcasts) -> Network.BroadcastStalled{pendingBroadcasts, stallReason}
          Nothing -> Network.BroadcastResumed
        go (Just seen) (Just seen) hasBacklog
      else go reported (Just seen) hasBacklog

  -- Report what the hand-off holds while it holds anything, and once more when
  -- it empties so the metrics fall back to zero. Silent otherwise: a node
  -- whose network is keeping up would emit this forever for no reader. This
  -- is the fine-grained view the two reports above cannot give, being
  -- transitions of a debounced predicate: it distinguishes a backlog that is
  -- draining slowly from one that is not moving at all, and shows which of
  -- the two 'broadcastStallBounds' limbs is being approached.
  traceBacklog hadBacklog = do
    (pendingBroadcasts, noProgressSeconds) <- outboxBacklog networkOutbox
    let hasBacklog = pendingBroadcasts > 0
    when (hasBacklog || hadBacklog) . traceWith tracer $
      BroadcastBacklog{pendingBroadcasts, noProgressSeconds}
    pure hasBacklog

  -- NOTE: 'enqueue' blocks when the input queue is full, so this loop can
  -- stop polling - most likely when the main loop is itself wedged on the
  -- inline 'postTx', which is exactly when monitoring goes quiet. It cannot
  -- hold up the main loop or the outbox, and both the refusal gate and
  -- 'Greetings' read 'outboxStalled' live rather than through this, so only
  -- the reports and the metrics lag.
  report = enqueue . NetworkInput 1 . ConnectivityEvent

-- | When the node starts refusing the client transactions that grow the
-- outbound backlog, and when it reports the backlog to clients. Ten seconds
-- of no progress is comfortably above the etcd broadcast loop's one second
-- retry and far below any contestation period; the cap on queued messages is
-- the memory backstop.
--
-- Deliberately not operator-configurable: the useful range is narrow, nothing
-- observable would tell an operator which value to pick, and the natural
-- guess for "off" (zero) is the most aggressive setting rather than the least.
--
-- NOTE: this measures the hand-off, and the shipped 'broadcast' completes as
-- soon as the message is in the network component's own 100-slot
-- pending-broadcast queue. So during an outage the first ~100 messages still
-- complete promptly and nothing is reported; the stall only becomes visible
-- once both queues are saturated, which also puts the real in-flight bound
-- around 'maxPending' plus that queue rather than at 'maxPending'.
broadcastStallBounds :: StallBounds
broadcastStallBounds = StallBounds{noProgressFor = 10, maxPending = 1000}

runHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , MonadDelay m
  , MonadTime m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  m ()
runHydraNode node@HydraNode{tracer, networkOutbox, nodeStateHandler = NodeStateHandler{queryNodeState}} = flip finally reportDiscarded . withNetworkOutbox node $ do
  -- On startup, resume an interrupted fanout: if the node is mid-fanout
  -- ('FanoutProgress'), re-emit the next fanout step so an auto-drain whose
  -- driver crashed after its last observed chunk continues instead of stalling
  -- (mirrors the rollback re-post). A step already on chain fails harmlessly
  -- ('StalePartialFanoutTx' is silently ignored) and catch-up observations
  -- re-drive; a passive observer ('AwaitingSelection') posts nothing.
  --
  -- Taking only the effects loses nothing: re-posting is not a new decision, so
  -- 'repostFanoutStep' emits no state changes. That matters here because they
  -- could not be applied anyway - 'processStateChanges' writes to the event
  -- sinks, while the in-memory state is updated by 'processNextInput' as it
  -- computes an outcome, and this calls 'repostFanoutStep' directly rather than
  -- going through an input.
  atomically queryNodeState >>= \ns -> case headState ns of
    FanoutProgress pfs -> case HeadLogic.repostFanoutStep pfs of
      Continue{effects} -> processEffects node tracer 0 effects
      _ -> pure ()
    _ -> pure ()
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ do
    now <- getCurrentTime
    stepHydraNode now node
 where
  -- Whatever is still queued when we stop is dropped, so say how much: these
  -- messages were accepted from the head logic and never reached the network,
  -- and for a 'ReqSn' or 'AckSn' that leaves a snapshot round announced to
  -- nobody. Usually zero, but not only when the network is refusing work:
  -- 'submit' never blocks, so the main loop routinely runs a few messages
  -- ahead of the drain and stopping right then drops those too. A stall is
  -- what makes the count large.
  reportDiscarded = do
    discarded <- atomically $ pendingActions networkOutbox
    when (discarded > 0) . traceWith tracer $ DiscardedBroadcasts{discarded}

stepHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , MonadTime m
  , IsChainState tx
  ) =>
  UTCTime ->
  HydraNode tx m ->
  m ()
stepHydraNode now node = do
  i@Queued{queuedId, queuedItem} <- dequeue
  traceWith tracer $ BeginInput{by = party, inputId = queuedId, input = queuedItem}
  outcome <-
    refuseWhenStalled queuedItem >>= \case
      Just refusal -> pure refusal
      Nothing -> atomically $ processNextInput node queuedItem now
  traceWith tracer (LogicOutcome party outcome)
  case outcome of
    Continue{stateChanges, effects} -> do
      processStateChanges node stateChanges
      processEffects node tracer queuedId effects
      releaseParkedWhenSynced stateChanges
    Wait{reason, stateChanges} -> do
      processStateChanges node stateChanges
      maybeReenqueue reason i
    Error{} -> pure ()
  traceWith tracer EndInput{by = party, inputId = queuedId}
 where
  -- Refuse the client inputs that grow an outbound backlog we cannot drain,
  -- see 'growsBroadcastBacklog'. Everything else - every chain and network
  -- input, and 'Close', 'Contest' and 'Fanout' - keeps being processed while
  -- the network is down.
  refuseWhenStalled = \case
    ClientInput clientInput
      | growsBroadcastBacklog clientInput -> do
          -- Only where the input would actually have produced a broadcast.
          -- A node still catching up owes the client its drift, and one whose
          -- head is not open owes it 'CommandFailed' with the state; both are
          -- more use than a retryable "the queue is not draining", and
          -- neither would have broadcast anything.
          --
          -- NOTE: refusing before the head logic runs means a 'Decommit' that
          -- would have been rejected outright (one already in flight, or a tx
          -- the ledger refuses) gets this retryable answer instead of the
          -- definitive 'DecommitInvalid'. Telling them apart needs the very
          -- head-logic pass we are avoiding, and it is only reachable while
          -- stalled.
          wouldBroadcast <- atomically $ inOpenHeadAndSynced <$> queryNodeState
          if not wouldBroadcast
            then pure Nothing
            else
              outboxStalled networkOutbox >>= \case
                Nothing -> pure Nothing
                Just (stallReason, pendingBroadcasts) ->
                  pure . Just $
                    Continue
                      { stateChanges = []
                      , effects = [ClientEffect ServerOutput.RejectedInputBecauseBroadcastStalled{clientInput, stallReason, pendingBroadcasts}]
                      }
    _ -> pure Nothing

  maybeReenqueue reason q@Queued{queuedId, queuedItem} =
    case queuedItem of
      -- While the node is catching up it can't process any network message
      -- yet, so re-enqueuing here would just spin (and never terminate if the
      -- node stays out of sync). Park the message instead and replay it once
      -- the node is in sync (see 'releaseParkedWhenSynced'). This also avoids
      -- burning the message's retry budget during sync, which would otherwise
      -- drop e.g. ReqTx before catch-up completes.
      NetworkInput _ _
        | WaitOnNodeInSync{} <- reason -> park q
      NetworkInput ttl msg
        | ttl > 0 -> reenqueue waitDelay q{queuedItem = NetworkInput (ttl - 1) msg}
      _ -> traceWith tracer $ DroppedFromQueue{inputId = queuedId, input = queuedItem}

  -- Replay parked network inputs once the node transitions into sync, so
  -- messages received during catch-up are processed rather than lost.
  releaseParkedWhenSynced stateChanges =
    when (any isNodeSynced stateChanges) releaseParked

  isNodeSynced :: StateChanged tx -> Bool
  isNodeSynced = \case
    NodeSynced{} -> True
    _ -> False

  Environment{party} = env

  HydraNode{tracer, inputQueue = InputQueue{dequeue, reenqueue, park, releaseParked}, env, networkOutbox, nodeStateHandler = NodeStateHandler{queryNodeState}} = node

-- | Client inputs that turn into a broadcast directly, and so are the ones
-- worth refusing: the protocol's own messages cannot pile up while the
-- network is down, because 'broadcast' is self-delivering, so our own 'AckSn'
-- never comes back, the confirmed snapshot number freezes and
-- 'snapshotInFlight' caps both 'ReqSn' and 'AckSn' at one apiece.
--
-- NOTE: that cap is not airtight, and this is not a complete bound.
-- 'onOpenChainTick' emits a 'ReqSn' on a chain tick with no network input at
-- all, and 'SideLoadSnapshot' - which broadcasts nothing itself, so is not
-- listed here - clears exactly the state that cap reads. A client looping
-- 'SideLoadSnapshot' can therefore draw one further 'ReqSn' per tick. Left
-- ungated on purpose: side-loading is the documented recovery for a 'ReqSn'
-- or 'AckSn' lost from the hand-off, so refusing it while stalled would block
-- the way out. The leak is one small message per chain tick.
growsBroadcastBacklog :: ClientInput tx -> Bool
growsBroadcastBacklog = \case
  NewTx{} -> True
  Decommit{} -> True
  _ -> False

-- | Whether 'growsBroadcastBacklog' inputs would reach the code that
-- broadcasts, rather than being answered by the sync or head-state checks.
inOpenHeadAndSynced :: NodeState tx -> Bool
inOpenHeadAndSynced = \case
  NodeInSync{headState = Open{}} -> True
  _ -> False

-- | The maximum number of times to re-enqueue a network messages upon 'Wait'.
-- outcome.
defaultTTL :: TTL
defaultTTL = 6000

-- | The maximum number of times to re-enqueue 'ReqTx' and 'ReqDec' network
-- messages upon 'Wait'.
defaultTxTTL :: TTL
defaultTxTTL = 5

-- | The time to wait between re-enqueuing a 'Wait' outcome.
waitDelay :: DiffTime
waitDelay = 0.1

-- | Monadic interface around 'Hydra.Logic.update'.
processNextInput ::
  IsChainState tx =>
  HydraNode tx m ->
  Input tx ->
  UTCTime ->
  STM m (Outcome tx)
processNextInput HydraNode{nodeStateHandler, ledger, env} e now =
  modifyNodeState $ \s ->
    let outcome = HeadLogic.update env ledger now s e
     in (outcome, aggregateState s outcome)
 where
  NodeStateHandler{modifyNodeState} = nodeStateHandler

processStateChanges :: (MonadSTM m, MonadTime m) => HydraNode tx m -> [StateChanged tx] -> m ()
processStateChanges node stateChanges = do
  events <- forM stateChanges $ \stateChanged -> do
    time <- getCurrentTime
    eventId <- atomically getNextEventId
    pure StateEvent{eventId, stateChanged, time}
  putEventsToSinks eventSinks events
 where
  HydraNode
    { eventSinks
    , nodeStateHandler = NodeStateHandler{getNextEventId}
    } = node

processEffects ::
  ( MonadAsync m
  , MonadCatch m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  Word64 ->
  [Effect tx] ->
  m ()
processEffects node tracer inputId effects = do
  mapM_ processEffect $ zip effects [0 ..]
 where
  processEffect (effect, effectId) = do
    traceWith tracer $ BeginEffect party inputId effectId effect
    case effect of
      -- NOTE: 'sendMessage' is a write to an unbounded channel, so it cannot
      -- block. It also shares that channel with the server outputs this loop
      -- emits through the event sinks, so handing it off would reorder the
      -- two as clients see them.
      ClientEffect i -> sendMessage server i
      -- NOTE: from here on 'EndEffect' means the effect was accepted for
      -- execution, not that it completed.
      NetworkEffect msg -> submit networkOutbox $ broadcast hn msg
      -- NOTE: still inline, and 'postTx' can block - the direct backend waits
      -- on the tx-submission response. So a stalled chain backend still stops
      -- this loop, which is the sibling advisory on the L1 submission queue,
      -- not this one. Handing it off does not fix that either: 'Close' and
      -- 'Contest' are 'OnChainEffect's too, so they would queue behind the
      -- same stall, and the transaction would then be built against a later
      -- chain state than the input was decided on.
      OnChainEffect{postChainTx} ->
        postTx postChainTx
          `catch` \(postTxError :: PostTxError tx) ->
            enqueue . ChainInput $ PostTxError{postChainTx, postTxError, failingTx = Nothing}
    traceWith tracer $ EndEffect party inputId effectId

  HydraNode
    { hn
    , oc = Chain{postTx}
    , inputQueue = InputQueue{enqueue}
    , env = Environment{party}
    , server
    , networkOutbox
    } = node

-- ** Manage state

-- | Handle to access and modify the state in the Hydra Node.
data NodeStateHandler tx m = NodeStateHandler
  { modifyNodeState :: forall a. (NodeState tx -> (a, NodeState tx)) -> STM m a
  , queryNodeState :: STM m (NodeState tx)
  , getNextEventId :: STM m EventId
  }

-- | Initialize a new 'NodeStateHandler'.
createNodeStateHandler ::
  MonadLabelledSTM m =>
  -- | Last seen 'EventId'.
  Maybe EventId ->
  NodeState tx ->
  m (NodeStateHandler tx m)
createNodeStateHandler lastSeenEventId initialState = do
  nextEventIdV <- newLabelledTVarIO "next-event-id" $ maybe 0 (+ 1) lastSeenEventId
  ns <- newLabelledTVarIO "node-state" initialState
  pure
    NodeStateHandler
      { modifyNodeState = stateTVar ns
      , queryNodeState = readTVar ns
      , getNextEventId = do
          eventId <- readTVar nextEventIdV
          writeTVar nextEventIdV $ eventId + 1
          pure eventId
      }

-- * Logging

data HydraNodeLog tx
  = BeginInput {by :: Party, inputId :: Word64, input :: Input tx}
  | EndInput {by :: Party, inputId :: Word64}
  | BeginEffect {by :: Party, inputId :: Word64, effectId :: Word32, effect :: Effect tx}
  | EndEffect {by :: Party, inputId :: Word64, effectId :: Word32}
  | LogicOutcome {by :: Party, outcome :: Outcome tx}
  | DroppedFromQueue {inputId :: Word64, input :: Input tx}
  | LoadingState
  | LoadedState {lastEventId :: Last EventId, nodeState :: NodeState tx}
  | LoadedChainState {lastKnownChainPoint :: ChainPointType tx}
  | ReplayingState
  | Misconfiguration {misconfigurationErrors :: [ParamMismatch]}
  | -- | Outbound messages accepted from the head logic but never handed to
    -- the network, dropped because the node is stopping.
    DiscardedBroadcasts {discarded :: Natural}
  | -- | How much the outbound hand-off is holding, and for how long it has
    -- completed nothing. Emitted by 'monitorBroadcast' only while there is a
    -- backlog, plus once when one clears, so a node whose network is fine
    -- says nothing. Drives the broadcast metrics; see 'Hydra.Logging.Monitoring'.
    BroadcastBacklog {pendingBroadcasts :: Natural, noProgressSeconds :: DiffTime}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (HydraNodeLog tx)
deriving stock instance IsChainState tx => Show (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => ToJSON (HydraNodeLog tx)
