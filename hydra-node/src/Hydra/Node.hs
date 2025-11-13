{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- Checkout [Hydra
-- Documentation](https://hydra.family/head-protocol/docs/dev/architecture)
-- for some details about the overall architecture of the `Node`.
module Hydra.Node where

import Hydra.Prelude

import Conduit (MonadUnliftIO, ZipSink (..), foldMapC, foldlC, mapC, mapM_C, runConduitRes, (.|))
import Control.Concurrent.Class.MonadSTM (
  stateTVar,
  writeTVar,
 )
import Control.Monad.Trans.Writer (execWriter, tell)
import Data.Text (pack)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Server (Server, sendMessage)
import Hydra.Cardano.Api (
  getVerificationKey,
 )
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  ChainStateHistory,
  PostTxError,
  initHistory,
 )
import Hydra.Chain.ChainState (ChainStateType, IsChainState)
import Hydra.Events (EventId, EventSink (..), EventSource (..), getEventId, putEventsToSinks)
import Hydra.Events.Rotation (EventStore (..))
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
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger (Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (DeliverResult (..), Host (..), Network (..), NetworkCallback (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.InputQueue (InputQueue (..), Queued (..), createInputQueue)
import Hydra.Node.ParameterMismatch (ParamMismatch (..), ParameterMismatch (..))
import Hydra.Node.State (NodeState (..), initNodeState)
import Hydra.Node.Util (readFileTextEnvelopeThrow)
import Hydra.Options (CardanoChainConfig (..), ChainConfig (..), RunOptions (..), defaultContestationPeriod, defaultDepositPeriod)
import Hydra.Tx (HasParty (..), HeadParameters (..), Party (..), deriveParty)
import Hydra.Tx.Utils (verificationKeyToOnChainId)

-- * Environment Handling

-- | Initialize the 'Environment' from command line options.
initEnvironment :: RunOptions -> IO Environment
initEnvironment options = do
  sk <- readFileTextEnvelopeThrow hydraSigningKey
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
          ownSigningKey <- readFileTextEnvelopeThrow cardanoSigningKey
          otherVerificationKeys <- mapM readFileTextEnvelopeThrow cardanoVerificationKeys
          pure $ verificationKeyToOnChainId <$> (getVerificationKey ownSigningKey : otherVerificationKeys)

  contestationPeriod = case chainConfig of
    Offline{} -> defaultContestationPeriod
    Cardano CardanoChainConfig{contestationPeriod = cp} -> cp
  depositPeriod = case chainConfig of
    Offline{} -> defaultDepositPeriod
    Cardano CardanoChainConfig{depositPeriod = dp} -> dp

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

  validateParameters HeadParameters{contestationPeriod = loadedCp, parties} =
    execWriter $ do
      when (loadedCp /= configuredCp) $
        tell [ContestationPeriodMismatch{loadedCp, configuredCp}]

      let loadedParties = sort parties
          configuredParties = sort (party : otherParties)
      when (loadedParties /= configuredParties) $
        tell [PartiesMismatch{loadedParties, configuredParties}]

  Environment{contestationPeriod = configuredCp, otherParties, party} = env

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
  , -- XXX: This is an odd field in here, but needed for the chain layer to
    -- bootstrap. Maybe move to NodeStateHandler or make it differently accessible?
    chainStateHistory :: ChainStateHistory tx
  }

instance HasParty (DraftHydraNode tx m) where
  getParty DraftHydraNode{env} = getParty env

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
  traceWith tracer $ LoadedState{lastEventId, nodeState}
  -- Check whether the loaded state matches our configuration (env)
  -- XXX: re-stream events just for this?
  checkHeadState tracer env (headState nodeState)
  -- (Re-)submit events to sinks; de-duplication is handled by the sinks
  traceWith tracer ReplayingState
  runConduitRes $
    sourceEvents eventSource .| mapM_C (\e -> lift $ putEventsToSinks eventSinks [e])

  nodeStateHandler <- createNodeStateHandler (getLast lastEventId) nodeState
  inputQueue <- createInputQueue
  pure
    DraftHydraNode
      { tracer
      , env
      , ledger
      , nodeStateHandler
      , inputQueue
      , eventSource
      , eventSinks = eventSink : eventSinks
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

wireNetworkInput ::
  (MonadCatch m, MonadAsync m, MonadTime m, IsChainState tx) =>
  HydraNode tx m ->
  NetworkCallback (Authenticated (Message tx)) m
wireNetworkInput node =
  NetworkCallback
    { deliver = \Authenticated{party = sender, payload = msg} -> do
        let input = mkNetworkInput sender msg
        -- FIXME: not make up queuedId
        processInput node (0, input) >>= \case
          Continue{} -> pure Delivered
          Wait{} -> pure NotDelivered
          Error{} -> pure NotDelivered
    , onConnectivity = \conn ->
        -- FIXME: not make up queuedId
        void $ processInput node (0, NetworkInput 1 $ ConnectivityEvent conn)
    }

-- | Create a network input with corresponding default ttl from given sender.
mkNetworkInput :: Party -> Message tx -> Input tx
mkNetworkInput sender msg =
  case msg of
    ReqTx{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    ReqDec{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    _ -> NetworkInput defaultTTL $ ReceivedMessage{sender, msg}

-- | Connect chain, network and API to a hydrated 'DraftHydraNode' to get a fully
-- connected 'HydraNode'.
--
-- TODO: make this non-monadic (or drop it entirely)
connect ::
  Monad m =>
  Chain tx m ->
  Network m (Message tx) ->
  Server tx m ->
  DraftHydraNode tx m ->
  m (HydraNode tx m)
connect chain network server node =
  pure HydraNode{tracer, env, ledger, nodeStateHandler, inputQueue, eventSource, eventSinks, oc = chain, hn = network, server}
 where
  DraftHydraNode{tracer, env, ledger, nodeStateHandler, inputQueue, eventSource, eventSinks} = node

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
  }

runHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , MonadTime m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  m ()
runHydraNode node =
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ stepHydraNode node

stepHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , MonadTime m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  m ()
stepHydraNode node = do
  i@Queued{queuedId, queuedItem} <- dequeue
  traceWith tracer $ BeginInput{by = party, inputId = queuedId, input = queuedItem}
  processInput node (queuedId, queuedItem) >>= \case
    Wait{} ->
      maybeReenqueue i
    _ -> pure ()
  traceWith tracer EndInput{by = party, inputId = queuedId}
 where
  maybeReenqueue q@Queued{queuedId, queuedItem} =
    case queuedItem of
      NetworkInput ttl msg
        | ttl > 0 -> reenqueue waitDelay q{queuedItem = NetworkInput (ttl - 1) msg}
      _ -> traceWith tracer $ DroppedFromQueue{inputId = queuedId, input = queuedItem}

  Environment{party} = env

  HydraNode{tracer, inputQueue = InputQueue{dequeue, reenqueue}, env} = node

processInput ::
  ( MonadCatch m
  , MonadAsync m
  , MonadTime m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  (Word64, Input tx) ->
  m (Outcome tx)
processInput node (queuedId, input) = do
  -- TODO: trace on this level / drop queuedId?
  outcome <- atomically $ modifyNodeState $ \s ->
    let outcome = HeadLogic.update env ledger s input
     in (outcome, aggregateState s outcome)
  traceWith tracer (LogicOutcome party outcome)
  case outcome of
    Continue{stateChanges, effects} -> do
      processStateChanges node stateChanges
      processEffects node tracer queuedId effects
    Wait{stateChanges} -> do
      processStateChanges node stateChanges
    Error{} -> pure ()
  pure outcome
 where
  Environment{party} = env

  HydraNode{tracer, env, ledger, nodeStateHandler} = node

  NodeStateHandler{modifyNodeState} = nodeStateHandler

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
      ClientEffect i -> sendMessage server i
      NetworkEffect msg -> broadcast hn msg
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
  | ReplayingState
  | Misconfiguration {misconfigurationErrors :: [ParamMismatch]}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (HydraNodeLog tx)
deriving stock instance IsChainState tx => Show (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => ToJSON (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => FromJSON (HydraNodeLog tx)
