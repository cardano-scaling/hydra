{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- Checkout [Hydra
-- Documentation](https://hydra.family/head-protocol/dev/architecture)
-- for some details about the overall architecture of the `Node`.
module Hydra.Node where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Api.Consensus (EraMismatch (..))
import Conduit (MonadUnliftIO, ZipSink (..), foldMapC, foldlC, mapC, mapM_C, runConduitRes, (.|))
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  labelTVarIO,
  newTVarIO,
  stateTVar,
  writeTVar,
 )
import Control.Monad.Trans.Writer (execWriter, tell)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Server (Server, sendMessage)
import Hydra.Cardano.Api (
  Address,
  AnyCardanoEra (AnyCardanoEra),
  AsType (AsPaymentKey, AsSigningKey, AsVerificationKey),
  CardanoEra (..),
  ChainPoint,
  EraHistory,
  GenesisParameters,
  LedgerEra,
  NetworkId,
  PParams,
  QueryInShelleyBasedEra (QueryProtocolParameters),
  ShelleyAddr,
  ShelleyEra,
  SystemStart (..),
  TxId,
  UTxO,
  getVerificationKey,
  shelleyBasedEra,
 )
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  ChainStateHistory,
  PostTxError,
  initHistory,
 )
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.Blockfrost.TimeHandle qualified as Blockfrost
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Chain.ChainState (ChainStateType, IsChainState)
import Hydra.Chain.Direct.TimeHandle qualified as TimeHandle
import Hydra.Chain.ScriptRegistry qualified as ScriptRegistry
import Hydra.Events (EventId, EventSink (..), EventSource (..), StateEvent (..), getEventId, putEventsToSinks, stateChanged)
import Hydra.HeadLogic (
  Effect (..),
  HeadState (..),
  IdleState (..),
  Input (..),
  Outcome (..),
  aggregate,
  aggregateChainStateHistory,
  aggregateState,
  defaultTTL,
 )
import Hydra.HeadLogic qualified as HeadLogic
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.Ledger (Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..), NetworkCallback (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Message (Message, NetworkEvent (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.InputQueue (InputQueue (..), Queued (..), createInputQueue)
import Hydra.Node.ParameterMismatch (ParamMismatch (..), ParameterMismatch (..))
import Hydra.Node.Util (readFileTextEnvelopeThrow)
import Hydra.Options
    ( ChainBackend(..),
      ChainConfig(..),
      RunOptions(..),
      defaultContestationPeriod,
      defaultDepositDeadline,
      CardanoChainConfig(..),
      ChainBackend(..),
      ChainConfig(..),
      RunOptions(..),
      defaultContestationPeriod,
      defaultDepositDeadline )
import Hydra.Tx (HasParty (..), HeadParameters (..), Party (..), ScriptRegistry, deriveParty)
import Hydra.Tx.Crypto (AsType (AsHydraKey))
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.Utils (verificationKeyToOnChainId)

-- * Environment Handling

-- | Intialize the 'Environment' from command line options.
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
      Direct{} -> undefined

  contestationPeriod = case chainConfig of
    Offline{} -> defaultContestationPeriod
    Cardano CardanoChainConfig{contestationPeriod = cp} -> cp
  depositDeadline = case chainConfig of
    Offline{} -> defaultDepositDeadline
    Cardano CardanoChainConfig{depositDeadline = ddeadline} -> ddeadline

  loadParty p =
    Party <$> readFileTextEnvelopeThrow p

  RunOptions
    { hydraSigningKey
    , hydraVerificationKeys
    , chainConfig
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
  , nodeState :: NodeState tx m
  , inputQueue :: InputQueue m (Input tx)
  , eventSource :: EventSource (StateEvent tx) m
  , eventSinks :: [EventSink (StateEvent tx) m]
  , -- XXX: This is an odd field in here, but needed for the chain layer to
    -- bootstrap. Maybe move to NodeState or make it differently accessible?
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
  EventSource (StateEvent tx) m ->
  [EventSink (StateEvent tx) m] ->
  m (DraftHydraNode tx m)
hydrate tracer env ledger initialChainState eventSource eventSinks = do
  traceWith tracer LoadingState
  (lastEventId, (headState, chainStateHistory)) <-
    runConduitRes $
      sourceEvents eventSource
        .| getZipSink
          ( (,)
              <$> ZipSink (foldMapC (Last . pure . getEventId))
              <*> ZipSink recoverHeadStateC
          )
  traceWith tracer $ LoadedState{lastEventId, headState}
  -- Check whether the loaded state matches our configuration (env)
  -- XXX: re-stream events just for this?
  checkHeadState tracer env headState
  -- (Re-)submit events to sinks; de-duplication is handled by the sinks
  traceWith tracer ReplayingState
  runConduitRes $
    sourceEvents eventSource .| mapM_C (\e -> lift $ putEventsToSinks eventSinks [e])

  nodeState <- createNodeState (getLast lastEventId) headState
  inputQueue <- createInputQueue
  pure
    DraftHydraNode
      { tracer
      , env
      , ledger
      , nodeState
      , inputQueue
      , eventSource
      , eventSinks
      , chainStateHistory
      }
 where
  initialState = Idle IdleState{chainState = initialChainState}

  recoverHeadStateC =
    mapC stateChanged
      .| getZipSink
        ( (,)
            <$> ZipSink (foldlC aggregate initialState)
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
    { deliver = \Authenticated{payload, party} ->
        enqueue $ NetworkInput defaultTTL $ ReceivedMessage{sender = party, msg = payload}
    , onConnectivity =
        enqueue . NetworkInput defaultTTL . ConnectivityEvent
    }
 where
  DraftHydraNode{inputQueue = InputQueue{enqueue}} = node

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
  pure HydraNode{tracer, env, ledger, nodeState, inputQueue, eventSource, eventSinks, oc = chain, hn = network, server}
 where
  DraftHydraNode{tracer, env, ledger, nodeState, inputQueue, eventSource, eventSinks} = node

-- | Fully connected hydra node with everything wired in.
data HydraNode tx m = HydraNode
  { tracer :: Tracer m (HydraNodeLog tx)
  , env :: Environment
  , ledger :: Ledger tx
  , nodeState :: NodeState tx m
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
  outcome <- atomically $ processNextInput node queuedItem
  traceWith tracer (LogicOutcome party outcome)
  case outcome of
    Continue{stateChanges, effects} -> do
      processStateChanges node stateChanges
      processEffects node tracer queuedId effects
    Wait{stateChanges} -> do
      processStateChanges node stateChanges
      maybeReenqueue i
    Error{} -> pure ()
  traceWith tracer EndInput{by = party, inputId = queuedId}
 where
  maybeReenqueue q@Queued{queuedId, queuedItem} =
    case queuedItem of
      NetworkInput ttl msg
        | ttl > 0 -> reenqueue waitDelay q{queuedItem = NetworkInput (ttl - 1) msg}
      _ -> traceWith tracer $ DroppedFromQueue{inputId = queuedId, input = queuedItem}

  Environment{party} = env

  HydraNode{tracer, inputQueue = InputQueue{dequeue, reenqueue}, env} = node

-- | The time to wait between re-enqueuing a 'Wait' outcome from 'HeadLogic'.
waitDelay :: DiffTime
waitDelay = 0.1

-- | Monadic interface around 'Hydra.Logic.update'.
processNextInput ::
  IsChainState tx =>
  HydraNode tx m ->
  Input tx ->
  STM m (Outcome tx)
processNextInput HydraNode{nodeState, ledger, env} e =
  modifyHeadState $ \s ->
    let outcome = computeOutcome s e
     in (outcome, aggregateState s outcome)
 where
  NodeState{modifyHeadState} = nodeState

  computeOutcome = HeadLogic.update env ledger

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
    , nodeState = NodeState{getNextEventId}
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
            enqueue . ChainInput $ PostTxError{postChainTx, postTxError}
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
data NodeState tx m = NodeState
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , queryHeadState :: STM m (HeadState tx)
  , getNextEventId :: STM m EventId
  }

-- | Initialize a new 'NodeState'.
createNodeState ::
  MonadLabelledSTM m =>
  -- | Last seen 'EventId'.
  Maybe EventId ->
  HeadState tx ->
  m (NodeState tx m)
createNodeState lastSeenEventId initialState = do
  nextEventIdV <- newTVarIO $ maybe 0 (+ 1) lastSeenEventId
  labelTVarIO nextEventIdV "next-event-id"
  hs <- newTVarIO initialState
  labelTVarIO hs "head-state"
  pure
    NodeState
      { modifyHeadState = stateTVar hs
      , queryHeadState = readTVar hs
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
  | LoadedState {lastEventId :: Last EventId, headState :: HeadState tx}
  | ReplayingState
  | Misconfiguration {misconfigurationErrors :: [ParamMismatch]}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (HydraNodeLog tx)
deriving stock instance IsChainState tx => Show (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => ToJSON (HydraNodeLog tx)

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink

class BackendOps a where
  queryGenesisParameters :: (MonadIO m, MonadThrow m) => a -> m (GenesisParameters ShelleyEra)
  queryScriptRegistry :: (MonadIO m, MonadThrow m) => a -> [TxId] -> m ScriptRegistry
  queryNetworkId :: (MonadIO m, MonadThrow m) => a -> m NetworkId
  queryTip :: (MonadIO m, MonadThrow m) => a -> m ChainPoint
  queryUTxO :: (MonadIO m, MonadThrow m) => a -> [Address ShelleyAddr] -> m UTxO
  queryEraHistory :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m EraHistory
  querySystemStart :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m SystemStart
  queryProtocolParameters :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m (PParams LedgerEra)
  queryTimeHandle :: (MonadIO m, MonadThrow m) => a -> m TimeHandle.TimeHandle

-- TODO: Perhaps use Reader monad for fetching configuration?
-- we could also use a carrier type and define this instance on this type instead
instance BackendOps ChainBackend where
  queryGenesisParameters = \case
    DirectBackend{networkId, nodeSocket} ->
      liftIO $ CardanoClient.queryGenesisParameters networkId nodeSocket CardanoClient.QueryTip
    BlockfrostBackend{projectPath} -> do
      prj <- liftIO $ Blockfrost.projectFromFile projectPath
      Blockfrost.toCardanoGenesisParameters <$> Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
  queryScriptRegistry backend txIds =
    case backend of
      DirectBackend{networkId, nodeSocket} ->
        ScriptRegistry.queryScriptRegistry networkId nodeSocket txIds
      BlockfrostBackend{projectPath} -> do
        prj <- liftIO $ Blockfrost.projectFromFile projectPath
        Blockfrost.runBlockfrostM prj $ Blockfrost.queryScriptRegistry txIds
  queryNetworkId = \case
    DirectBackend{networkId} -> pure networkId
    BlockfrostBackend{projectPath} -> do
      prj <- liftIO $ Blockfrost.projectFromFile projectPath
      -- TODO: This calls to queryGenesisParameters again, but we only need the network magic
      Blockfrost.Genesis{_genesisNetworkMagic} <- Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
      pure $ Blockfrost.toCardanoNetworkId _genesisNetworkMagic
  queryTip = \case
    DirectBackend{networkId, nodeSocket} ->
      liftIO $ CardanoClient.queryTip networkId nodeSocket
    BlockfrostBackend{projectPath} -> do
      prj <- liftIO $ Blockfrost.projectFromFile projectPath
      Blockfrost.runBlockfrostM prj Blockfrost.queryTip
  queryUTxO backend addresses =
    case backend of
      DirectBackend{networkId, nodeSocket} ->
        liftIO $ CardanoClient.queryUTxO networkId nodeSocket CardanoClient.QueryTip addresses
      BlockfrostBackend{projectPath} -> do
        prj <- liftIO $ Blockfrost.projectFromFile projectPath
        Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxO addresses
  queryEraHistory backend queryPoint =
    case backend of
      DirectBackend{networkId, nodeSocket} ->
        liftIO $ CardanoClient.queryEraHistory networkId nodeSocket queryPoint
      BlockfrostBackend{projectPath} -> do
        prj <- liftIO $ Blockfrost.projectFromFile projectPath
        Blockfrost.runBlockfrostM prj Blockfrost.queryEraHistory
  querySystemStart backend queryPoint =
    case backend of
      DirectBackend{networkId, nodeSocket} ->
        liftIO $ CardanoClient.querySystemStart networkId nodeSocket queryPoint
      BlockfrostBackend{projectPath} -> do
        prj <- liftIO $ Blockfrost.projectFromFile projectPath
        Blockfrost.Genesis{_genesisSystemStart} <- Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
        pure $ SystemStart $ posixSecondsToUTCTime _genesisSystemStart
  queryProtocolParameters backend queryPoint =
    case backend of
      DirectBackend{networkId, nodeSocket} ->
        liftIO $ CardanoClient.runQueryExpr networkId nodeSocket queryPoint $ do
          AnyCardanoEra era <- CardanoClient.queryCurrentEraExpr
          case era of
            ConwayEra{} -> CardanoClient.queryInShelleyBasedEraExpr shelleyBasedEra QueryProtocolParameters
            _ -> liftIO . throwIO $ CardanoClient.QueryEraMismatchException EraMismatch{ledgerEraName = show era, otherEraName = "Conway"}
      BlockfrostBackend{projectPath} -> do
        prj <- liftIO $ Blockfrost.projectFromFile projectPath
        Blockfrost.runBlockfrostM prj Blockfrost.queryProtocolParameters
  queryTimeHandle = \case
    DirectBackend{networkId, nodeSocket} ->
      liftIO $ TimeHandle.queryTimeHandle networkId nodeSocket
    BlockfrostBackend{projectPath} -> do
      prj <- liftIO $ Blockfrost.projectFromFile projectPath
      Blockfrost.runBlockfrostM prj Blockfrost.queryTimeHandle
