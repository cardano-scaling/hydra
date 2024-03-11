{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- Checkout [Hydra
-- Documentation](https://hydra.family/head-protocol/core-concepts/architecture)
-- for some details about the overall architecture of the `Node`.
module Hydra.Node where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  labelTVarIO,
  newTVarIO,
  stateTVar,
 )
import Control.Monad.Trans.Writer (execWriter, tell)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Server (Server, sendOutput)
import Hydra.Cardano.Api (AsType (AsPaymentKey, AsSigningKey, AsVerificationKey), getVerificationKey)
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  ChainStateHistory,
  ChainStateType,
  HeadParameters (..),
  IsChainState,
  PostTxError,
 )
import Hydra.Chain.Direct.Tx (verificationKeyToOnChainId)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.Events (EventSink (..), EventSource (..), putEventToSinks, putEventsToSinks)
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  HeadState (..),
  IdleState (..),
  Input (..),
  Outcome (..),
  aggregateState,
  defaultTTL,
  recoverChainStateHistory,
  recoverState,
 )
import Hydra.HeadLogic qualified as Logic
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.Ledger (Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Message (Message)
import Hydra.Node.InputQueue (InputQueue (..), Queued (..), createInputQueue)
import Hydra.Node.ParameterMismatch (ParamMismatch (..), ParameterMismatch (..))
import Hydra.Options (ChainConfig (..), DirectChainConfig (..), RunOptions (..), defaultContestationPeriod)
import Hydra.Party (Party (..), deriveParty)

-- * Environment Handling

-- | Intialize the 'Environment' from command line options.
initEnvironment :: RunOptions -> IO Environment
initEnvironment options = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  participants <- getParticipants
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
      , participants
      , contestationPeriod
      }
 where
  -- XXX: This is mostly a cardano-specific initialization step of loading
  -- --cardano-verification-key options and deriving 'OnChainId's from it. We should be able to call out to the various chain layer
  getParticipants =
    case chainConfig of
      Offline{} -> pure []
      Direct
        DirectChainConfig
          { cardanoVerificationKeys
          , cardanoSigningKey
          } -> do
          ownSigningKey <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) cardanoSigningKey
          otherVerificationKeys <- mapM (readFileTextEnvelopeThrow (AsVerificationKey AsPaymentKey)) cardanoVerificationKeys
          pure $ verificationKeyToOnChainId <$> (getVerificationKey ownSigningKey : otherVerificationKeys)

  contestationPeriod = case chainConfig of
    Offline{} -> defaultContestationPeriod
    Direct DirectChainConfig{contestationPeriod = cp} -> cp

  loadParty p =
    Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) p

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

-- | A dry version of the 'HydraNode' that does not yet hold state and is not
-- yet connected to the network.
data DryHydraNode tx m = DryHydraNode
  { tracer :: Tracer m (HydraNodeLog tx)
  , env :: Environment
  , ledger :: Ledger tx
  , initialChainState :: ChainStateType tx
  }

-- | Create a dry 'HydraNode' which needs to be 'hydrate'd next.
mkHydraNode ::
  Tracer m (HydraNodeLog tx) ->
  Environment ->
  Ledger tx ->
  ChainStateType tx ->
  DryHydraNode tx m
mkHydraNode tracer env ledger initialChainState =
  DryHydraNode{tracer, env, ledger, initialChainState}

-- | A wet version (see 'hydrate') of the 'HydraNode' that holds state and can
-- be used to 'wireChainInput', but is not yet connected (see 'connect').
data WetHydraNode tx m = WetHydraNode
  { tracer :: Tracer m (HydraNodeLog tx)
  , env :: Environment
  , ledger :: Ledger tx
  , nodeState :: NodeState tx m
  , inputQueue :: InputQueue m (Input tx)
  , eventSource :: EventSource (StateChanged tx) m
  , eventSinks :: [EventSink (StateChanged tx) m]
  }

-- | Hydrate a 'DryHydraNode' into a 'WetHydraNode' by loading events from
-- source, re-aggregate node state and sending events to sinks while doing so.
hydrate ::
  (MonadDelay m, MonadLabelledSTM m, MonadAsync m, MonadThrow m, IsChainState tx) =>
  DryHydraNode tx m ->
  EventSource (StateChanged tx) m ->
  [EventSink (StateChanged tx) m] ->
  m
    ( WetHydraNode tx m
    , ChainStateHistory tx -- TODO: Make this part of NodeState?
    )
hydrate dryNode eventSource eventSinks = do
  (hs, chainStateHistory) <- loadStateEventSource tracer eventSource eventSinks initialChainState
  checkHeadState tracer env hs
  nodeState <- createNodeState hs
  inputQueue <- createInputQueue
  let wetNode =
        WetHydraNode
          { tracer
          , env
          , ledger
          , nodeState
          , inputQueue
          , eventSource
          , eventSinks
          }
  pure (wetNode, chainStateHistory)
 where
  DryHydraNode{tracer, env, ledger, initialChainState} = dryNode

wireChainInput :: WetHydraNode tx m -> (ChainEvent tx -> m ())
wireChainInput node = enqueue . ChainInput
 where
  WetHydraNode{inputQueue = InputQueue{enqueue}} = node

wireClientInput :: WetHydraNode tx m -> (ClientInput tx -> m ())
wireClientInput node = enqueue . ClientInput
 where
  WetHydraNode{inputQueue = InputQueue{enqueue}} = node

wireNetworkInput :: WetHydraNode tx m -> (Authenticated (Message tx) -> m ())
wireNetworkInput node (Authenticated msg otherParty) =
  enqueue $ NetworkInput defaultTTL otherParty msg
 where
  WetHydraNode{inputQueue = InputQueue{enqueue}} = node

-- | Connect chain, network and API to a hydrated 'WetHydraNode' to get a fully
-- connected 'HydraNode'.
connect ::
  WetHydraNode tx m ->
  Chain tx m ->
  Network m (Message tx) ->
  Server tx m ->
  HydraNode tx m
connect node chain network server =
  HydraNode{tracer, env, ledger, nodeState, inputQueue, eventSource, eventSinks, oc = chain, hn = network, server}
 where
  WetHydraNode{tracer, env, ledger, nodeState, inputQueue, eventSource, eventSinks} = node

-- | Fully connected hydra node with everything wired in.
data HydraNode tx m = HydraNode
  { tracer :: Tracer m (HydraNodeLog tx)
  , env :: Environment
  , ledger :: Ledger tx
  , nodeState :: NodeState tx m
  , inputQueue :: InputQueue m (Input tx)
  , eventSource :: EventSource (StateChanged tx) m
  , eventSinks :: [EventSink (StateChanged tx) m]
  , oc :: Chain tx m
  , hn :: Network m (Message tx)
  , server :: Server tx m
  }

runHydraNode ::
  ( MonadCatch m
  , MonadAsync m
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
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  m ()
stepHydraNode node = do
  i@Queued{queuedId, queuedItem} <- dequeue
  traceWith tracer $ BeginInput{by = party, inputId = queuedId, input = queuedItem}
  outcome <- atomically $ do
    let nextStateChangeID = queuedId -- an event won't necessarily produce a statechange, but if it does, then this'll be its ID
    processNextInput node queuedItem nextStateChangeID
  traceWith tracer (LogicOutcome party outcome)
  case outcome of
    Continue{events, effects} -> do
      forM_ events $ putEventToSinks eventSinks
      processEffects node tracer queuedId effects
    Wait{events} -> do
      forM_ events $ putEventToSinks eventSinks
      reenqueue waitDelay (decreaseTTL i)
    Error{} -> pure ()
  traceWith tracer EndInput{by = party, inputId = queuedId}
 where
  decreaseTTL =
    \case
      -- XXX: this is smelly, handle wait re-enqueing differently
      Queued{queuedId, queuedItem = NetworkInput ttl aParty msg}
        | ttl > 0 -> Queued{queuedId, queuedItem = NetworkInput (ttl - 1) aParty msg}
      e -> e

  Environment{party} = env

  HydraNode{tracer, inputQueue = InputQueue{dequeue, reenqueue}, env, eventSinks} = node

-- | The time to wait between re-enqueuing a 'Wait' outcome from 'HeadLogic'.
waitDelay :: DiffTime
waitDelay = 0.1

-- | Monadic interface around 'Hydra.Logic.update'.
processNextInput ::
  IsChainState tx =>
  HydraNode tx m ->
  Input tx ->
  Word64 ->
  STM m (Outcome tx)
processNextInput HydraNode{nodeState, ledger, env} e nextStateChangeID =
  modifyHeadState $ \s ->
    let outcome = computeOutcome s e
     in (outcome, aggregateState s outcome)
 where
  NodeState{modifyHeadState} = nodeState

  computeOutcome = Logic.update env ledger nextStateChangeID

-- NOTE(Elaine): think you shouldnt be able to put all above into a single atomically call, the types shouldnt allow that
-- some of the sinks won't be atomic, even if the disk-based persistence is
-- and actually it doesn't matter if we accidentally do the same event twice because the PERSISTENCE is in charge of at-least-once semantics

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
      ClientEffect i -> sendOutput server i
      NetworkEffect msg -> broadcast hn msg >> enqueue (NetworkInput defaultTTL party msg)
      OnChainEffect{postChainTx} ->
        postTx postChainTx
          `catch` \(postTxError :: PostTxError tx) ->
            enqueue . ChainInput $ PostTxError{postChainTx, postTxError}
    traceWith tracer $ EndEffect party inputId effectId

  HydraNode
    { hn
    , oc = Chain{postTx}
    , server
    , inputQueue = InputQueue{enqueue}
    , env = Environment{party}
    } = node

-- ** Manage state

-- | Handle to access and modify the state in the Hydra Node.
data NodeState tx m = NodeState
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , queryHeadState :: STM m (HeadState tx)
  }

-- | Initialize a new 'NodeState'.
createNodeState :: MonadLabelledSTM m => HeadState tx -> m (NodeState tx m)
createNodeState initialState = do
  tv <- newTVarIO initialState
  labelTVarIO tv "node-state"
  pure
    NodeState
      { modifyHeadState = stateTVar tv
      , queryHeadState = readTVar tv
      }

-- | Load a 'HeadState' from persistence.
loadState ::
  (MonadThrow m, IsChainState tx) =>
  Tracer m (HydraNodeLog tx) ->
  EventSource (StateChanged tx) m ->
  ChainStateType tx ->
  m (HeadState tx, ChainStateHistory tx)
loadState tracer eventSource defaultChainState = do
  events <- getEvents eventSource
  traceWith tracer LoadedState{numberOfEvents = fromIntegral $ length events}
  let headState = recoverState initialState events
      chainStateHistory = recoverChainStateHistory defaultChainState events
  pure (headState, chainStateHistory)
 where
  initialState = Idle IdleState{chainState = defaultChainState}

loadStateEventSource ::
  (MonadThrow m, IsChainState tx) =>
  Tracer m (HydraNodeLog tx) ->
  EventSource (StateChanged tx) m ->
  [EventSink (StateChanged tx) m] ->
  ChainStateType tx ->
  m (HeadState tx, ChainStateHistory tx)
loadStateEventSource tracer eventSource eventSinks defaultChainState = do
  events <- getEvents eventSource
  traceWith tracer LoadedState{numberOfEvents = fromIntegral $ length events}
  let headState = recoverState initialState events
      chainStateHistory = recoverChainStateHistory defaultChainState events
  -- deliver to sinks per spec, deduplication is handled by the sinks
  -- FIXME(Elaine): persistence currently not handling duplication, so this relies on not providing the eventSource's sink as an arg here
  putEventsToSinks eventSinks events
  pure (headState, chainStateHistory)
 where
  initialState = Idle IdleState{chainState = defaultChainState}

-- * Logging

data HydraNodeLog tx
  = BeginInput {by :: Party, inputId :: Word64, input :: Input tx}
  | EndInput {by :: Party, inputId :: Word64}
  | BeginEffect {by :: Party, inputId :: Word64, effectId :: Word32, effect :: Effect tx}
  | EndEffect {by :: Party, inputId :: Word64, effectId :: Word32}
  | LogicOutcome {by :: Party, outcome :: Outcome tx}
  | LoadedState {numberOfEvents :: Word64}
  | Misconfiguration {misconfigurationErrors :: [ParamMismatch]}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (HydraNodeLog tx)
deriving stock instance IsChainState tx => Show (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => ToJSON (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => FromJSON (HydraNodeLog tx)

instance IsChainState tx => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink
