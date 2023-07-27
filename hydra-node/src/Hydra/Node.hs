{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Hydra.API.Server (Server, sendOutput)
import Hydra.Cardano.Api (AsType (AsSigningKey, AsVerificationKey))
import Hydra.Chain (Chain (..), ChainStateType, HeadParameters (..), IsChainState, PostTxError)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  IdleState (IdleState),
  Outcome (..),
  aggregateState,
  collectEffects,
  defaultTTL,
  recoverState,
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.Ledger (IsTx, Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node.EventQueue (EventQueue (..), Queued (..))
import Hydra.Options (ChainConfig (..), ParamMismatch (..), RunOptions (..))
import Hydra.Party (Party (..), deriveParty)
import Hydra.Persistence (PersistenceIncremental (..), loadAll)

-- * Environment Handling

initEnvironment :: RunOptions -> IO Environment
initEnvironment RunOptions{hydraSigningKey, hydraVerificationKeys, chainConfig = DirectChainConfig{contestationPeriod}} = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
      , contestationPeriod
      }
 where
  loadParty p =
    Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) p

-- ** Create and run a hydra node

-- | Main handle of a hydra node where all layers are tied together.
data HydraNode tx m = HydraNode
  { eq :: EventQueue m (Event tx)
  , hn :: Network m (Message tx)
  , nodeState :: NodeState tx m
  , oc :: Chain tx m
  , server :: Server tx m
  , ledger :: Ledger tx
  , env :: Environment
  , persistence :: PersistenceIncremental (StateChanged tx) m
  }

data HydraNodeLog tx
  = BeginEvent {by :: Party, eventId :: Word64, event :: Event tx}
  | EndEvent {by :: Party, eventId :: Word64}
  | BeginEffect {by :: Party, eventId :: Word64, effectId :: Word32, effect :: Effect tx}
  | EndEffect {by :: Party, eventId :: Word64, effectId :: Word32}
  | LogicOutcome {by :: Party, outcome :: Outcome tx}
  | CreatedState
  | LoadedState
  | Misconfiguration {misconfigurationErrors :: [ParamMismatch]}
  deriving stock (Generic)

deriving instance (IsChainState tx) => Eq (HydraNodeLog tx)
deriving instance (IsChainState tx) => Show (HydraNodeLog tx)
deriving instance (IsChainState tx) => ToJSON (HydraNodeLog tx)
deriving instance (IsChainState tx) => FromJSON (HydraNodeLog tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary

runHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , IsChainState tx
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
runHydraNode tracer node =
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ stepHydraNode tracer node

stepHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , IsChainState tx
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
stepHydraNode tracer node = do
  e@Queued{eventId, queuedEvent} <- nextEvent eq
  traceWith tracer $ BeginEvent{by = party, eventId, event = queuedEvent}
  outcome <- atomically (processNextEvent node queuedEvent)
  traceWith tracer (LogicOutcome party outcome)
  handleOutcome e outcome
  let effs = collectEffects outcome
  mapM_ (uncurry $ flip $ processEffect node tracer) $ zip effs (map (eventId,) [0 ..])
  traceWith tracer EndEvent{by = party, eventId}
 where
  handleOutcome e = \case
    Error _ -> pure ()
    Wait _reason -> putEventAfter eq waitDelay (decreaseTTL e)
    StateChanged sc -> append sc
    Effects _ -> pure ()
    Combined l r -> handleOutcome e l >> handleOutcome e r

  decreaseTTL =
    \case
      -- XXX: this is smelly, handle wait re-enqueing differently
      Queued{eventId, queuedEvent = NetworkEvent ttl aParty msg}
        | ttl > 0 -> Queued{eventId, queuedEvent = NetworkEvent (ttl - 1) aParty msg}
      e -> e

  Environment{party} = env

  PersistenceIncremental{append} = persistence

  HydraNode{persistence, eq, env} = node

-- | The time to wait between re-enqueuing a 'Wait' outcome from 'HeadLogic'.
waitDelay :: DiffTime
waitDelay = 0.1

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  (IsChainState tx) =>
  HydraNode tx m ->
  Event tx ->
  STM m (Outcome tx)
processNextEvent HydraNode{nodeState, ledger, env} e =
  modifyHeadState $ \s ->
    let outcome = Logic.update env ledger s e
     in (outcome, aggregateState s outcome)
 where
  NodeState{modifyHeadState} = nodeState

processEffect ::
  ( MonadAsync m
  , MonadCatch m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  (Word64, Word32) ->
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc = Chain{postTx}, server, eq, env = Environment{party}} tracer (eventId, effectId) e = do
  traceWith tracer $ BeginEffect party eventId effectId e
  case e of
    ClientEffect i -> sendOutput server i
    NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent defaultTTL party msg)
    OnChainEffect{postChainTx} ->
      postTx postChainTx
        `catch` \(postTxError :: PostTxError tx) ->
          putEvent eq $ PostTxError{postChainTx, postTxError}
  traceWith tracer $ EndEffect party eventId effectId

-- ** Manage state

-- | Handle to access and modify the state in the Hydra Node.
data NodeState tx m = NodeState
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , queryHeadState :: STM m (HeadState tx)
  }

-- | Initialize a new 'NodeState'.
createNodeState :: (MonadLabelledSTM m) => HeadState tx -> m (NodeState tx m)
createNodeState initialState = do
  tv <- newTVarIO initialState
  labelTVarIO tv "node-state"
  pure
    NodeState
      { modifyHeadState = stateTVar tv
      , queryHeadState = readTVar tv
      }

newtype ParamMismatchError = ParamMismatchError [ParamMismatch] deriving (Eq, Show)

instance Exception ParamMismatchError

-- | Load state from persistence.
loadState ::
  (MonadThrow m, IsChainState tx) =>
  Tracer m (HydraNodeLog tx) ->
  PersistenceIncremental (StateChanged tx) m ->
  ChainStateType tx ->
  m (HeadState tx)
loadState tracer persistence defaultChainState =
  loadAll persistence >>= \case
    [] -> do
      -- TODO: better logs
      traceWith tracer CreatedState
      pure initialState
    events -> do
      -- TODO: better logs
      traceWith tracer LoadedState
      pure $ recoverState initialState events
 where
  initialState = Idle IdleState{chainState = defaultChainState}

-- XXX: parse don't validate
checkHeadState ::
  MonadThrow m =>
  Tracer m (HydraNodeLog tx) ->
  Environment ->
  HeadState tx ->
  m ()
checkHeadState tracer env headState = do
  let paramsMismatch = checkParamsAgainstExistingState headState
  unless (null paramsMismatch) $ do
    traceWith tracer (Misconfiguration paramsMismatch)
    throwIO $ ParamMismatchError paramsMismatch
 where
  -- check if hydra-node parameters are matching with the hydra-node state.
  checkParamsAgainstExistingState :: HeadState tx -> [ParamMismatch]
  checkParamsAgainstExistingState = maybe [] validateParameters . getHeadParameters

  validateParameters HeadParameters{contestationPeriod = loadedCp, parties} =
    flip execState [] $ do
      when (loadedCp /= configuredCp) $
        modify (<> [ContestationPeriodMismatch{loadedCp, configuredCp}])
      when (loadedParties /= configuredParties) $
        modify (<> [PartiesMismatch{loadedParties, configuredParties}])
   where
    loadedParties = sort parties

  configuredParties = sort (party : otherParties)

  Environment{contestationPeriod = configuredCp, otherParties, party} = env
