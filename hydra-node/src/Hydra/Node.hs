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
import Hydra.Chain (Chain (..), ChainStateType, IsChainState, PostTxError)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  Outcome (..),
  defaultTTL,
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.Ledger (IsTx, Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node.EventQueue (EventQueue (..), Queued (..))
import Hydra.Options (ChainConfig (..), RunOptions (..))
import Hydra.Party (Party (..), deriveParty)
import Hydra.Persistence (Persistence (..))

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
  , persistence :: Persistence (HeadState tx) m
  }

data HydraNodeLog tx
  = BeginEvent {by :: Party, eventId :: Word64, event :: Event tx}
  | EndEvent {by :: Party, eventId :: Word64}
  | BeginEffect {by :: Party, eventId :: Word64, effectId :: Word32, effect :: Effect tx}
  | EndEffect {by :: Party, eventId :: Word64, effectId :: Word32}
  | LogicOutcome {by :: Party, outcome :: Outcome tx}
  deriving stock (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState tx) => Show (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState tx) => ToJSON (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState tx) => FromJSON (HydraNodeLog tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary

runHydraNode ::
  ( MonadThrow m
  , MonadCatch m
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
  ( MonadThrow m
  , MonadCatch m
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
  effs <- handleOutcome e outcome
  mapM_ (uncurry $ flip $ processEffect node tracer) $ zip effs (map (eventId,) [0 ..])
  traceWith tracer EndEvent{by = party, eventId}
 where
  handleOutcome e = \case
    -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
    -- does trace and not throw!
    Error _ -> do
      pure []
    Wait _reason -> do
      putEventAfter eq waitDelay (decreaseTTL e)
      pure []
    NewState s -> do
      save s
      pure []
    Effects effs -> do
      pure effs
    Combined l r -> do
      effl <- handleOutcome e l
      effr <- handleOutcome e r
      pure $ effl <> effr

  decreaseTTL =
    \case
      -- XXX: this is smelly, handle wait re-enqueing differently
      Queued{eventId, queuedEvent = NetworkEvent ttl msg}
        | ttl > 0 -> Queued{eventId, queuedEvent = NetworkEvent (ttl - 1) msg}
      e -> e

  Environment{party} = env

  Persistence{save} = persistence

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
    handleOutcome s $ Logic.update env ledger s e
 where
  NodeState{modifyHeadState} = nodeState

  handleOutcome s = \case
    Effects effects -> (Effects effects, s)
    NewState s' -> (NewState s', s')
    Error err -> (Error err, s)
    Wait reason -> (Wait reason, s)
    Combined l r ->
      let (leftOutcome, leftState) = handleOutcome s l
          (rightOutcome, rightState) = handleOutcome leftState r
       in (Combined leftOutcome rightOutcome, rightState)

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
    NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent defaultTTL msg)
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
createNodeState :: (MonadSTM m, MonadLabelledSTM m) => HeadState tx -> m (NodeState tx m)
createNodeState initialState = do
  tv <- newTVarIO initialState
  labelTVarIO tv "node-state"
  pure
    NodeState
      { modifyHeadState = stateTVar tv
      , queryHeadState = readTVar tv
      }
