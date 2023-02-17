{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- == Node Architecture
--
-- The following [diagram (click for a full-width
-- version)](https://raw.githubusercontent.com/input-output-hk/hydra/master/hydra-node/images/hydra-architecture-direct.jpg)
-- represents the internal structure of the Hydra Node and the interactions
-- between its components.
--
-- ![Hydra Architecture](https://raw.githubusercontent.com/input-output-hk/hydra/master/hydra-node/images/hydra-architecture-direct_800x.jpg)
--
-- __Legend__:
--
--     * Grayed boxes represent components which are not developed yet
--     * Black boxes represent components which are expected to be used as _black box_, eg. without any knowledge of their inner workings.
--     * Arrows depict the flow of data (Requests, messages, responses...)
--     * We represent some components that are not part of the Hydra node proper for legibility's sake
module Hydra.Node where

import Hydra.Prelude

import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (
  MonadLabelledSTM,
  isEmptyTQueue,
  labelTQueueIO,
  labelTVarIO,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  stateTVar,
  writeTQueue,
 )
import Hydra.API.Server (Server, sendOutput)
import Hydra.Cardano.Api (AsType (AsSigningKey, AsVerificationKey))
import Hydra.Chain (Chain (..), ChainCallback, ChainEvent (..), ChainStateType, IsChainState, PostTxError)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  Outcome (..),
  defaultTTL,
  getChainState,
  setChainState,
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.Ledger (IsTx, Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
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
  = BeginEvent {by :: Party, event :: Event tx}
  | EndEvent {by :: Party, event :: Event tx}
  | BeginEffect {by :: Party, effect :: Effect tx}
  | EndEffect {by :: Party, effect :: Effect tx}
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
  e <- nextEvent eq
  traceWith tracer $ BeginEvent party e
  outcome <- atomically (processNextEvent node e)
  traceWith tracer (LogicOutcome party outcome)
  case outcome of
    -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
    -- does trace and not throw!
    Error _ -> return ()
    Wait _reason -> putEventAfter eq 0.1 (decreaseTTL e)
    NewState s effs -> do
      save s
      forM_ effs (processEffect node tracer)
    OnlyEffects effs ->
      forM_ effs (processEffect node tracer)
  traceWith tracer (EndEvent party e)
 where
  decreaseTTL =
    \case
      NetworkEvent ttl msg -> NetworkEvent (ttl - 1) msg
      e -> e

  Environment{party} = env

  Persistence{save} = persistence

  HydraNode{persistence, eq, env} = node

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  (IsChainState tx) =>
  HydraNode tx m ->
  Event tx ->
  STM m (Outcome tx)
processNextEvent HydraNode{nodeState, ledger, env} e =
  modifyHeadState $ \s ->
    case Logic.update env ledger s e of
      OnlyEffects effects -> (OnlyEffects effects, s)
      NewState s' effects -> (NewState s' effects, s')
      Error err -> (Error err, s)
      Wait reason -> (Wait reason, s)
 where
  NodeState{modifyHeadState} = nodeState

processEffect ::
  ( MonadAsync m
  , MonadCatch m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc = Chain{postTx}, server, eq, env = Environment{party}} tracer e = do
  traceWith tracer $ BeginEffect party e
  case e of
    ClientEffect i -> sendOutput server i
    NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent defaultTTL msg)
    OnChainEffect{chainState, postChainTx} ->
      postTx chainState postChainTx
        `catch` \(postTxError :: PostTxError tx) ->
          putEvent eq $ PostTxError{postChainTx, postTxError}
  traceWith tracer $ EndEffect party e
-- ** Some general event queue from which the Hydra head is "fed"

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , putEventAfter :: NominalDiffTime -> e -> m ()
  , nextEvent :: m e
  , isEmpty :: m Bool
  }

createEventQueue ::
  ( MonadSTM m
  , MonadDelay m
  , MonadAsync m
  , MonadLabelledSTM m
  ) =>
  m (EventQueue m e)
createEventQueue = do
  numThreads <- newTVarIO (0 :: Integer)
  labelTVarIO numThreads "num-threads"
  q <- atomically newTQueue
  labelTQueueIO q "event-queue"
  pure
    EventQueue
      { putEvent =
          atomically . writeTQueue q
      , putEventAfter = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . async $ do
            threadDelay $ realToFrac delay
            atomically $ do
              modifyTVar' numThreads pred
              writeTQueue q e
      , nextEvent =
          atomically $ readTQueue q
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isEmpty' <- isEmptyTQueue q
            pure (isEmpty' && n == 0)
      }

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

chainCallback ::
  MonadSTM m =>
  NodeState tx m ->
  EventQueue m (Event tx) ->
  ChainCallback tx m
chainCallback NodeState{modifyHeadState} eq cont = do
  -- Provide chain state to continuation and update it when we get a newState
  -- NOTE: Although we do handle the chain state explictly in the 'HeadLogic',
  -- this is required as multiple transactions may be observed and the chain
  -- state shall accumulate the state changes coming with those observations.
  mEvent <- atomically . modifyHeadState $ \hs ->
    case cont $ getChainState hs of
      Nothing ->
        (Nothing, hs)
      Just ev@Observation{newChainState} ->
        (Just ev, setChainState newChainState hs)
      Just ev ->
        (Just ev, hs)
  case mEvent of
    Nothing -> pure ()
    Just chainEvent -> putEvent eq $ OnChainEvent{chainEvent}
