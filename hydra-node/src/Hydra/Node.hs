{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- == Node Architecture
--
-- The following [diagram (click for a full-width
-- version)](https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/images/hydra-architecture-direct.jpg)
-- represents the internal structure of the Hydra Node and the interactions
-- between its components.
--
-- ![Hydra Architecture](https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/images/hydra-architecture-direct_800x.jpg)
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
  isEmptyTQueue,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  stateTVar,
  writeTQueue,
 )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
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
  LogicError (..),
  Outcome (..),
  defaultTTL,
  emitSnapshot,
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.Ledger (IsTx, Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Options (RunOptions (..))
import Hydra.Party (Party (..), deriveParty)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

-- * Environment Handling

initEnvironment :: RunOptions -> IO Environment
initEnvironment RunOptions{hydraSigningKey, hydraVerificationKeys} = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
      }
 where
  loadParty p =
    Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) p
-- ** Create and run a hydra node

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

-- NOTE(AB): we use partial fields access here for convenience purpose, to
-- make serialisation To/From JSON straightforward
-- NOTE(AB): It's not needed to log the full events and effects both when starting
-- and ending the action, we should rather reference the event/effect processed
-- using some id when the action completest
data HydraNodeLog tx
  = ErrorHandlingEvent {by :: Party, event :: Event tx, reason :: LogicError tx}
  | BeginEvent {by :: Party, event :: Event tx}
  | EndEvent {by :: Party, event :: Event tx}
  | BeginEffect {by :: Party, effect :: Effect tx}
  | EndEffect {by :: Party, effect :: Effect tx}
  | CreatedState
  | LoadedState
  | NodeOptions {runOptions :: RunOptions}
  deriving stock (Generic)

deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => Eq (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => Show (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => ToJSON (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => FromJSON (HydraNodeLog tx)

instance (IsTx tx, IsChainState (ChainStateType tx)) => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary

createHydraNode ::
  (MonadSTM m, IsTx tx, FromJSON (ChainStateType tx)) =>
  Tracer m (HydraNodeLog tx) ->
  NodeState tx m ->
  EventQueue m (Event tx) ->
  Network m (Message tx) ->
  Ledger tx ->
  Chain tx m ->
  Server tx m ->
  Environment ->
  -- | Persistence handle to load/save head state
  Persistence (HeadState tx) m ->
  m (HydraNode tx m)
createHydraNode tracer nodeState eq hn ledger oc server env persistence = do
  -- TODO: where to load?
  hs <-
    load persistence >>= \case
      Nothing -> do
        traceWith tracer CreatedState
        pure IdleState{chainState = undefined}
      Just a -> do
        traceWith tracer LoadedState
        pure a
  pure HydraNode{eq, hn, nodeState, oc, server, ledger, env, persistence}

runHydraNode ::
  ( MonadThrow m
  , MonadCatch m
  , MonadAsync m
  , IsTx tx
  , IsChainState (ChainStateType tx)
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
  , IsTx tx
  , IsChainState (ChainStateType tx)
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
stepHydraNode tracer node = do
  e <- nextEvent eq
  traceWith tracer $ BeginEvent party e
  atomically (processNextEvent node e) >>= \case
    -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
    -- does trace and not throw!
    Error err -> traceWith tracer (ErrorHandlingEvent party e err)
    Wait _reason -> putEventAfter eq 0.1 (decreaseTTL e) >> traceWith tracer (EndEvent party e)
    NewState s effs -> do
      save s
      forM_ effs (processEffect node tracer)
      traceWith tracer (EndEvent party e)
    OnlyEffects effs ->
      forM_ effs (processEffect node tracer) >> traceWith tracer (EndEvent party e)
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
  (IsTx tx, IsChainState (ChainStateType tx)) =>
  HydraNode tx m ->
  Event tx ->
  STM m (Outcome tx)
processNextEvent HydraNode{nodeState, ledger, env} e =
  modifyHeadState $ \s ->
    case Logic.update env ledger s e of
      OnlyEffects effects -> (OnlyEffects effects, s)
      NewState s' effects ->
        let (s'', effects') = emitSnapshot env effects s'
         in (NewState s'' effects', s'')
      Error err -> (Error err, s)
      Wait reason -> (Wait reason, s)
 where
  NodeState{modifyHeadState} = nodeState

processEffect ::
  ( MonadAsync m
  , MonadCatch m
  , IsTx tx
  , IsChainState (ChainStateType tx)
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
    OnChainEffect postChainTx ->
      postTx undefined postChainTx
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

createEventQueue :: (MonadSTM m, MonadDelay m, MonadAsync m) => m (EventQueue m e)
createEventQueue = do
  numThreads <- newTVarIO (0 :: Integer)
  q <- atomically newTQueue
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
createNodeState :: MonadSTM m => HeadState tx -> m (NodeState tx m)
createNodeState initialState = do
  tv <- newTVarIO initialState
  pure
    NodeState
      { modifyHeadState = stateTVar tv
      , queryHeadState = readTVar tv
      }

-- ** Save and load files

-- | Handle to save and load files to/from disk using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
  }

newtype PersistenceException
  = PersistenceException String
  deriving (Eq, Show)

instance Exception PersistenceException

-- | Initialize persistence handle for given type 'a' at given file path.
createPersistence :: (MonadIO m, MonadThrow m) => Proxy a -> FilePath -> m (Persistence a m)
createPersistence _ fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  pure $
    Persistence
      { save = \a -> do
          writeBinaryFileDurableAtomic fp . toStrict $ Aeson.encode a
      , load =
          liftIO (doesFileExist fp) >>= \case
            False -> pure Nothing
            True -> do
              bs <- readFileBS fp
              -- XXX: This is weird and smelly
              if BS.null bs
                then pure Nothing
                else case Aeson.eitherDecodeStrict' bs of
                  Left e -> throwIO $ PersistenceException e
                  Right a -> pure $ Just a
      }
