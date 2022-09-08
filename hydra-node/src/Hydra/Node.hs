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
import Hydra.API.Server (Server, sendOutput)
import Hydra.Cardano.Api (AsType (AsSigningKey, AsVerificationKey), deserialiseFromRawBytes)
import Hydra.Chain (Chain (..), ChainEvent, PostTxError)
import Hydra.ClientInput (ClientInput)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  LogicError (..),
  Outcome (..),
  emitSnapshot,
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.Ledger (IsTx, Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Options (RunOptions (..))
import Hydra.Party (Party (..), deriveParty)

-- * Environment Handling

initEnvironment :: RunOptions -> IO Environment
initEnvironment RunOptions{hydraSigningKey, hydraVerificationKeys} = do
  sk <- loadSigningKey hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
      }
 where
  -- TODO: use text envelopes instead of this maybe fail stuff
  loadSigningKey p =
    readFileBS p >>= maybeFail <$> deserialiseFromRawBytes (AsSigningKey AsHydraKey)

  loadParty p =
    Party <$> loadVerificationKey p

  loadVerificationKey p = do
    readFileBS p >>= maybeFail <$> deserialiseFromRawBytes (AsVerificationKey AsHydraKey)

  maybeFail = maybe (fail "could not deserialise from raw bytes") pure
-- ** Create and run a hydra node

data HydraNode tx m = HydraNode
  { eq :: EventQueue m (Event tx)
  , hn :: Network m (Message tx)
  , hh :: HydraHead tx m
  , oc :: Chain tx m
  , server :: Server tx m
  , env :: Environment
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsTx tx => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary

createHydraNode ::
  MonadSTM m =>
  EventQueue m (Event tx) ->
  Network m (Message tx) ->
  Ledger tx ->
  Chain tx m ->
  Server tx m ->
  Environment ->
  m (HydraNode tx m)
createHydraNode eq hn ledger oc server env = do
  hh <- createHydraHead IdleState ledger
  pure HydraNode{eq, hn, hh, oc, server, env}

handleClientInput :: HydraNode tx m -> ClientInput tx -> m ()
handleClientInput HydraNode{eq} = putEvent eq . ClientEvent

handleChainEvent :: HydraNode tx m -> ChainEvent tx -> m ()
handleChainEvent HydraNode{eq} = putEvent eq . OnChainEvent

handleMessage :: HydraNode tx m -> Message tx -> m ()
handleMessage HydraNode{eq} = putEvent eq . NetworkEvent

runHydraNode ::
  ( MonadThrow m
  , MonadAsync m
  , IsTx tx
  , MonadCatch m
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
  , MonadAsync m
  , IsTx tx
  , MonadCatch m
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
stepHydraNode tracer node@HydraNode{eq, env = Environment{party}} = do
  e <- nextEvent eq
  traceWith tracer $ BeginEvent party e
  atomically (processNextEvent node e) >>= \case
    -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
    -- does trace and not throw!
    Error err -> traceWith tracer (ErrorHandlingEvent party e err)
    Wait _reason -> putEventAfter eq 0.1 e >> traceWith tracer (EndEvent party e)
    NewState _ effs ->
      forM_ effs (processEffect node tracer) >> traceWith tracer (EndEvent party e)
    OnlyEffects effs ->
      forM_ effs (processEffect node tracer) >> traceWith tracer (EndEvent party e)

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  IsTx tx =>
  HydraNode tx m ->
  Event tx ->
  STM m (Outcome tx)
processNextEvent HydraNode{hh, env} e =
  modifyHeadState hh $ \s ->
    case Logic.update env (ledger hh) s e of
      OnlyEffects effects -> (OnlyEffects effects, s)
      NewState s' effects ->
        let (s'', effects') = emitSnapshot env effects s'
         in (NewState s'' effects', s'')
      Error err -> (Error err, s)
      Wait reason -> (Wait reason, s)

processEffect ::
  ( MonadAsync m
  , MonadCatch m
  , IsTx tx
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc, server, eq, env = Environment{party}} tracer e = do
  traceWith tracer $ BeginEffect party e
  case e of
    ClientEffect i -> sendOutput server i
    NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent msg)
    OnChainEffect postChainTx ->
      postTx oc postChainTx
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

-- ** HydraHead handle to manage a single hydra head state concurrently

-- | Handle to access and modify a Hydra Head's state.
data HydraHead tx m = HydraHead
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , ledger :: Ledger tx
  }

queryHeadState :: HydraHead tx m -> STM m (HeadState tx)
queryHeadState = (`modifyHeadState` \s -> (s, s))

putState :: HydraHead tx m -> HeadState tx -> STM m ()
putState HydraHead{modifyHeadState} new =
  modifyHeadState $ const ((), new)

createHydraHead :: MonadSTM m => HeadState tx -> Ledger tx -> m (HydraHead tx m)
createHydraHead initialState ledger = do
  tv <- newTVarIO initialState
  pure HydraHead{modifyHeadState = stateTVar tv, ledger}
