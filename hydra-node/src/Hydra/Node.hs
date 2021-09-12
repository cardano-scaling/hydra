{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (rawDeserialiseVerKeyDSIGN), deriveVerKeyDSIGN, rawDeserialiseSignKeyDSIGN)
import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (
  isEmptyTQueue,
  modifyTVar',
  newTQueue,
  newTVar,
  newTVarIO,
  readTQueue,
  stateTVar,
  writeTQueue,
 )
import Hydra.API.Server (Server, sendOutput)
import Hydra.Chain (Chain (..), OnChainTx)
import Hydra.ClientInput (ClientInput)
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
import Hydra.Ledger (Ledger, Tx, Utxo)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Options (Options (..))
import Hydra.Party (Party (UnsafeParty))

-- * Environment Handling

initEnvironment :: Options -> IO Environment
initEnvironment Options{me, parties} = do
  sk <- loadSigningKey me
  let vk = deriveVerKeyDSIGN sk
  otherVKeys <- mapM loadVerificationKey parties
  pure $
    Environment
      { party = UnsafeParty vk
      , signingKey = sk
      , otherParties = UnsafeParty <$> otherVKeys
      }
 where
  loadSigningKey p = do
    mKey <- rawDeserialiseSignKeyDSIGN <$> readFileBS p
    case mKey of
      Nothing -> fail $ "Failed to decode signing key from " <> p
      Just key -> pure key

  loadVerificationKey p = do
    mKey <- rawDeserialiseVerKeyDSIGN <$> readFileBS p
    case mKey of
      Nothing -> fail $ "Failed to decode verification key from " <> p
      Just key -> pure key
--

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
  | ProcessingEvent {by :: Party, event :: Event tx}
  | ProcessedEvent {by :: Party, event :: Event tx}
  | ProcessingEffect {by :: Party, effect :: Effect tx}
  | ProcessedEffect {by :: Party, effect :: Effect tx}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (Arbitrary tx, Arbitrary (Utxo tx)) => Arbitrary (HydraNodeLog tx) where
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
  hh <- createHydraHead ReadyState ledger
  pure HydraNode{eq, hn, hh, oc, server, env}

handleClientInput :: HydraNode tx m -> ClientInput tx -> m ()
handleClientInput HydraNode{eq} = putEvent eq . ClientEvent

handleChainTx :: HydraNode tx m -> OnChainTx tx -> m ()
handleChainTx HydraNode{eq} = putEvent eq . OnChainEvent

handleMessage :: HydraNode tx m -> Message tx -> m ()
handleMessage HydraNode{eq} = putEvent eq . NetworkEvent

runHydraNode ::
  ( MonadThrow m
  , MonadAsync m
  , Tx tx
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
  , Tx tx
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
stepHydraNode tracer node@HydraNode{eq, env = Environment{party}} = do
  e <- nextEvent eq
  traceWith tracer $ ProcessingEvent party e
  atomically (processNextEvent node e) >>= \case
    -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
    -- does trace and not throw!
    Left err -> traceWith tracer (ErrorHandlingEvent party e err)
    Right effs ->
      forM_ effs (processEffect node tracer) >> traceWith tracer (ProcessedEvent party e)

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  Tx tx =>
  HydraNode tx m ->
  Event tx ->
  STM m (Either (LogicError tx) [Effect tx])
processNextEvent HydraNode{hh, env} e =
  modifyHeadState hh $ \s ->
    case Logic.update env (ledger hh) s e of
      NewState s' effects ->
        let (s'', effects') = emitSnapshot env effects s'
         in (Right effects', s'')
      Error err -> (Left err, s)
      Wait -> (Right [Delay 0.1 e], s)

processEffect ::
  ( MonadAsync m
  , MonadThrow m
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc, server, eq, env = Environment{party}} tracer e = do
  traceWith tracer $ ProcessingEffect party e
  case e of
    ClientEffect i -> sendOutput server i
    NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent msg)
    OnChainEffect tx -> postTx oc tx
    Delay after event -> putEventAfter eq after event
  traceWith tracer $ ProcessedEffect party e
-- ** Some general event queue from which the Hydra head is "fed"

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , putEventAfter :: DiffTime -> e -> m ()
  , nextEvent :: m e
  , isEmpty :: m Bool
  }

createEventQueue :: (MonadSTM m, MonadDelay m, MonadAsync m) => m (EventQueue m e)
createEventQueue = do
  numThreads <- atomically (newTVar (0 :: Integer))
  q <- atomically newTQueue
  pure
    EventQueue
      { putEvent =
          atomically . writeTQueue q
      , putEventAfter = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . async $ do
            threadDelay delay
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
