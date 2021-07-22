{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (rawDeserialiseVerKeyDSIGN), deriveVerKeyDSIGN, rawDeserialiseSignKeyDSIGN)
import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (newTQueue, newTVarIO, readTQueue, stateTVar, writeTQueue)
import Hydra.Chain (Chain (..))
import Hydra.ClientInput (ClientInput)
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  LogicError (..),
  OnChainTx (..),
  Outcome (..),
  SnapshotStrategy (SnapshotAfterEachTx),
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.Ledger (Ledger, Party (..), Tx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Options (Options (..))
import Hydra.ServerOutput (ServerOutput)

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
      , snapshotStrategy = SnapshotAfterEachTx
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
  , sendOutput :: ServerOutput tx -> m ()
  , env :: Environment
  }

data HydraNodeLog tx
  = ErrorHandlingEvent Party (Event tx) (LogicError tx)
  | ProcessingEvent Party (Event tx)
  | ProcessedEvent Party (Event tx)
  | ProcessingEffect Party (Effect tx)
  | ProcessedEffect Party (Effect tx)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

handleClientInput :: HydraNode tx m -> ClientInput tx -> m ()
handleClientInput HydraNode{eq} = putEvent eq . ClientEvent

handleChainTx :: HydraNode tx m -> OnChainTx tx -> m ()
handleChainTx HydraNode{eq} = putEvent eq . OnChainEvent

handleMessage :: HydraNode tx m -> Message tx -> m ()
handleMessage HydraNode{eq} = putEvent eq . NetworkEvent

runHydraNode ::
  ( MonadThrow m
  , MonadAsync m
  , MonadTimer m
  , Tx tx
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
runHydraNode tracer node@HydraNode{eq, env = Environment{party}} = do
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ do
    e <- nextEvent eq
    traceWith tracer $ ProcessingEvent party e
    processNextEvent node e >>= \case
      -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
      -- does trace and not throw!
      Left err -> traceWith tracer (ErrorHandlingEvent party e err)
      Right effs -> forM_ effs (processEffect node tracer) >> traceWith tracer (ProcessedEvent party e)

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  ( Tx tx
  , MonadSTM m
  ) =>
  HydraNode tx m ->
  Event tx ->
  m (Either (LogicError tx) [Effect tx])
processNextEvent HydraNode{hh, env} e =
  atomically $
    modifyHeadState hh $ \s ->
      case Logic.update env (ledger hh) s e of
        NewState s' effects -> (Right effects, s')
        Error err -> (Left err, s)
        Wait -> (Right [Delay 0.1 e], s)

processEffect ::
  ( MonadAsync m
  , MonadTimer m
  , MonadThrow m
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc, sendOutput, eq, env = Environment{party}} tracer e = do
  traceWith tracer $ ProcessingEffect party e
  case e of
    ClientEffect i -> sendOutput i
    NetworkEffect msg -> broadcast hn msg
    OnChainEffect tx -> postTx oc tx
    Delay after event -> void . async $ threadDelay after >> putEvent eq event
  traceWith tracer $ ProcessedEffect party e
-- ** Some general event queue from which the Hydra head is "fed"

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , nextEvent :: m e
  }

createEventQueue :: MonadSTM m => m (EventQueue m e)
createEventQueue = do
  q <- atomically newTQueue
  pure
    EventQueue
      { putEvent = atomically . writeTQueue q
      , nextEvent = atomically $ readTQueue q
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
