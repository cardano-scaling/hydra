{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Cardano.Prelude hiding (STM, async, atomically, cancel, check, poll, threadDelay)
import Control.Concurrent.STM (
  newTQueueIO,
  readTQueue,
  writeTQueue,
 )
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Class.MonadAsync (MonadAsync, async)
import Control.Monad.Class.MonadSTM (MonadSTM (STM), atomically, newTVar, stateTVar)
import Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import Hydra.Ledger
import Hydra.Logging (Tracer, traceWith)
import Hydra.Logic (
  ClientRequest (..),
  ClientResponse (..),
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  LogicError (..),
  NetworkEvent (..),
  OnChainTx (..),
  Outcome (Error, NewState),
  confirmedLedger,
 )
import qualified Hydra.Logic as Logic
import Hydra.Network (HydraNetwork (..))

-- ** Create and run a hydra node

data HydraNode tx m = HydraNode
  { eq :: EventQueue m (Event tx)
  , hn :: HydraNetwork tx m
  , hh :: HydraHead tx m
  , oc :: OnChain m
  , sendResponse :: ClientResponse tx -> m ()
  , env :: Environment
  }

data HydraNodeLog tx
  = ErrorHandlingEvent (Event tx) (LogicError tx)
  | ProcessingEvent (Event tx)
  | ProcessedEvent (Event tx)
  | ProcessingEffect (Effect tx)
  | ProcessedEffect (Effect tx)

deriving instance Show tx => Show (UTxO tx) => Show (LedgerState tx) => Show (HydraNodeLog tx)
deriving instance Eq tx => Eq (UTxO tx) => Eq (LedgerState tx) => Eq (HydraNodeLog tx)

handleClientRequest :: HydraNode tx m -> ClientRequest tx -> m ()
handleClientRequest HydraNode{eq} = putEvent eq . ClientEvent

handleChainTx :: HydraNode tx m -> OnChainTx -> m ()
handleChainTx HydraNode{eq} = putEvent eq . OnChainEvent

handleMessage :: HydraNode tx m -> Logic.HydraMessage tx -> m ()
handleMessage HydraNode{eq} = putEvent eq . NetworkEvent . MessageReceived

queryLedgerState :: MonadSTM m => HydraNode tx m -> STM m (Maybe (LedgerState tx))
queryLedgerState HydraNode{hh} = getConfirmedLedger hh

runHydraNode ::
  MonadThrow m =>
  MonadAsync m =>
  MonadTimer m =>
  Show (UTxO tx) =>
  Show (LedgerState tx) => -- TODO(SN): leaky abstraction of HydraHead
  Show tx =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
runHydraNode tracer node@HydraNode{eq} = do
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ do
    e <- nextEvent eq
    traceWith tracer $ ProcessingEvent e
    processNextEvent node e >>= \case
      Left err -> traceWith tracer (ErrorHandlingEvent e err)
      Right effs -> forM_ effs (processEffect node tracer) >> traceWith tracer (ProcessedEvent e)

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  Show (UTxO tx) =>
  Show (LedgerState tx) =>
  Show tx =>
  MonadSTM m =>
  HydraNode tx m ->
  Event tx ->
  m (Either (LogicError tx) [Effect tx])
processNextEvent HydraNode{hh, env} e = do
  atomically $
    modifyHeadState hh $ \s ->
      case Logic.update env (ledger hh) s e of
        NewState s' effects -> (Right effects, s')
        Error err -> (Left err, s)

processEffect ::
  MonadAsync m =>
  MonadTimer m =>
  MonadThrow m =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc, sendResponse, eq} tracer e = do
  traceWith tracer $ ProcessingEffect e
  case e of
    ClientEffect i -> sendResponse i
    NetworkEffect msg -> broadcast hn msg
    OnChainEffect tx -> postTx oc tx
    Delay after event -> void . async $ threadDelay after >> putEvent eq event
    Wait -> panic "TODO: wait and reschedule continuation" -- TODO(SN) this error is not forced
  traceWith tracer $ ProcessedEffect e
-- ** Some general event queue from which the Hydra head is "fed"

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , nextEvent :: m e
  }

createEventQueue :: IO (EventQueue IO e)
createEventQueue = do
  q <- newTQueueIO
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

getConfirmedLedger :: MonadSTM m => HydraHead tx m -> STM m (Maybe (LedgerState tx))
getConfirmedLedger hh =
  queryHeadState hh <&> \case
    OpenState st -> Just (confirmedLedger st)
    _ -> Nothing

queryHeadState :: HydraHead tx m -> STM m (HeadState tx)
queryHeadState = (`modifyHeadState` \s -> (s, s))

putState :: HydraHead tx m -> HeadState tx -> STM m ()
putState HydraHead{modifyHeadState} new =
  modifyHeadState $ const ((), new)

createHydraHead :: (MonadSTM m) => HeadState tx -> Ledger tx -> m (HydraHead tx m)
createHydraHead initialState ledger = do
  tv <- atomically $ newTVar initialState
  pure HydraHead{modifyHeadState = stateTVar tv, ledger}
-- ** OnChain handle to abstract over chain access

data ChainError = ChainError
  deriving (Exception, Show)

-- | Handle to interface with the main chain network
newtype OnChain m = OnChain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    -- Does at least throw 'ChainError'.
    postTx :: MonadThrow m => OnChainTx -> m ()
  }
