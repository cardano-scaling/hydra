{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.State (
  -- * OnChainHeadState
  OnChainHeadState,
  HeadStateKind (..),
  getKnownUTxO,

  -- ** Working with opaque states
  SomeOnChainHeadState (..),
  TokHeadState (..),
  reifyState,

  -- ** Initializing
  idleOnChainHeadState,

  -- ** Constructing transitions
  initialize,
  abort,
  commit,
  collect,
  close,
  fanout,

  -- ** Observing transitions
  observeTx,
) where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Data.Map as Map
import Hydra.Chain (HeadParameters, OnChainTx (..), PostTxError (..))
import Hydra.Chain.Direct.Tx (
  abortTx,
  closeTx,
  collectComTx,
  commitTx,
  fanoutTx,
  initTx,
  observeAbortTx,
  observeCloseTx,
  observeCollectComTx,
  observeCommitTx,
  observeFanoutTx,
  observeInitTx,
 )
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import qualified Hydra.Data.Party as OnChain
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..))

-- | An opaque on-chain head state, which records information and events
-- happening on the layer-1 for a given Hydra head.
data OnChainHeadState m (st :: HeadStateKind) = OnChainHeadState
  { networkId :: NetworkId
  , ownWallet :: TinyWallet m
  , ownParty :: Party
  , stateMachine :: HydraStateMachine st
  }

-- NOTE (1): The state machine UTxO produced by the Init transaction (a.k.a
-- 'threadOutput') is always present and 'threaded' across all transactions.
--
-- NOTE (2): The Head's identifier is somewhat encoded in the TxOut's address
--
-- TODO: Data and [OnChain.Party] are overlapping
--
-- TODO: There are better ways to model this to avoid duplicating common fields
-- across all branches!
data HydraStateMachine (st :: HeadStateKind) where
  Idle :: HydraStateMachine 'StIdle
  Initialized ::
    { initialThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    , initialInitials :: [UTxOWithScript]
    , initialCommits :: [UTxOWithScript]
    } ->
    HydraStateMachine 'StInitialized
  Open ::
    { openThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    } ->
    HydraStateMachine 'StOpen
  Closed ::
    { closedThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    } ->
    HydraStateMachine 'StClosed

getKnownUTxO ::
  OnChainHeadState m st ->
  UTxO
getKnownUTxO =
  undefined

-- Working with opaque states

-- | An existential wrapping /some/ 'OnChainHeadState' into a value that carry
-- no type-level information about the state.
data SomeOnChainHeadState m where
  SomeOnChainHeadState :: forall st m. OnChainHeadState m st -> SomeOnChainHeadState m

-- | Some Kind for witnessing Hydra state-machine's states at the type-level.
--
-- This is useful to
--
-- (a) Reads code evolving the state machine, as it makes transition more
-- obvious from type-signatures;
-- (b) Pattern-match on the 'HydraStateMachine' without having to bother with
-- non-reachable cases.
data HeadStateKind = StIdle | StInitialized | StOpen | StClosed

-- | A token witnessing the state's type of an 'OnChainHeadState'. See 'reifyState'
data TokHeadState (st :: HeadStateKind) where
  TkIdle :: TokHeadState 'StIdle
  TkInitialized :: TokHeadState 'StInitialized
  TkOpen :: TokHeadState 'StOpen
  TkClosed :: TokHeadState 'StClosed

deriving instance Show (TokHeadState st)

-- | Reify a 'HeadStateKind' kind into a value to enable pattern-matching on
-- existentials.
reifyState :: forall m st. OnChainHeadState m st -> TokHeadState st
reifyState OnChainHeadState{stateMachine} =
  case stateMachine of
    Idle{} -> TkIdle
    Initialized{} -> TkInitialized
    Open{} -> TkOpen
    Closed{} -> TkClosed

-- Initialization

-- | Initialize a new 'OnChainHeadState'.
idleOnChainHeadState ::
  NetworkId ->
  TinyWallet m ->
  Party ->
  OnChainHeadState m 'StIdle
idleOnChainHeadState networkId ownWallet ownParty =
  OnChainHeadState
    { networkId
    , ownWallet
    , ownParty
    , stateMachine = Idle
    }

-- Constructing Transitions

-- | Initialize a head from an 'StIdle' state. This does not change the state
-- but produces a transaction which, if observed, does apply the transition.
initialize ::
  (MonadSTM m, MonadThrow (STM m)) =>
  HeadParameters ->
  [VerificationKey PaymentKey] ->
  OnChainHeadState m 'StIdle ->
  STM m Tx
initialize params cardanoKeys OnChainHeadState{ownWallet, networkId} = do
  u <- getUTxO ownWallet
  -- NOTE: 'lookupMax' to favor change outputs!
  case Map.lookupMax u of
    Just (fromLedgerTxIn -> seedInput, _) ->
      pure $ initTx networkId cardanoKeys params seedInput
    Nothing ->
      throwIO (NoSeedInput @Tx)

commit ::
  (MonadSTM m, MonadThrow (STM m)) =>
  UTxO ->
  OnChainHeadState m 'StInitialized ->
  STM m Tx
commit =
  undefined

abort ::
  (MonadSTM m, MonadThrow (STM m)) =>
  OnChainHeadState m 'StInitialized ->
  STM m Tx
abort =
  undefined

collect ::
  (MonadSTM m, MonadThrow (STM m)) =>
  OnChainHeadState m 'StInitialized ->
  STM m Tx
collect =
  undefined

close ::
  (MonadSTM m, MonadThrow (STM m)) =>
  ConfirmedSnapshot Tx ->
  OnChainHeadState m 'StOpen ->
  STM m Tx
close =
  undefined

fanout ::
  (MonadSTM m, MonadThrow (STM m)) =>
  UTxO ->
  OnChainHeadState m 'StClosed ->
  STM m Tx
fanout =
  undefined

-- Observing Transitions

class HasTransition st st' where
  observeTx ::
    Tx ->
    OnChainHeadState m st ->
    m (Maybe (OnChainTx Tx, OnChainHeadState m st'))

instance HasTransition 'StIdle 'StInitialized where
  observeTx = undefined

instance HasTransition 'StInitialized 'StIdle where
  observeTx = undefined

instance HasTransition 'StInitialized 'StInitialized where
  observeTx = undefined

instance HasTransition 'StInitialized 'StOpen where
  observeTx = undefined

instance HasTransition 'StOpen 'StClosed where
  observeTx = undefined

instance HasTransition 'StClosed 'StIdle where
  observeTx = undefined

--
-- Helpers
--

type UTxOWithScript = (TxIn, TxOut CtxUTxO, ScriptData)

fst3 :: (a, b, c) -> a
fst3 (a, _b, _c) = a
