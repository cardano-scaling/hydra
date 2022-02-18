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

import qualified Cardano.Api.UTxO as UTxO
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
  ownInitial,
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
  SomeOnChainHeadState ::
    forall st m.
    HasTransition st =>
    OnChainHeadState m st ->
    SomeOnChainHeadState m

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

-- NOTE / TODO: The only real reason why all the function belows are in STM is
-- because they do access the wallet's UTXO for error reporting. This is
-- misleading and slightly annoying for testing given that all those functions
-- are actually "pure" (modulo exceptions which can be captured as 'Either').
--
-- I'd suggest to remove the `MonadThrow` instance and instead have the
-- potentially failing functions return an 'Either' and, do the exception
-- throwing in the upper-layer, wrapping the error with details such as the
-- wallet UTxO.

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
commit utxo st@OnChainHeadState{networkId, ownParty, ownWallet, stateMachine} = do
  case ownInitial (verificationKey ownWallet) initialInitials of
    Nothing ->
      throwIO (CannotFindOwnInitial{knownUTxO = getKnownUTxO st} :: PostTxError Tx)
    Just initial ->
      case UTxO.pairs utxo of
        [aUTxO] -> do
          pure $ commitTx networkId ownParty (Just aUTxO) initial
        [] -> do
          pure $ commitTx networkId ownParty Nothing initial
        _ ->
          throwIO (MoreThanOneUTxOCommitted @Tx)
 where
  Initialized
    { initialInitials
    } = stateMachine

abort ::
  (MonadSTM m) =>
  OnChainHeadState m 'StInitialized ->
  STM m Tx
abort OnChainHeadState{networkId, stateMachine} = do
  let (i, _, dat, _) = initialThreadOutput
      initials = Map.fromList $ map drop2nd initialInitials
      commits = Map.fromList $ map tripleToPair initialCommits
   in case abortTx networkId (i, dat) initials commits of
        Left err ->
          -- FIXME: Exception with MonadThrow?
          error $ show err
        Right tx' ->
          pure tx'
 where
  Initialized
    { initialThreadOutput
    , initialInitials
    , initialCommits
    } = stateMachine

collect ::
  (MonadSTM m) =>
  OnChainHeadState m 'StInitialized ->
  STM m Tx
collect OnChainHeadState{networkId, stateMachine} = do
  let (i, _, dat, parties) = initialThreadOutput
      commits = Map.fromList $ fmap tripleToPair initialCommits
  pure $ collectComTx networkId (i, dat, parties) commits
 where
  Initialized
    { initialThreadOutput
    , initialCommits
    } = stateMachine

close ::
  (MonadSTM m) =>
  ConfirmedSnapshot Tx ->
  OnChainHeadState m 'StOpen ->
  STM m Tx
close confirmedSnapshot OnChainHeadState{stateMachine} = do
  let (sn, sigs) =
        case confirmedSnapshot of
          ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
          InitialSnapshot{snapshot} -> (snapshot, mempty)
  let (i, o, dat, _) = openThreadOutput
  pure $ closeTx sn sigs (i, o, dat)
 where
  Open{openThreadOutput} = stateMachine

fanout ::
  (MonadSTM m) =>
  UTxO ->
  OnChainHeadState m 'StClosed ->
  STM m Tx
fanout utxo OnChainHeadState{stateMachine} = do
  let (i, _, dat, _) = closedThreadOutput
   in pure $ fanoutTx utxo (i, dat)
 where
  Closed{closedThreadOutput} = stateMachine

-- Observing Transitions

class HasTransition st where
  observeTx ::
    Tx ->
    OnChainHeadState m st ->
    STM m (Maybe (OnChainTx Tx, SomeOnChainHeadState m))

instance HasTransition 'StIdle where
  observeTx = undefined

instance HasTransition 'StInitialized where
  observeTx = undefined

instance HasTransition 'StOpen where
  observeTx = undefined

instance HasTransition 'StClosed where
  observeTx = undefined

--
-- Helpers
--

type UTxOWithScript = (TxIn, TxOut CtxUTxO, ScriptData)

fst3 :: (a, b, c) -> a
fst3 (a, _b, _c) = a

drop2nd :: (a, b, c) -> (a, c)
drop2nd (a, _b, c) = (a, c)

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))
