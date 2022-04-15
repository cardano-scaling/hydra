{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.State (
  -- * OnChainHeadState
  OnChainHeadState,
  HeadStateKind (..),
  HeadStateKindVal (..),
  getKnownUTxO,

  -- ** Working with opaque states
  SomeOnChainHeadState (..),
  TokHeadState (..),
  reifyState,
  happenedAt,

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
  observeSomeTx,

  -- *** Internal API
  ObserveTx (..),
  HasTransition (..),
  TransitionFrom (..),
) where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Hydra.Chain (HeadId (..), HeadParameters, OnChainTx (..), PostTxError (..))
import Hydra.Chain.Direct.Tx (
  CloseObservation (..),
  CollectComObservation (..),
  InitObservation (..),
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
import qualified Hydra.Data.Party as OnChain
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..))
import qualified Text.Show

-- | An opaque on-chain head state, which records information and events
-- happening on the layer-1 for a given Hydra head.
data OnChainHeadState (st :: HeadStateKind) = OnChainHeadState
  { networkId :: NetworkId
  , peerVerificationKeys :: [VerificationKey PaymentKey]
  , ownVerificationKey :: VerificationKey PaymentKey
  , ownParty :: Party
  , stateMachine :: HydraStateMachine st
  }
  deriving (Show)

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
    , initialHeadId :: HeadId
    , initialHeadTokenScript :: PlutusScript
    , initialHappenedAt :: ChainPoint
    } ->
    HydraStateMachine 'StInitialized
  Open ::
    { openThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    , openHeadId :: HeadId
    , openHeadTokenScript :: PlutusScript
    -- , openHappenedAt :: ChainPoint
    } ->
    HydraStateMachine 'StOpen
  Closed ::
    { closedThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    , closedHeadId :: HeadId
    , closedHeadTokenScript :: PlutusScript
    -- , closedHappenedAt :: ChainPoint
    } ->
    HydraStateMachine 'StClosed

deriving instance Show (HydraStateMachine st)

getKnownUTxO ::
  OnChainHeadState st ->
  UTxO
getKnownUTxO OnChainHeadState{stateMachine} =
  case stateMachine of
    Idle{} ->
      mempty
    Initialized{initialThreadOutput, initialInitials, initialCommits} ->
      UTxO $
        Map.fromList $
          take2Of4 initialThreadOutput : (take2Of3 <$> (initialInitials <> initialCommits))
    Open{openThreadOutput = (i, o, _, _)} ->
      UTxO.singleton (i, o)
    Closed{closedThreadOutput = (i, o, _, _)} ->
      UTxO.singleton (i, o)

-- Working with opaque states

-- | An existential wrapping /some/ 'OnChainHeadState' into a value that carry
-- no type-level information about the state.
data SomeOnChainHeadState where
  SomeOnChainHeadState ::
    forall st.
    HasTransition st =>
    OnChainHeadState st ->
    SomeOnChainHeadState

instance Show SomeOnChainHeadState where
  show (SomeOnChainHeadState st) = show st

happenedAt :: SomeOnChainHeadState -> Maybe ChainPoint
happenedAt (SomeOnChainHeadState st) =
  case (reifyState st, st) of
    (TkIdle, _stIdle) ->
      Nothing
    (TkInitialized, stInitialized) ->
      Just (initialHappenedAt (stateMachine stInitialized))
    (TkOpen, _stOpen) ->
      Nothing
    -- Just (openHappenedAt stOpen)
    (TkClosed, _stClosed) ->
      Nothing

--  Just (closeHappenedAt stClosed)

-- rewind :: SomeOnChainHeadState -> SomeOnChainHeadState
-- rewind = error "rewind"

-- | Some Kind for witnessing Hydra state-machine's states at the type-level.
--
-- This is useful to
--
-- (a) Reads code evolving the state machine, as it makes transition more
-- obvious from type-signatures;
-- (b) Pattern-match on the 'HydraStateMachine' without having to bother with
-- non-reachable cases.
data HeadStateKind = StIdle | StInitialized | StOpen | StClosed
  deriving (Eq, Show, Enum, Bounded)

class HeadStateKindVal (st :: HeadStateKind) where
  headStateKindVal :: Proxy st -> HeadStateKind

instance HeadStateKindVal 'StIdle where
  headStateKindVal _ = StIdle
instance HeadStateKindVal 'StInitialized where
  headStateKindVal _ = StInitialized
instance HeadStateKindVal 'StOpen where
  headStateKindVal _ = StOpen
instance HeadStateKindVal 'StClosed where
  headStateKindVal _ = StClosed

-- | A token witnessing the state's type of an 'OnChainHeadState'. See 'reifyState'
data TokHeadState (st :: HeadStateKind) where
  TkIdle :: TokHeadState 'StIdle
  TkInitialized :: TokHeadState 'StInitialized
  TkOpen :: TokHeadState 'StOpen
  TkClosed :: TokHeadState 'StClosed

deriving instance Show (TokHeadState st)

-- | Reify a 'HeadStateKind' kind into a value to enable pattern-matching on
-- existentials.
reifyState :: forall st. OnChainHeadState st -> TokHeadState st
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
  [VerificationKey PaymentKey] ->
  VerificationKey PaymentKey ->
  Party ->
  OnChainHeadState 'StIdle
idleOnChainHeadState networkId peerVerificationKeys ownVerificationKey ownParty =
  OnChainHeadState
    { networkId
    , peerVerificationKeys
    , ownVerificationKey
    , ownParty
    , stateMachine = Idle
    }

-- Constructing Transitions

-- | Initialize a head from an 'StIdle' state. This does not change the state
-- but produces a transaction which, if observed, does apply the transition.
initialize ::
  HeadParameters ->
  [VerificationKey PaymentKey] ->
  TxIn ->
  OnChainHeadState 'StIdle ->
  Tx
initialize params cardanoKeys seedInput OnChainHeadState{networkId} = do
  initTx networkId cardanoKeys params seedInput

commit ::
  UTxO ->
  OnChainHeadState 'StInitialized ->
  Either (PostTxError Tx) Tx
commit utxo st@OnChainHeadState{networkId, ownParty, ownVerificationKey, stateMachine} = do
  case ownInitial initialHeadTokenScript ownVerificationKey initialInitials of
    Nothing ->
      Left (CannotFindOwnInitial{knownUTxO = getKnownUTxO st})
    Just initial ->
      case UTxO.pairs utxo of
        [aUTxO] -> do
          Right $ commitTx networkId ownParty (Just aUTxO) initial
        [] -> do
          Right $ commitTx networkId ownParty Nothing initial
        _ ->
          Left (MoreThanOneUTxOCommitted @Tx)
 where
  Initialized
    { initialInitials
    , initialHeadTokenScript
    } = stateMachine

abort ::
  HasCallStack =>
  OnChainHeadState 'StInitialized ->
  Tx
abort OnChainHeadState{ownVerificationKey, stateMachine} = do
  let (i, o, dat, _) = initialThreadOutput
      initials = Map.fromList $ map tripleToPair initialInitials
      commits = Map.fromList $ map tripleToPair initialCommits
   in case abortTx ownVerificationKey (i, o, dat) (initialHeadTokenScript stateMachine) initials commits of
        Left err ->
          -- FIXME: Exception with MonadThrow?
          error $ show err
        Right tx ->
          tx
 where
  Initialized
    { initialThreadOutput
    , initialInitials
    , initialCommits
    } = stateMachine

collect ::
  OnChainHeadState 'StInitialized ->
  Tx
collect OnChainHeadState{networkId, ownVerificationKey, stateMachine} = do
  let commits = Map.fromList $ fmap tripleToPair initialCommits
   in collectComTx networkId ownVerificationKey initialThreadOutput commits
 where
  Initialized
    { initialThreadOutput
    , initialCommits
    } = stateMachine

close ::
  ConfirmedSnapshot Tx ->
  OnChainHeadState 'StOpen ->
  Tx
close confirmedSnapshot OnChainHeadState{ownVerificationKey, stateMachine} = do
  let (sn, sigs) =
        case confirmedSnapshot of
          ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
          InitialSnapshot{snapshot} -> (snapshot, mempty)
      (i, o, dat, _) = openThreadOutput
   in closeTx ownVerificationKey sn sigs (i, o, dat)
 where
  Open{openThreadOutput} = stateMachine

fanout ::
  UTxO ->
  OnChainHeadState 'StClosed ->
  Tx
fanout utxo OnChainHeadState{stateMachine} = do
  let (i, o, dat, _) = closedThreadOutput
   in fanoutTx utxo (i, o, dat) closedHeadTokenScript
 where
  Closed{closedThreadOutput, closedHeadTokenScript} = stateMachine

-- Observing Transitions

-- | A class for describing a Hydra transition from a state to another.
--
-- The transition is encoded at the type-level through the `HeadStateKind` and
-- the function `transition` overloaded for all transitions.
class
  HasTransition st =>
  ObserveTx (st :: HeadStateKind) (st' :: HeadStateKind)
  where
  observeTx ::
    Tx ->
    OnChainHeadState st ->
    Maybe (OnChainTx Tx, OnChainHeadState st')

-- | A convenient class to declare all possible transitions from a given
-- starting state 'st'. This is useful to embed 'OnChainHeadState' with an
-- existential that carries some capabilities in the form of transitions (e.g.
-- 'SomeOnChainHeadState').
class HasTransition (st :: HeadStateKind) where
  transitions ::
    Proxy st -> [TransitionFrom st]

--
-- StIdle
--

instance HasTransition 'StIdle where
  transitions _ =
    [ TransitionTo (Proxy @ 'StInitialized)
    ]

instance ObserveTx 'StIdle 'StInitialized where
  observeTx tx OnChainHeadState{networkId, peerVerificationKeys, ownParty, ownVerificationKey} = do
    let allVerificationKeys = ownVerificationKey : peerVerificationKeys
    (event, observation) <- observeInitTx networkId allVerificationKeys ownParty tx
    let InitObservation{threadOutput, initials, commits, headId, headTokenScript} = observation
    let st' =
          OnChainHeadState
            { networkId
            , ownParty
            , ownVerificationKey
            , peerVerificationKeys
            , stateMachine =
                Initialized
                  { initialThreadOutput = threadOutput
                  , initialInitials = initials
                  , initialCommits = commits
                  , initialHeadId = headId
                  , initialHeadTokenScript = headTokenScript
                  , initialHappenedAt = error "initialHappenedAt"
                  }
            }
    pure (event, st')

--
-- StInitialized
--

instance HasTransition 'StInitialized where
  transitions _ =
    [ TransitionTo (Proxy @ 'StInitialized)
    , TransitionTo (Proxy @ 'StOpen)
    , TransitionTo (Proxy @ 'StIdle)
    ]

instance ObserveTx 'StInitialized 'StInitialized where
  observeTx tx st@OnChainHeadState{networkId, stateMachine} = do
    let initials = fst3 <$> initialInitials
    (event, newCommit) <- observeCommitTx networkId initials tx
    let st' =
          st
            { stateMachine =
                stateMachine
                  { initialInitials =
                      -- NOTE: A commit tx has been observed and thus we can
                      -- remove all it's inputs from our tracked initials
                      filter ((`notElem` txIns' tx) . fst3) initialInitials
                  , initialCommits =
                      newCommit : initialCommits
                  }
            }
    pure (event, st')
   where
    Initialized
      { initialCommits
      , initialInitials
      } = stateMachine

instance ObserveTx 'StInitialized 'StOpen where
  observeTx tx st@OnChainHeadState{networkId, peerVerificationKeys, ownVerificationKey, ownParty, stateMachine} = do
    let utxo = getKnownUTxO st
    (event, observation) <- observeCollectComTx utxo tx
    let CollectComObservation{threadOutput, headId} = observation
    guard (headId == initialHeadId)
    let st' =
          OnChainHeadState
            { networkId
            , peerVerificationKeys
            , ownVerificationKey
            , ownParty
            , stateMachine =
                Open
                  { openThreadOutput = threadOutput
                  , openHeadId = initialHeadId
                  , openHeadTokenScript = initialHeadTokenScript
                  }
            }
    pure (event, st')
   where
    Initialized
      { initialHeadId
      , initialHeadTokenScript
      } = stateMachine

instance ObserveTx 'StInitialized 'StIdle where
  observeTx tx st@OnChainHeadState{networkId, peerVerificationKeys, ownVerificationKey, ownParty} = do
    let utxo = getKnownUTxO st
    (event, ()) <- observeAbortTx utxo tx
    let st' =
          OnChainHeadState
            { networkId
            , peerVerificationKeys
            , ownVerificationKey
            , ownParty
            , stateMachine = Idle
            }
    pure (event, st')

--
-- StOpen
--

instance HasTransition 'StOpen where
  transitions _ =
    [ TransitionTo (Proxy @ 'StClosed)
    ]

instance ObserveTx 'StOpen 'StClosed where
  observeTx tx st@OnChainHeadState{networkId, peerVerificationKeys, ownVerificationKey, ownParty, stateMachine} = do
    let utxo = getKnownUTxO st
    (event, observation) <- observeCloseTx utxo tx
    let CloseObservation{threadOutput, headId} = observation
    guard (headId == openHeadId)
    let st' =
          OnChainHeadState
            { networkId
            , peerVerificationKeys
            , ownVerificationKey
            , ownParty
            , stateMachine =
                Closed
                  { closedThreadOutput = threadOutput
                  , closedHeadId = headId
                  , closedHeadTokenScript = openHeadTokenScript
                  }
            }
    pure (event, st')
   where
    Open
      { openHeadId
      , openHeadTokenScript
      } = stateMachine

--
-- StClosed
--

instance HasTransition 'StClosed where
  transitions _ =
    [ TransitionTo (Proxy @ 'StIdle)
    ]

instance ObserveTx 'StClosed 'StIdle where
  observeTx tx st@OnChainHeadState{networkId, peerVerificationKeys, ownVerificationKey, ownParty} = do
    let utxo = getKnownUTxO st
    (event, ()) <- observeFanoutTx utxo tx
    let st' =
          OnChainHeadState
            { networkId
            , peerVerificationKeys
            , ownVerificationKey
            , ownParty
            , stateMachine = Idle
            }
    pure (event, st')

-- | A convenient way to apply transition to 'SomeOnChainHeadState' without
-- bothering about the internal details.
observeSomeTx ::
  Tx ->
  SomeOnChainHeadState ->
  Maybe (OnChainTx Tx, SomeOnChainHeadState)
observeSomeTx tx (SomeOnChainHeadState (st :: OnChainHeadState st)) =
  asum $ (\(TransitionTo st') -> observeSome st') <$> transitions (Proxy @st)
 where
  observeSome ::
    forall st'.
    (ObserveTx st st', HasTransition st') =>
    Proxy st' ->
    Maybe (OnChainTx Tx, SomeOnChainHeadState)
  observeSome _ =
    second SomeOnChainHeadState <$> observeTx @st @st' tx st

--
-- TransitionFrom
--

data TransitionFrom st where
  TransitionTo ::
    forall st st'.
    ( ObserveTx st st'
    , HasTransition st'
    , HeadStateKindVal st
    , HeadStateKindVal st'
    ) =>
    Proxy st' ->
    TransitionFrom st

instance Show (TransitionFrom st) where
  show (TransitionTo proxy) =
    mconcat
      [ show (headStateKindVal (Proxy @st))
      , " -> "
      , show (headStateKindVal proxy)
      ]

instance Eq (TransitionFrom st) where
  (TransitionTo proxy) == (TransitionTo proxy') =
    headStateKindVal proxy == headStateKindVal proxy'

--
-- Helpers
--

type UTxOWithScript = (TxIn, TxOut CtxUTxO, ScriptData)

fst3 :: (a, b, c) -> a
fst3 (a, _b, _c) = a

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))

take2Of4 :: (a, b, c, d) -> (a, b)
take2Of4 (a, b, _c, _d) = (a, b)

take2Of3 :: (a, b, c) -> (a, b)
take2Of3 (a, b, _c) = (a, b)
