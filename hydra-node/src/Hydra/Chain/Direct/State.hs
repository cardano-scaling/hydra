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
  HasTransition,
  observeTx,
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

-- | An opaque on-chain head state, which records information and events
-- happening on the layer-1 for a given Hydra head.
data OnChainHeadState (st :: HeadStateKind) = OnChainHeadState
  { networkId :: NetworkId
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
    } ->
    HydraStateMachine 'StInitialized
  Open ::
    { openThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    , openHeadId :: HeadId
    , openHeadTokenScript :: PlutusScript
    } ->
    HydraStateMachine 'StOpen
  Closed ::
    { closedThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    , closedHeadId :: HeadId
    , closedHeadTokenScript :: PlutusScript
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
  VerificationKey PaymentKey ->
  Party ->
  OnChainHeadState 'StIdle
idleOnChainHeadState networkId ownVerificationKey ownParty =
  OnChainHeadState
    { networkId
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
  case ownInitial ownVerificationKey initialInitials of
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
    } = stateMachine

abort ::
  HasCallStack =>
  OnChainHeadState 'StInitialized ->
  Tx
abort OnChainHeadState{networkId, stateMachine} = do
  let (i, o, dat, _) = initialThreadOutput
      initials = Map.fromList $ map drop2nd initialInitials
      commits = Map.fromList $ map tripleToPair initialCommits
   in case abortTx networkId (i, o, dat) initials commits of
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
collect OnChainHeadState{networkId, stateMachine} = do
  let commits = Map.fromList $ fmap tripleToPair initialCommits
   in collectComTx networkId initialThreadOutput commits
 where
  Initialized
    { initialThreadOutput
    , initialCommits
    } = stateMachine

close ::
  ConfirmedSnapshot Tx ->
  OnChainHeadState 'StOpen ->
  Tx
close confirmedSnapshot OnChainHeadState{stateMachine} = do
  let (sn, sigs) =
        case confirmedSnapshot of
          ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
          InitialSnapshot{snapshot} -> (snapshot, mempty)
      (i, o, dat, _) = openThreadOutput
   in closeTx sn sigs (i, o, dat)
 where
  Open{openThreadOutput} = stateMachine

fanout ::
  UTxO ->
  OnChainHeadState 'StClosed ->
  Tx
fanout utxo OnChainHeadState{stateMachine} = do
  let (i, _, dat, _) = closedThreadOutput
   in fanoutTx utxo (i, dat) closedHeadTokenScript
 where
  Closed{closedThreadOutput, closedHeadTokenScript} = stateMachine

-- Observing Transitions

class HasTransition st where
  observeTx ::
    Tx ->
    OnChainHeadState st ->
    Maybe (OnChainTx Tx, SomeOnChainHeadState)

instance HasTransition 'StIdle where
  observeTx tx OnChainHeadState{networkId, ownParty, ownVerificationKey} = do
    (event, observation) <- observeInitTx networkId ownParty tx
    let InitObservation{threadOutput, initials, commits, headId, headTokenScript} = observation
    let st' =
          OnChainHeadState
            { networkId
            , ownParty
            , ownVerificationKey
            , stateMachine =
                Initialized
                  { initialThreadOutput = threadOutput
                  , initialInitials = initials
                  , initialCommits = commits
                  , initialHeadId = headId
                  , initialHeadTokenScript = headTokenScript
                  }
            }
    pure (event, SomeOnChainHeadState st')

instance HasTransition 'StInitialized where
  observeTx tx st@OnChainHeadState{networkId, ownVerificationKey, ownParty, stateMachine} = do
    observeCommit <|> observeCollectCom <|> observeAbort
   where
    Initialized
      { initialCommits
      , initialInitials
      , initialHeadId
      , initialHeadTokenScript
      } = stateMachine

    observeCommit = do
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
      pure (event, SomeOnChainHeadState st')

    observeCollectCom = do
      let utxo = getKnownUTxO st
      (event, observation) <- observeCollectComTx utxo tx
      let CollectComObservation{threadOutput, headId} = observation
      guard (headId == initialHeadId)
      let st' =
            OnChainHeadState
              { networkId
              , ownVerificationKey
              , ownParty
              , stateMachine =
                  Open
                    { openThreadOutput = threadOutput
                    , openHeadId = headId
                    , openHeadTokenScript = initialHeadTokenScript
                    }
              }
      pure (event, SomeOnChainHeadState st')

    observeAbort = do
      let utxo = getKnownUTxO st
      (event, ()) <- observeAbortTx utxo tx
      let st' =
            OnChainHeadState
              { networkId
              , ownVerificationKey
              , ownParty
              , stateMachine = Idle
              }
      pure (event, SomeOnChainHeadState st')

instance HasTransition 'StOpen where
  observeTx tx st@OnChainHeadState{networkId, ownVerificationKey, ownParty, stateMachine} = do
    let utxo = getKnownUTxO st
    (event, observation) <- observeCloseTx utxo tx
    let CloseObservation{threadOutput, headId} = observation
    guard (headId == openHeadId)
    let st' =
          OnChainHeadState
            { networkId
            , ownVerificationKey
            , ownParty
            , stateMachine =
                Closed
                  { closedThreadOutput = threadOutput
                  , closedHeadId = headId
                  , closedHeadTokenScript = openHeadTokenScript
                  }
            }
    pure (event, SomeOnChainHeadState st')
   where
    Open
      { openHeadId
      , openHeadTokenScript
      } = stateMachine

instance HasTransition 'StClosed where
  observeTx tx st@OnChainHeadState{networkId, ownVerificationKey, ownParty} = do
    let utxo = getKnownUTxO st
    (event, ()) <- observeFanoutTx utxo tx
    let st' =
          OnChainHeadState
            { networkId
            , ownVerificationKey
            , ownParty
            , stateMachine = Idle
            }
    pure (event, SomeOnChainHeadState st')

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

take2Of4 :: (a, b, c, d) -> (a, b)
take2Of4 (a, b, _c, _d) = (a, b)

take2Of3 :: (a, b, c) -> (a, b)
take2Of3 (a, b, _c) = (a, b)
