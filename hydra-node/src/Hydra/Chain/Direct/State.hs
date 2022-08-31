{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.State where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (init)

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Data.Typeable (cast, eqT, typeRep, type (:~:) (Refl))
import Hydra.Chain (HeadId (..), HeadParameters, OnChainTx (..), PostTxError (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..), genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (
  AbortObservation (AbortObservation),
  CloseObservation (..),
  ClosedThreadOutput (..),
  ClosingSnapshot (..),
  CollectComObservation (..),
  CommitObservation (..),
  InitObservation (..),
  InitialThreadOutput (..),
  OpenThreadOutput (..),
  UTxOWithScript,
  abortTx,
  closeTx,
  collectComTx,
  commitTx,
  contestTx,
  fanoutTx,
  initTx,
  observeAbortTx,
  observeCloseTx,
  observeCollectComTx,
  observeCommitTx,
  observeInitTx,
  ownInitial,
 )
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Plutus.V2.Ledger.Api (POSIXTime)
import Test.QuickCheck (choose, sized)
import qualified Text.Show

-- | A class for accessing the known 'UTxO' set in a type.
class HasKnownUTxO a where
  getKnownUTxO :: a -> UTxO

-- * States

-- | Read-only chain-specific data. This is different to 'HydraContext' as it
-- only provide contains data known to single peer.
data ChainContext = ChainContext
  { networkId :: NetworkId
  , peerVerificationKeys :: [VerificationKey PaymentKey]
  , ownVerificationKey :: VerificationKey PaymentKey
  , ownParty :: Party
  , scriptRegistry :: ScriptRegistry
  }
  deriving (Show, Eq)

instance Arbitrary ChainContext where
  arbitrary = sized $ \n -> choose (0, n) >>= genChainContext

-- | Generate a 'HydraContext' for a given number of parties.
genChainContext :: Int -> Gen ChainContext
genChainContext n = do
  networkId <- Testnet . NetworkMagic <$> arbitrary
  peerVerificationKeys <- replicateM n genVerificationKey
  ownVerificationKey <- genVerificationKey
  ownParty <- arbitrary
  scriptRegistry <- genScriptRegistry
  pure
    ChainContext
      { networkId
      , peerVerificationKeys
      , ownVerificationKey
      , ownParty
      , scriptRegistry
      }

-- | Get all cardano verification kesy known in the chain context.
allVerificationKeys :: ChainContext -> [VerificationKey PaymentKey]
allVerificationKeys ChainContext{peerVerificationKeys, ownVerificationKey} =
  ownVerificationKey : peerVerificationKeys

-- | The idle state does not contain any head-specific information and exists to
-- be used as a starting and terminal state.
newtype IdleState = IdleState {ctx :: ChainContext}
  deriving (Show, Eq)

instance HasKnownUTxO IdleState where
  getKnownUTxO IdleState{ctx = ChainContext{scriptRegistry}} =
    registryUTxO scriptRegistry

data InitialState = InitialState
  { ctx :: ChainContext
  , initialThreadOutput :: InitialThreadOutput
  , initialInitials :: [UTxOWithScript]
  , initialCommits :: [UTxOWithScript]
  , initialHeadId :: HeadId
  , initialHeadTokenScript :: PlutusScript
  }
  deriving (Show, Eq)

instance HasKnownUTxO InitialState where
  getKnownUTxO st =
    registryUTxO scriptRegistry <> headUtxo
   where
    headUtxo =
      UTxO $
        Map.fromList $
          take2Of3 initialThreadUTxO : (take2Of3 <$> (initialInitials <> initialCommits))

    InitialState
      { ctx = ChainContext{scriptRegistry}
      , initialThreadOutput = InitialThreadOutput{initialThreadUTxO}
      , initialInitials
      , initialCommits
      } = st

data OpenState = OpenState
  { ctx :: ChainContext
  , openThreadOutput :: OpenThreadOutput
  , openHeadId :: HeadId
  , openHeadTokenScript :: PlutusScript
  , openUtxoHash :: ByteString
  }
  deriving (Show, Eq)

instance HasKnownUTxO OpenState where
  getKnownUTxO st =
    registryUTxO scriptRegistry <> UTxO.singleton (i, o)
   where
    OpenState
      { ctx = ChainContext{scriptRegistry}
      , openThreadOutput = OpenThreadOutput{openThreadUTxO = (i, o, _)}
      } = st

data ClosedState = ClosedState
  { ctx :: ChainContext
  , closedThreadOutput :: ClosedThreadOutput
  , closedHeadId :: HeadId
  , closedHeadTokenScript :: PlutusScript
  }
  deriving (Show, Eq)

instance HasKnownUTxO ClosedState where
  getKnownUTxO st =
    registryUTxO scriptRegistry <> UTxO.singleton (i, o)
   where
    ClosedState
      { ctx = ChainContext{scriptRegistry}
      , closedThreadOutput = ClosedThreadOutput{closedThreadUTxO = (i, o, _)}
      } = st

getContestationDeadline :: ClosedState -> POSIXTime
getContestationDeadline
  ClosedState{closedThreadOutput = ClosedThreadOutput{closedContestationDeadline}} =
    closedContestationDeadline

-- Working with opaque states

-- | An existential wrapping /some/ on-chain head state into a value that carry
-- no type-level information about the state except that is 'HasTransitions' and 'HasKnownUTxO'.
data SomeOnChainHeadState where
  SomeOnChainHeadState ::
    forall st.
    (Eq st, Typeable st, Show st, HasKnownUTxO st, HasTransitions st) =>
    st ->
    SomeOnChainHeadState

instance Show SomeOnChainHeadState where
  show (SomeOnChainHeadState st) = show st

instance Eq SomeOnChainHeadState where
  (SomeOnChainHeadState (st :: a)) == (SomeOnChainHeadState (st' :: b)) =
    case eqT @a @b of
      Just Refl -> st == st'
      Nothing -> False

-- | Access the actual head state in the existential given it is of type 'st'.
castHeadState :: Typeable st => SomeOnChainHeadState -> Maybe st
castHeadState (SomeOnChainHeadState x) = cast x

-- * Constructing transactions

-- | A thin wrapper around 'initTx'. The seed input will determine the head identifier.
initialize ::
  ChainContext ->
  HeadParameters ->
  -- | Seed input.
  TxIn ->
  Tx
initialize ctx =
  initTx networkId (allVerificationKeys ctx)
 where
  ChainContext{networkId} = ctx

commit ::
  InitialState ->
  UTxO ->
  Either (PostTxError Tx) Tx
commit st utxo = do
  case ownInitial initialHeadTokenScript ownVerificationKey initialInitials of
    Nothing ->
      Left (CannotFindOwnInitial{knownUTxO = getKnownUTxO st})
    Just initial ->
      case UTxO.pairs utxo of
        [aUTxO] -> do
          rejectByronAddress aUTxO
          Right $ commitTx networkId ownParty (Just aUTxO) initial
        [] -> do
          Right $ commitTx networkId ownParty Nothing initial
        _ ->
          Left (MoreThanOneUTxOCommitted @Tx)
 where
  InitialState
    { ctx = ChainContext{networkId, ownParty, ownVerificationKey}
    , initialInitials
    , initialHeadTokenScript
    } = st

  rejectByronAddress :: (TxIn, TxOut CtxUTxO) -> Either (PostTxError Tx) ()
  rejectByronAddress = \case
    (_, TxOut (ByronAddressInEra addr) _ _ _) ->
      Left (UnsupportedLegacyOutput addr)
    (_, TxOut ShelleyAddressInEra{} _ _ _) ->
      Right ()

abort ::
  HasCallStack =>
  InitialState ->
  Tx
abort st = do
  let InitialThreadOutput{initialThreadUTxO = (i, o, dat)} = initialThreadOutput
      initials = Map.fromList $ map tripleToPair initialInitials
      commits = Map.fromList $ map tripleToPair initialCommits
   in case abortTx scriptRegistry ownVerificationKey (i, o, dat) initialHeadTokenScript initials commits of
        Left err ->
          -- FIXME: Exception with MonadThrow?
          error $ show err
        Right tx ->
          tx
 where
  InitialState
    { ctx = ChainContext{ownVerificationKey, scriptRegistry}
    , initialThreadOutput
    , initialInitials
    , initialCommits
    , initialHeadTokenScript
    } = st

collect ::
  InitialState ->
  Tx
collect st = do
  let commits = Map.fromList $ fmap tripleToPair initialCommits
   in collectComTx networkId ownVerificationKey initialThreadOutput commits
 where
  InitialState
    { ctx = ChainContext{networkId, ownVerificationKey}
    , initialThreadOutput
    , initialCommits
    } = st

close ::
  OpenState ->
  ConfirmedSnapshot Tx ->
  PointInTime ->
  Tx
close st confirmedSnapshot pointInTime =
  closeTx ownVerificationKey closingSnapshot pointInTime openThreadOutput
 where
  closingSnapshot = case confirmedSnapshot of
    -- XXX: Not needing anything of the 'InitialSnapshot' is another hint that
    -- we should not keep track of an actual initial 'Snapshot'
    InitialSnapshot{} -> CloseWithInitialSnapshot{openUtxoHash}
    ConfirmedSnapshot{snapshot = Snapshot{number, utxo}, signatures} ->
      CloseWithConfirmedSnapshot
        { snapshotNumber = number
        , closeUtxoHash = hashUTxO @Tx utxo
        , signatures
        }

  OpenState
    { ctx = ChainContext{ownVerificationKey}
    , openThreadOutput
    , openUtxoHash
    } = st

contest ::
  ClosedState ->
  ConfirmedSnapshot Tx ->
  PointInTime ->
  Tx
contest st confirmedSnapshot pointInTime = do
  contestTx ownVerificationKey sn sigs pointInTime closedThreadOutput
 where
  (sn, sigs) =
    case confirmedSnapshot of
      ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
      InitialSnapshot{snapshot} -> (snapshot, mempty)

  ClosedState
    { ctx = ChainContext{ownVerificationKey}
    , closedThreadOutput
    } = st

-- | Construct a fanout transaction based on the 'ClosedState' and known 'UTxO'
-- set to fan out.
fanout ::
  ClosedState ->
  UTxO ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  Tx
fanout st utxo deadlineSlotNo = do
  fanoutTx utxo (i, o, dat) deadlineSlotNo closedHeadTokenScript
 where
  ClosedState{closedThreadOutput, closedHeadTokenScript} = st

  ClosedThreadOutput{closedThreadUTxO = (i, o, dat)} = closedThreadOutput

-- * Observing Transitions

-- | A class for observing a transition from a state to another given the right
-- transaction.
class ObserveTx st st' where
  observeTx ::
    st ->
    Tx ->
    Maybe (OnChainTx Tx, st')

-- | A convenient class to declare all possible transitions from a given
-- starting state 'st'.
class HasTransitions st where
  transitions :: proxy st -> [TransitionFrom st]

-- | An existential to be used in 'HasTransitions'.
data TransitionFrom st where
  TransitionTo ::
    forall st st'.
    (Typeable st, Typeable st', ObserveTx st st', Eq st', Show st', HasKnownUTxO st', HasTransitions st') =>
    String ->
    Proxy st' ->
    TransitionFrom st

instance Show (TransitionFrom st) where
  show (TransitionTo name proxy) =
    mconcat
      [ show (typeRep (Proxy @st))
      , " -> "
      , show (typeRep proxy)
      , ": " <> name
      ]

instance Eq (TransitionFrom st) where
  (TransitionTo name proxy) == (TransitionTo name' proxy') =
    name == name'
      && typeRep proxy == typeRep proxy'

-- ** IdleState transitions

instance HasTransitions IdleState where
  transitions _ =
    [TransitionTo "init" (Proxy @InitialState)]

instance ObserveTx IdleState InitialState where
  observeTx IdleState{ctx} = observeInit ctx

observeInit ::
  ChainContext ->
  Tx ->
  Maybe (OnChainTx Tx, InitialState)
observeInit ctx tx = do
  observation <- observeInitTx networkId (allVerificationKeys ctx) ownParty tx
  pure (toEvent observation, toState observation)
 where
  toEvent InitObservation{contestationPeriod, parties} =
    OnInitTx{contestationPeriod, parties}

  toState InitObservation{threadOutput, initials, commits, headId, headTokenScript} =
    InitialState
      { ctx
      , initialThreadOutput = threadOutput
      , initialInitials = initials
      , initialCommits = commits
      , initialHeadId = headId
      , initialHeadTokenScript = headTokenScript
      }

  ChainContext
    { networkId
    , ownParty
    } = ctx
-- ** InitialState transitions

instance HasTransitions InitialState where
  transitions _ = [] -- TODO
  -- [ TransitionTo "commit" (Proxy @InitialState)
  -- , TransitionTo "collect" (Proxy @OpenState)
  -- , TransitionTo "abort" (Proxy @IdleState)
  -- ]

observeCommit ::
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, InitialState)
observeCommit st tx = do
  let initials = fst3 <$> initialInitials
  observation <- observeCommitTx networkId initials tx
  let CommitObservation{commitOutput, party, committed} = observation
  let event = OnCommitTx{party, committed}
  let st' =
        st
          { initialInitials =
              -- NOTE: A commit tx has been observed and thus we can
              -- remove all it's inputs from our tracked initials
              filter ((`notElem` txIns' tx) . fst3) initialInitials
          , initialCommits =
              commitOutput : initialCommits
          }
  pure (event, st')
 where
  InitialState
    { ctx = ChainContext{networkId}
    , initialCommits
    , initialInitials
    } = st

instance ObserveTx InitialState InitialState where
  observeTx = observeCommit

observeCollect ::
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, OpenState)
observeCollect st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCollectComTx utxo tx
  let CollectComObservation{threadOutput, headId, utxoHash} = observation
  guard (headId == initialHeadId)
  let event = OnCollectComTx
  let st' =
        OpenState
          { ctx
          , openThreadOutput = threadOutput
          , openHeadId = initialHeadId
          , openHeadTokenScript = initialHeadTokenScript
          , openUtxoHash = utxoHash
          }
  pure (event, st')
 where
  InitialState
    { ctx
    , initialHeadId
    , initialHeadTokenScript
    } = st

instance ObserveTx InitialState OpenState where
  observeTx = observeCollect

observeAbort ::
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, IdleState)
observeAbort st tx = do
  let utxo = getKnownUTxO st
  AbortObservation <- observeAbortTx utxo tx
  let event = OnAbortTx
  let st' = IdleState{ctx}
  pure (event, st')
 where
  InitialState{ctx} = st

instance ObserveTx InitialState IdleState where
  observeTx = observeAbort

--
-- StOpen
--

instance HasTransitions OpenState where
  transitions _ = [] -- TODO:
  -- [ TransitionTo "close" (Proxy @ClosedState)
  -- ]

observeClose ::
  OpenState ->
  Tx ->
  Maybe (OnChainTx Tx, ClosedState)
observeClose st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCloseTx utxo tx
  let CloseObservation{threadOutput, headId, snapshotNumber} = observation
  guard (headId == openHeadId)
  -- FIXME: The 0 here is a wart. We are in a pure function so we cannot easily compute with
  -- time. We tried passing the current time from the caller but given the current machinery
  -- around `observeSomeTx` this is actually not straightforward and quite ugly.
  let event = OnCloseTx{snapshotNumber, remainingContestationPeriod = 0}
  let st' =
        ClosedState
          { ctx
          , closedThreadOutput = threadOutput
          , closedHeadId = headId
          , closedHeadTokenScript = openHeadTokenScript
          }
  pure (event, st')
 where
  OpenState
    { ctx
    , openHeadId
    , openHeadTokenScript
    } = st

instance ObserveTx OpenState ClosedState where
  observeTx = observeClose

--
-- StClosed
--

instance HasTransitions ClosedState where
  transitions _ = [] -- TODO
  -- [ TransitionTo "contest" (Proxy @ClosedState)
  -- , TransitionTo "fanout" (Proxy @IdleState)
  -- ]

-- instance ObserveTx 'StClosed 'StIdle where
--   observeTx tx st@OnChainHeadState{networkId, peerVerificationKeys, ownVerificationKey, ownParty, scriptRegistry} = do
--     let utxo = getKnownUTxO st
--     FanoutObservation <- observeFanoutTx utxo tx
--     let event = OnFanoutTx
--     let st' =
--           OnChainHeadState
--             { networkId
--             , peerVerificationKeys
--             , ownVerificationKey
--             , ownParty
--             , scriptRegistry
--             , stateMachine = Idle
--             }
--     pure (event, st')

-- instance ObserveTx 'StClosed 'StClosed where
--   observeTx tx st@OnChainHeadState{networkId, peerVerificationKeys, ownVerificationKey, ownParty, stateMachine, scriptRegistry} = do
--     let utxo = getKnownUTxO st
--     observation <- observeContestTx utxo tx
--     let ContestObservation{contestedThreadOutput, headId, snapshotNumber} = observation
--     guard (headId == closedHeadId)
--     let event = OnContestTx{snapshotNumber}
--     let st' =
--           OnChainHeadState
--             { networkId
--             , peerVerificationKeys
--             , ownVerificationKey
--             , ownParty
--             , scriptRegistry
--             , stateMachine =
--                 Closed
--                   { closedThreadOutput = closedThreadOutput{closedThreadUTxO = contestedThreadOutput}
--                   , closedHeadId
--                   , closedHeadTokenScript
--                   }
--             }
--     pure (event, st')
--    where
--     Closed
--       { closedHeadId
--       , closedHeadTokenScript
--       , closedThreadOutput
--       } = stateMachine

-- | A convenient way to apply transition to 'SomeOnChainHeadState' without
-- known the starting or ending state. This function does enumerate and try all
-- 'transitions' of some given starting state.
observeSomeTx ::
  Tx ->
  SomeOnChainHeadState ->
  Maybe (OnChainTx Tx, SomeOnChainHeadState)
observeSomeTx tx (SomeOnChainHeadState (st :: st)) =
  asum $ (\(TransitionTo _ p) -> observeSome p) <$> transitions (Proxy :: Proxy st)
 where
  observeSome ::
    forall st'.
    (ObserveTx st st', Typeable st', Eq st', Show st', HasTransitions st', HasKnownUTxO st') =>
    Proxy st' ->
    Maybe (OnChainTx Tx, SomeOnChainHeadState)
  observeSome _ =
    second SomeOnChainHeadState <$> observeTx @st @st' st tx

--
-- Helpers
--

fst3 :: (a, b, c) -> a
fst3 (a, _b, _c) = a

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))

take2Of3 :: (a, b, c) -> (a, b)
take2Of3 (a, b, _c) = (a, b)
