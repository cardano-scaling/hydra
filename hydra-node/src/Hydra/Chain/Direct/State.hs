{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.State where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (init)

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Hydra.Chain (HeadId (..), HeadParameters, OnChainTx (..), PostTxError (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..), genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (
  AbortObservation (AbortObservation),
  AbortTxError (OverlappingInputs),
  CloseObservation (..),
  ClosedThreadOutput (..),
  ClosingSnapshot (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  FanoutObservation (FanoutObservation),
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
  headTokensFromValue,
  initTx,
  observeAbortTx,
  observeCloseTx,
  observeCollectComTx,
  observeCommitTx,
  observeContestTx,
  observeFanoutTx,
  observeInitTx,
 )
import Hydra.Data.ContestationPeriod (posixToUTCTime)
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.QuickCheck (sized)

-- | A class for accessing the known 'UTxO' set in a type. This is useful to get
-- all the relevant UTxO for resolving transaction inputs.
class HasKnownUTxO a where
  getKnownUTxO :: a -> UTxO

-- * States & transitions

-- | A definition of all transitions between 'ChainState's. Enumerable and
-- bounded to be used as labels for checking coverage.
data ChainTransition
  = Init
  | Commit
  | Collect
  | Close
  | Contest
  | Fanout
  deriving (Eq, Show, Enum, Bounded)

-- | An enumeration of all possible on-chain states of a Hydra Head, where each
-- case stores the relevant information to construct & observe transactions to
-- other states.
data ChainState
  = Idle IdleState
  | Initial InitialState
  | Open OpenState
  | Closed ClosedState
  deriving (Eq, Show)

instance HasKnownUTxO ChainState where
  getKnownUTxO :: ChainState -> UTxO
  getKnownUTxO = \case
    Idle st -> getKnownUTxO st
    Initial st -> getKnownUTxO st
    Open st -> getKnownUTxO st
    Closed st -> getKnownUTxO st

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
  arbitrary = sized $ \n -> do
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

-- | Get all cardano verification keys available in the chain context.
allVerificationKeys :: ChainContext -> [VerificationKey PaymentKey]
allVerificationKeys ChainContext{peerVerificationKeys, ownVerificationKey} =
  ownVerificationKey : peerVerificationKeys

-- | The idle state does not contain any head-specific information and exists to
-- be used as a starting and terminal state.
newtype IdleState = IdleState {ctx :: ChainContext}
  deriving (Show, Eq)

instance Arbitrary IdleState where
  arbitrary = IdleState <$> arbitrary

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

-- * Constructing transactions

-- | Construct an init transaction given some general 'ChainContext', the
-- 'HeadParameters' and a seed 'TxIn' which will be spent.
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

-- | Construct a commit transaction based on the 'InitialState'. This does look
-- for "our initial output" to spend and check the given 'UTxO' to be
-- compatible. Hence, this function does fail if already committed.
commit ::
  InitialState ->
  UTxO ->
  Either (PostTxError Tx) Tx
commit st utxo = do
  case ownInitial of
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

  ownInitial :: Maybe (TxIn, TxOut CtxUTxO, Hash PaymentKey)
  ownInitial =
    foldl' go Nothing initialInitials
   where
    go (Just x) _ = Just x
    go Nothing (i, out, _) = do
      let vkh = verificationKeyHash ownVerificationKey
      guard $ hasMatchingPT vkh (txOutValue out)
      pure (i, out, vkh)

  hasMatchingPT :: Hash PaymentKey -> Value -> Bool
  hasMatchingPT vkh val =
    case headTokensFromValue initialHeadTokenScript val of
      [(AssetName bs, 1)] -> bs == serialiseToRawBytes vkh
      _ -> False

  rejectByronAddress :: (TxIn, TxOut CtxUTxO) -> Either (PostTxError Tx) ()
  rejectByronAddress = \case
    (_, TxOut (ByronAddressInEra addr) _ _ _) ->
      Left (UnsupportedLegacyOutput addr)
    (_, TxOut ShelleyAddressInEra{} _ _ _) ->
      Right ()

-- | Construct a collect transaction based on the 'InitialState'. This will
-- reimburse all the already committed outputs.
abort ::
  HasCallStack =>
  InitialState ->
  Tx
abort st = do
  let InitialThreadOutput{initialThreadUTxO = (i, o, dat)} = initialThreadOutput
      initials = Map.fromList $ map tripleToPair initialInitials
      commits = Map.fromList $ map tripleToPair initialCommits
   in case abortTx scriptRegistry ownVerificationKey (i, o, dat) initialHeadTokenScript initials commits of
        Left OverlappingInputs ->
          -- FIXME: This is a "should not happen" error. We should try to fix
          -- the arguments of abortTx to make it impossible of having
          -- "overlapping" inputs. But, how exactly?
          error $ show OverlappingInputs
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

-- | Construct a collect transaction based on the 'InitialState'. This will know
-- collect all the committed outputs.
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

-- | Construct a close transaction based on the 'OpenState' and a confirmed
-- snapshot. The given 'PointInTime' will be used as an upper validity bound and
-- will define the start of the contestation period.
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

-- | Construct a contest transaction based on the 'ClosedState' and a confirmed
-- snapshot. The given 'PointInTime' will be used as an upper validity bound and
-- needs to be before the deadline.
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

-- | Construct a fanout transaction based on the 'ClosedState' and off-chain
-- agreed 'UTxO' set to fan out.
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

-- | Observe a transition without knowing the starting or ending state. This
-- function should try to observe all relevant transitions given some
-- 'ChainState'.
observeSomeTx :: Tx -> ChainState -> Maybe (OnChainTx Tx, ChainState)
observeSomeTx tx = \case
  Idle IdleState{ctx} ->
    second Initial <$> observeInit ctx tx
  Initial st ->
    second Initial <$> observeCommit st tx
      <|> second Idle <$> observeAbort st tx
      <|> second Open <$> observeCollect st tx
  Open st -> second Closed <$> observeClose st tx
  Closed st ->
    second Closed <$> observeContest st tx
      <|> second Idle <$> observeFanout st tx

-- ** IdleState transitions

-- | Observe an init transition using a 'InitialState' and 'observeInitTx'.
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

-- | Observe an commit transition using a 'InitialState' and 'observeCommitTx'.
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

-- | Observe an collect transition using a 'InitialState' and 'observeCollectComTx'.
-- This function checks the head id and ignores if not relevant.
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

-- | Observe an abort transition using a 'InitialState' and 'observeAbortTx'.
-- This function checks the head id and ignores if not relevant.
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

-- ** OpenState transitions

-- | Observe a close transition using a 'OpenState' and 'observeCloseTx'.
-- This function checks the head id and ignores if not relevant.
observeClose ::
  OpenState ->
  Tx ->
  Maybe (OnChainTx Tx, ClosedState)
observeClose st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCloseTx utxo tx
  let CloseObservation{threadOutput, headId, snapshotNumber} = observation
  guard (headId == openHeadId)
  let ClosedThreadOutput{closedContestationDeadline} = threadOutput
  let event =
        OnCloseTx
          { snapshotNumber
          , contestationDeadline = posixToUTCTime closedContestationDeadline
          }
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

-- ** ClosedState transitions

-- | Observe a fanout transition using a 'ClosedState' and 'observeContestTx'.
-- This function checks the head id and ignores if not relevant.
observeContest ::
  ClosedState ->
  Tx ->
  Maybe (OnChainTx Tx, ClosedState)
observeContest st tx = do
  let utxo = getKnownUTxO st
  observation <- observeContestTx utxo tx
  let ContestObservation{contestedThreadOutput, headId, snapshotNumber} = observation
  guard (headId == closedHeadId)
  let event = OnContestTx{snapshotNumber}
  let st' = st{closedThreadOutput = closedThreadOutput{closedThreadUTxO = contestedThreadOutput}}
  pure (event, st')
 where
  ClosedState
    { closedHeadId
    , closedThreadOutput
    } = st

-- | Observe a fanout transition using a 'ClosedState' and 'observeFanoutTx'.
observeFanout ::
  ClosedState ->
  Tx ->
  Maybe (OnChainTx Tx, IdleState)
observeFanout st tx = do
  let utxo = getKnownUTxO st
  FanoutObservation <- observeFanoutTx utxo tx
  pure (OnFanoutTx, IdleState{ctx})
 where
  ClosedState{ctx} = st

-- * Helpers

fst3 :: (a, b, c) -> a
fst3 (a, _b, _c) = a

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))

take2Of3 :: (a, b, c) -> (a, b)
take2Of3 (a, b, _c) = (a, b)
