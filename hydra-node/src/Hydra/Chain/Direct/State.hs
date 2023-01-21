{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Contains the a state-ful interface to transaction construction and observation.
--
-- It defines the 'ChainStateType tx' to be used in the 'Hydra.Chain.Direct'
-- layer and it's constituents.
module Hydra.Chain.Direct.State where

import Hydra.Prelude hiding (init)

import qualified Cardano.Api.UTxO as UTxO
import Data.List ((\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  AssetName (AssetName),
  ChainPoint (..),
  CtxUTxO,
  Hash,
  Key (SigningKey, VerificationKey, verificationKeyHash),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  PlutusScript,
  SerialiseAsRawBytes (serialiseToRawBytes),
  SlotNo (SlotNo),
  Tx,
  TxIn,
  TxOut,
  UTxO,
  UTxO' (UTxO),
  Value,
  chainPointToSlotNo,
  txIns',
  txOutValue,
  pattern ByronAddressInEra,
  pattern ShelleyAddressInEra,
  pattern TxOut,
 )
import Hydra.Chain (
  ChainSlot (ChainSlot),
  ChainStateType,
  HeadId (..),
  HeadParameters (..),
  IsChainState (..),
  OnChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct.ScriptRegistry (
  ScriptRegistry (..),
  genScriptRegistry,
  registryUTxO,
 )
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
  UTxOHash (UTxOHash),
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
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.Data.ContestationPeriod (posixToUTCTime)
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genUTxOAdaOnlyOfSize, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genPointInTimeBefore, genValidityBoundsFromContestationPeriod, slotNoFromUTCTime)
import Hydra.Ledger.Cardano.Json ()
import Hydra.Options (maximumNumberOfParties)
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  genConfirmedSnapshot,
  getSnapshot,
 )
import Test.QuickCheck (choose, frequency, oneof, sized, vector)
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Modifiers (Positive (Positive))

-- | A class for accessing the known 'UTxO' set in a type. This is useful to get
-- all the relevant UTxO for resolving transaction inputs.
class HasKnownUTxO a where
  getKnownUTxO :: a -> UTxO

-- * States & transitions

-- | The chain state used by the Hydra.Chain.Direct implementation. It records
-- the actual 'ChainState' paired with a 'ChainSlot' (used to know up to which
-- point to rewind on rollbacks).
data ChainStateAt = ChainStateAt
  { chainState :: ChainState
  , recordedAt :: Maybe ChainPoint
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary ChainStateAt where
  arbitrary = genericArbitrary

instance IsChainState Tx where
  type ChainStateType Tx = ChainStateAt

  chainStateSlot ChainStateAt{recordedAt} =
    maybe (ChainSlot 0) chainSlotFromPoint recordedAt

-- | Get a generic 'ChainSlot' from a Cardano 'ChainPoint'. Slot 0 is used for
-- the genesis point.
chainSlotFromPoint :: ChainPoint -> ChainSlot
chainSlotFromPoint p =
  case chainPointToSlotNo p of
    Nothing -> ChainSlot 0
    Just (SlotNo s) -> ChainSlot $ fromIntegral s

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
  = -- | The idle state does not contain any head-specific information and exists to
    -- be used as a starting and terminal state.
    Idle
  | Initial InitialState
  | Open OpenState
  | Closed ClosedState
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary ChainState where
  arbitrary = genChainState

instance HasKnownUTxO ChainState where
  getKnownUTxO :: ChainState -> UTxO
  getKnownUTxO = \case
    Idle -> mempty
    Initial st -> getKnownUTxO st
    Open st -> getKnownUTxO st
    Closed st -> getKnownUTxO st

-- | Read-only chain-specific data. This is different to 'HydraContext' as it
-- only contains data known to single peer.
data ChainContext = ChainContext
  { networkId :: NetworkId
  , peerVerificationKeys :: [VerificationKey PaymentKey]
  , ownVerificationKey :: VerificationKey PaymentKey
  , ownParty :: Party
  , scriptRegistry :: ScriptRegistry
  , contestationPeriod :: ContestationPeriod
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance HasKnownUTxO ChainContext where
  getKnownUTxO ChainContext{scriptRegistry} = registryUTxO scriptRegistry

instance Arbitrary ChainContext where
  arbitrary = sized $ \n -> do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    peerVerificationKeys <- replicateM n genVerificationKey
    ownVerificationKey <- genVerificationKey
    ownParty <- arbitrary
    scriptRegistry <- genScriptRegistry
    contestationPeriod <- arbitrary
    pure
      ChainContext
        { networkId
        , peerVerificationKeys
        , ownVerificationKey
        , ownParty
        , scriptRegistry
        , contestationPeriod
        }

-- | Get all cardano verification keys available in the chain context.
allVerificationKeys :: ChainContext -> [VerificationKey PaymentKey]
allVerificationKeys ChainContext{peerVerificationKeys, ownVerificationKey} =
  ownVerificationKey : peerVerificationKeys

data InitialState = InitialState
  { initialThreadOutput :: InitialThreadOutput
  , initialInitials :: [UTxOWithScript]
  , initialCommits :: [UTxOWithScript]
  , headId :: HeadId
  , initialHeadTokenScript :: PlutusScript
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance HasKnownUTxO InitialState where
  getKnownUTxO st =
    UTxO $
      Map.fromList $
        take2Of3 initialThreadUTxO : (take2Of3 <$> (initialInitials <> initialCommits))
   where
    take2Of3 (a, b, _c) = (a, b)

    InitialState
      { initialThreadOutput = InitialThreadOutput{initialThreadUTxO}
      , initialInitials
      , initialCommits
      } = st

data OpenState = OpenState
  { openThreadOutput :: OpenThreadOutput
  , headId :: HeadId
  , openHeadTokenScript :: PlutusScript
  , openUtxoHash :: UTxOHash
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance HasKnownUTxO OpenState where
  getKnownUTxO st =
    UTxO.singleton (i, o)
   where
    OpenState
      { openThreadOutput = OpenThreadOutput{openThreadUTxO = (i, o, _)}
      } = st

data ClosedState = ClosedState
  { closedThreadOutput :: ClosedThreadOutput
  , headId :: HeadId
  , closedHeadTokenScript :: PlutusScript
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance HasKnownUTxO ClosedState where
  getKnownUTxO st =
    UTxO.singleton (i, o)
   where
    ClosedState
      { closedThreadOutput = ClosedThreadOutput{closedThreadUTxO = (i, o, _)}
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
  ChainContext ->
  InitialState ->
  UTxO ->
  Either (PostTxError Tx) Tx
commit ctx st utxo = do
  case ownInitial of
    Nothing ->
      Left (CannotFindOwnInitial{knownUTxO = getKnownUTxO st})
    Just initial ->
      case UTxO.pairs utxo of
        [aUTxO] -> do
          rejectByronAddress aUTxO
          Right $ commitTx scriptRegistry networkId headId ownParty (Just aUTxO) initial
        [] -> do
          Right $ commitTx scriptRegistry networkId headId ownParty Nothing initial
        _ ->
          Left (MoreThanOneUTxOCommitted @Tx)
 where
  ChainContext{networkId, ownParty, ownVerificationKey, scriptRegistry} = ctx

  InitialState
    { initialInitials
    , initialHeadTokenScript
    , headId
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
  -- | Committed UTxOs to reimburse.
  UTxO ->
  ChainContext ->
  InitialState ->
  Tx
abort committedUTxO ctx st = do
  let InitialThreadOutput{initialThreadUTxO = (i, o, dat)} = initialThreadOutput
      initials = Map.fromList $ map tripleToPair initialInitials
      commits = Map.fromList $ map tripleToPair initialCommits
   in case abortTx committedUTxO scriptRegistry ownVerificationKey (i, o, dat) initialHeadTokenScript initials commits of
        Left OverlappingInputs ->
          -- FIXME: This is a "should not happen" error. We should try to fix
          -- the arguments of abortTx to make it impossible of having
          -- "overlapping" inputs. But, how exactly?
          error $ show OverlappingInputs
        Right tx ->
          tx
 where
  ChainContext{ownVerificationKey, scriptRegistry} = ctx

  InitialState
    { initialThreadOutput
    , initialInitials
    , initialCommits
    , initialHeadTokenScript
    } = st

  tripleToPair (a, b, c) = (a, (b, c))

-- | Construct a collect transaction based on the 'InitialState'. This will know
-- collect all the committed outputs.
collect ::
  ChainContext ->
  InitialState ->
  Tx
collect ctx st = do
  let commits = Map.fromList $ fmap tripleToPair initialCommits
   in collectComTx networkId ownVerificationKey initialThreadOutput commits headId
 where
  ChainContext{networkId, ownVerificationKey} = ctx

  InitialState
    { initialThreadOutput
    , initialCommits
    , headId
    } = st

  tripleToPair (a, b, c) = (a, (b, c))

-- | Construct a close transaction based on the 'OpenState' and a confirmed
-- snapshot.
--  - 'SlotNo' parameter will be used as the 'Tx' lower bound.
--  - 'PointInTime' parameter will be used as an upper validity bound and
--       will define the start of the contestation period.
-- NB: lower and upper bound slot difference should not exceed contestation period
close ::
  ChainContext ->
  OpenState ->
  ConfirmedSnapshot Tx ->
  -- | 'Tx' validity lower bound
  SlotNo ->
  -- | 'Tx' validity upper bound
  PointInTime ->
  Tx
close ctx st confirmedSnapshot startSlotNo pointInTime =
  closeTx ownVerificationKey closingSnapshot startSlotNo pointInTime openThreadOutput headId
 where
  closingSnapshot = case confirmedSnapshot of
    -- XXX: Not needing anything of the 'InitialSnapshot' is another hint that
    -- we should not keep track of an actual initial 'Snapshot'
    InitialSnapshot{} -> CloseWithInitialSnapshot{openUtxoHash}
    ConfirmedSnapshot{snapshot = Snapshot{number, utxo}, signatures} ->
      CloseWithConfirmedSnapshot
        { snapshotNumber = number
        , closeUtxoHash = UTxOHash $ hashUTxO @Tx utxo
        , signatures
        }

  ChainContext{ownVerificationKey} = ctx

  OpenState
    { openThreadOutput
    , openUtxoHash
    , headId
    } = st

-- | Construct a contest transaction based on the 'ClosedState' and a confirmed
-- snapshot. The given 'PointInTime' will be used as an upper validity bound and
-- needs to be before the deadline.
contest ::
  ChainContext ->
  ClosedState ->
  ConfirmedSnapshot Tx ->
  PointInTime ->
  Tx
contest ctx st confirmedSnapshot pointInTime = do
  contestTx ownVerificationKey sn sigs pointInTime closedThreadOutput headId
 where
  (sn, sigs) =
    case confirmedSnapshot of
      ConfirmedSnapshot{signatures} -> (getSnapshot confirmedSnapshot, signatures)
      _ -> (getSnapshot confirmedSnapshot, mempty)

  ChainContext{ownVerificationKey} = ctx

  ClosedState
    { closedThreadOutput
    , headId
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
observeSomeTx :: ChainContext -> ChainState -> Tx -> Maybe (OnChainTx Tx, ChainState)
observeSomeTx ctx cst tx = case cst of
  Idle ->
    second Initial <$> observeInit ctx tx
  Initial st ->
    second Initial <$> observeCommit ctx st tx
      <|> (,Idle) <$> observeAbort st tx
      <|> second Open <$> observeCollect st tx
  Open st -> second Closed <$> observeClose st tx
  Closed st ->
    second Closed <$> observeContest st tx
      <|> (,Idle) <$> observeFanout st tx

-- ** IdleState transitions

-- | Observe an init transition using a 'InitialState' and 'observeInitTx'.
observeInit ::
  ChainContext ->
  Tx ->
  Maybe (OnChainTx Tx, InitialState)
observeInit ctx tx = do
  observation <-
    observeInitTx
      networkId
      (allVerificationKeys ctx)
      (Hydra.Chain.Direct.State.contestationPeriod ctx)
      ownParty
      tx
  pure (toEvent observation, toState observation)
 where
  toEvent InitObservation{contestationPeriod, parties, headId} =
    OnInitTx{contestationPeriod, parties, headId}

  toState InitObservation{threadOutput, initials, commits, headId, headTokenScript} =
    InitialState
      { initialThreadOutput = threadOutput
      , initialInitials = initials
      , initialCommits = commits
      , headId = headId
      , initialHeadTokenScript = headTokenScript
      }

  ChainContext{networkId, ownParty} = ctx

-- ** InitialState transitions

-- | Observe an commit transition using a 'InitialState' and 'observeCommitTx'.
observeCommit ::
  ChainContext ->
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, InitialState)
observeCommit ctx st tx = do
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
  ChainContext{networkId} = ctx

  InitialState
    { initialCommits
    , initialInitials
    } = st

  fst3 (a, _b, _c) = a

-- | Observe an collect transition using a 'InitialState' and 'observeCollectComTx'.
-- This function checks the head id and ignores if not relevant.
observeCollect ::
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, OpenState)
observeCollect st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCollectComTx utxo tx
  let CollectComObservation{threadOutput, headId = collectComHeadId, utxoHash} = observation
  guard (headId == collectComHeadId)
  let event = OnCollectComTx
  let st' =
        OpenState
          { openThreadOutput = threadOutput
          , headId = headId
          , openHeadTokenScript = initialHeadTokenScript
          , openUtxoHash = utxoHash
          }
  pure (event, st')
 where
  InitialState
    { headId = headId
    , initialHeadTokenScript
    } = st

-- | Observe an abort transition using a 'InitialState' and 'observeAbortTx'.
observeAbort ::
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx)
observeAbort st tx = do
  let utxo = getKnownUTxO st
  AbortObservation <- observeAbortTx utxo tx
  pure OnAbortTx

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
  let CloseObservation{threadOutput, headId = closeObservationHeadId, snapshotNumber} = observation
  guard (openHeadId == closeObservationHeadId)
  let ClosedThreadOutput{closedContestationDeadline} = threadOutput
  let event =
        OnCloseTx
          { snapshotNumber
          , contestationDeadline = posixToUTCTime closedContestationDeadline
          }
  let st' =
        ClosedState
          { closedThreadOutput = threadOutput
          , headId = openHeadId
          , closedHeadTokenScript = openHeadTokenScript
          }
  pure (event, st')
 where
  OpenState
    { headId = openHeadId
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
  let ContestObservation{contestedThreadOutput, headId = contestObservationHeadId, snapshotNumber, closedContestors} = observation
  guard (closedStateHeadId == contestObservationHeadId)
  let event = OnContestTx{snapshotNumber}
  let st' = st{closedThreadOutput = closedThreadOutput{closedThreadUTxO = contestedThreadOutput, closedContestors}}
  pure (event, st')
 where
  ClosedState
    { headId = closedStateHeadId
    , closedThreadOutput
    } = st

-- | Observe a fanout transition using a 'ClosedState' and 'observeFanoutTx'.
observeFanout ::
  ClosedState ->
  Tx ->
  Maybe (OnChainTx Tx)
observeFanout st tx = do
  let utxo = getKnownUTxO st
  FanoutObservation <- observeFanoutTx utxo tx
  pure OnFanoutTx

-- * Generators

-- | Maximum number of parties used in the generators.
maxGenParties :: Int
maxGenParties = 3

-- | Maximum number of assets (ADA or other tokens) used in the generators.
maxGenAssets :: Int
maxGenAssets = 70

-- | Generate a 'ChainState' within known limits above.
genChainState :: Gen ChainState
genChainState =
  oneof
    [ pure Idle
    , Initial <$> genInitialState
    , Open <$> genOpenState
    , Closed <$> genClosedState
    ]
 where
  genInitialState = do
    ctx <- genHydraContext maxGenParties
    snd <$> genStInitial ctx

  genOpenState = do
    ctx <- genHydraContext maxGenParties
    snd <$> genStOpen ctx

  genClosedState = do
    -- XXX: Untangle the whole generator mess here
    (_, st, _) <- genFanoutTx maxGenParties maxGenAssets
    pure st

-- | Generate a 'ChainContext' and 'ChainState' within the known limits above, along with a
-- transaction that results in a transition away from it.
genChainStateWithTx :: Gen (ChainContext, ChainState, Tx, ChainTransition)
genChainStateWithTx =
  oneof
    [ genInitWithState
    , genCommitWithState
    , genCollectWithState
    , genCloseWithState
    , genContestWithState
    , genFanoutWithState
    ]
 where
  genInitWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genInitWithState = do
    ctx <- genHydraContext maxGenParties
    cctx <- pickChainContext ctx
    seedInput <- genTxIn
    let tx = initialize cctx (ctxHeadParameters ctx) seedInput
    pure (cctx, Idle, tx, Init)

  genCommitWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genCommitWithState = do
    ctx <- genHydraContext maxGenParties
    (cctx, stInitial) <- genStInitial ctx
    utxo <- genCommit
    let tx = unsafeCommit cctx stInitial utxo
    pure (cctx, Initial stInitial, tx, Commit)

  genCollectWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genCollectWithState = do
    (ctx, _, st, tx) <- genCollectComTx
    pure (ctx, Initial st, tx, Collect)

  genCloseWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genCloseWithState = do
    (ctx, st, tx, _) <- genCloseTx maxGenParties
    pure (ctx, Open st, tx, Close)

  genContestWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genContestWithState = do
    (hctx, _, st, tx) <- genContestTx
    ctx <- pickChainContext hctx
    pure (ctx, Closed st, tx, Contest)

  genFanoutWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genFanoutWithState = do
    Positive numParties <- arbitrary
    Positive numOutputs <- arbitrary
    (hctx, st, tx) <- genFanoutTx numParties numOutputs
    ctx <- pickChainContext hctx
    pure (ctx, Closed st, tx, Fanout)

-- ** Warning zone

-- | Define some 'global' context from which generators can pick
-- values for generation. This allows to write fairly independent generators
-- which however still make sense with one another within the context of a head.
--
-- For example, one can generate a head's _party_ from that global list, whereas
-- other functions may rely on all parties and thus, we need both generation to
-- be coherent.
--
-- Do not use this in production code, but only for generating test data.
data HydraContext = HydraContext
  { ctxVerificationKeys :: [VerificationKey PaymentKey]
  , ctxHydraSigningKeys :: [SigningKey HydraKey]
  , ctxNetworkId :: NetworkId
  , ctxContestationPeriod :: ContestationPeriod
  }
  deriving (Show)

ctxParties :: HydraContext -> [Party]
ctxParties = fmap deriveParty . ctxHydraSigningKeys

ctxHeadParameters ::
  HydraContext ->
  HeadParameters
ctxHeadParameters ctx@HydraContext{ctxContestationPeriod} =
  HeadParameters ctxContestationPeriod (ctxParties ctx)

-- | Generate a `HydraContext` for a bounded arbitrary number of parties.
--
-- 'maxParties'  sets the upper bound in the number of parties in the Head.
genHydraContext :: Int -> Gen HydraContext
genHydraContext maxParties = choose (1, maxParties) >>= genHydraContextFor

-- | Generate a 'HydraContext' for a given number of parties.
genHydraContextFor :: Int -> Gen HydraContext
genHydraContextFor n = do
  ctxVerificationKeys <- replicateM n genVerificationKey
  ctxHydraSigningKeys <- fmap generateSigningKey <$> vector n
  ctxNetworkId <- Testnet . NetworkMagic <$> arbitrary
  ctxContestationPeriod <- arbitrary
  pure $
    HydraContext
      { ctxVerificationKeys
      , ctxHydraSigningKeys
      , ctxNetworkId
      , ctxContestationPeriod
      }

-- | Get all peer-specific 'ChainContext's from a 'HydraContext'. NOTE: This
-- assumes that 'HydraContext' has same length 'ctxVerificationKeys' and
-- 'ctxHydraSigningKeys'.
deriveChainContexts :: HydraContext -> Gen [ChainContext]
deriveChainContexts ctx = do
  scriptRegistry <- genScriptRegistry
  pure $
    flip map (zip ctxVerificationKeys allParties) $ \(vk, p) ->
      ChainContext
        { networkId = ctxNetworkId
        , peerVerificationKeys = ctxVerificationKeys \\ [vk]
        , ownVerificationKey = vk
        , ownParty = p
        , scriptRegistry
        , contestationPeriod = ctxContestationPeriod ctx
        }
 where
  allParties = ctxParties ctx

  HydraContext
    { ctxVerificationKeys
    , ctxNetworkId
    } = ctx

-- | Pick one of the participants and derive the peer-specific 'ChainContext'
-- from a 'HydraContext'. NOTE: This assumes that 'HydraContext' has same length
-- 'ctxVerificationKeys' and 'ctxHydraSigningKeys'.
pickChainContext :: HydraContext -> Gen ChainContext
pickChainContext ctx =
  deriveChainContexts ctx >>= elements

genStInitial ::
  HydraContext ->
  Gen (ChainContext, InitialState)
genStInitial ctx = do
  seedInput <- genTxIn
  cctx <- pickChainContext ctx
  let txInit = initialize cctx (ctxHeadParameters ctx) seedInput
  pure (cctx, snd . fromJust $ observeInit cctx txInit)

genInitTx ::
  HydraContext ->
  Gen Tx
genInitTx ctx = do
  cctx <- pickChainContext ctx
  initialize cctx (ctxHeadParameters ctx) <$> genTxIn

genCommits ::
  HydraContext ->
  Tx ->
  Gen [Tx]
genCommits ctx txInit = do
  allChainContexts <- deriveChainContexts ctx
  forM allChainContexts $ \cctx -> do
    let (_, stInitial) = fromJust $ observeInit cctx txInit
    unsafeCommit cctx stInitial <$> genCommit

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]

genCollectComTx :: Gen (ChainContext, [UTxO], InitialState, Tx)
genCollectComTx = do
  ctx <- genHydraContextFor maximumNumberOfParties
  txInit <- genInitTx ctx
  commits <- genCommits ctx txInit
  cctx <- pickChainContext ctx
  let (committedUTxO, stInitialized) = unsafeObserveInitAndCommits cctx txInit commits
  pure (cctx, committedUTxO, stInitialized, collect cctx stInitialized)

genCloseTx :: Int -> Gen (ChainContext, OpenState, Tx, ConfirmedSnapshot Tx)
genCloseTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen) <- genStOpen ctx
  snapshot <- genConfirmedSnapshot 0 u0 (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod cp
  pure (cctx, stOpen, close cctx stOpen snapshot startSlot pointInTime, snapshot)

genContestTx :: Gen (HydraContext, PointInTime, ClosedState, Tx)
genContestTx = do
  ctx <- genHydraContextFor maximumNumberOfParties
  (u0, stOpen) <- genStOpen ctx
  confirmed <- genConfirmedSnapshot 0 u0 []
  cctx <- pickChainContext ctx
  let cp = Hydra.Chain.Direct.State.contestationPeriod cctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  let txClose = close cctx stOpen confirmed startSlot closePointInTime
  let stClosed = snd $ fromJust $ observeClose stOpen txClose
  utxo <- arbitrary
  contestSnapshot <- genConfirmedSnapshot (succ $ number $ getSnapshot confirmed) utxo (ctxHydraSigningKeys ctx)
  contestPointInTime <- genPointInTimeBefore (getContestationDeadline stClosed)
  pure (ctx, closePointInTime, stClosed, contest cctx stClosed contestSnapshot contestPointInTime)

genFanoutTx :: Int -> Int -> Gen (HydraContext, ClosedState, Tx)
genFanoutTx numParties numOutputs = do
  ctx <- genHydraContext numParties
  utxo <- genUTxOAdaOnlyOfSize numOutputs
  (_, toFanout, stClosed) <- genStClosed ctx utxo
  let deadlineSlotNo = slotNoFromUTCTime (getContestationDeadline stClosed)
  pure (ctx, stClosed, fanout stClosed toFanout deadlineSlotNo)

getContestationDeadline :: ClosedState -> UTCTime
getContestationDeadline
  ClosedState{closedThreadOutput = ClosedThreadOutput{closedContestationDeadline}} =
    posixToUTCTime closedContestationDeadline

genStOpen ::
  HydraContext ->
  Gen (UTxO, OpenState)
genStOpen ctx = do
  txInit <- genInitTx ctx
  commits <- genCommits ctx txInit
  cctx <- pickChainContext ctx
  let (committed, stInitial) = unsafeObserveInitAndCommits cctx txInit commits
  let txCollect = collect cctx stInitial
  pure (fold committed, snd . fromJust $ observeCollect stInitial txCollect)

genStClosed ::
  HydraContext ->
  UTxO ->
  Gen (SnapshotNumber, UTxO, ClosedState)
genStClosed ctx utxo = do
  (u0, stOpen) <- genStOpen ctx
  confirmed <- arbitrary
  let (sn, snapshot, toFanout) = case confirmed of
        InitialSnapshot{} ->
          ( 0
          , InitialSnapshot{initialUTxO = u0}
          , u0
          )
        ConfirmedSnapshot{snapshot = snap, signatures} ->
          ( number snap
          , ConfirmedSnapshot
              { snapshot = snap{utxo = utxo}
              , signatures
              }
          , utxo
          )
  cctx <- pickChainContext ctx
  let cp = Hydra.Chain.Direct.State.contestationPeriod cctx
  (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod cp
  let txClose = close cctx stOpen snapshot startSlot pointInTime
  pure (sn, toFanout, snd . fromJust $ observeClose stOpen txClose)
-- ** Danger zone

unsafeCommit ::
  HasCallStack =>
  ChainContext ->
  InitialState ->
  UTxO ->
  Tx
unsafeCommit ctx st u =
  either (error . show) id $ commit ctx st u

unsafeObserveInitAndCommits ::
  ChainContext ->
  Tx ->
  [Tx] ->
  ([UTxO], InitialState)
unsafeObserveInitAndCommits ctx txInit commits =
  (utxo, stInitial')
 where
  (_, stInitial) = fromJust $ observeInit ctx txInit
  (utxo, stInitial') = flip runState stInitial $ do
    forM commits $ \txCommit -> do
      st <- get
      let (event, st') = fromJust $ observeCommit ctx st txCommit
      put st'
      pure $ case event of
        OnCommitTx{committed} -> committed
        _ -> mempty
