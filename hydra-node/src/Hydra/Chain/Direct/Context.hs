module Hydra.Chain.Direct.Context where

import Hydra.Prelude

import Data.List ((\\))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  NetworkId (..),
  NetworkMagic (..),
  PaymentKey,
  SigningKey,
  Tx,
  UTxO,
  VerificationKey,
 )
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ClosedState (ClosedState),
  InitialState,
  OpenState,
  close,
  closedThreadOutput,
  collect,
  commit,
  fanout,
  initialize,
  observeClose,
  observeCollect,
  observeCommit,
  observeInit,
 )
import Hydra.Chain.Direct.Tx (ClosedThreadOutput (ClosedThreadOutput), closedContestationDeadline)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.Data.ContestationPeriod (posixToUTCTime)
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genUTxOAdaOnlyOfSize, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genPointInTime, slotNoFromUTCTime)
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, genConfirmedSnapshot)
import Test.QuickCheck (choose, elements, frequency, vector)

-- TODO: Move this to test code as it assumes global knowledge
-- (ctxHydraSigningKeys). Also all of these functions are "unsafe" and fail on
-- not matching assumptions

-- | Define some 'global' context from which generators can pick
-- values for generation. This allows to write fairly independent generators
-- which however still make sense with one another within the context of a head.
--
-- For example, one can generate a head's _party_ from that global list, whereas
-- other functions may rely on all parties and thus, we need both generation to
-- be coherent.
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

--
-- Generators
--

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
  Gen InitialState
genStInitial ctx = do
  seedInput <- genTxIn
  cctx <- pickChainContext ctx
  let initTx = initialize cctx (ctxHeadParameters ctx) seedInput
  pure . snd . fromJust $ observeInit cctx initTx

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
genCommits ctx initTx = do
  allChainContexts <- deriveChainContexts ctx
  forM allChainContexts $ \cctx -> do
    let (_, stInitial) = fromJust $ observeInit cctx initTx
    unsafeCommit stInitial <$> genCommit

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]

genCloseTx :: Int -> Gen (OpenState, Tx, ConfirmedSnapshot Tx)
genCloseTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen) <- genStOpen ctx
  snapshot <- genConfirmedSnapshot 0 u0 (ctxHydraSigningKeys ctx)
  pointInTime <- genPointInTime
  pure (stOpen, close stOpen snapshot pointInTime, snapshot)

genFanoutTx :: Int -> Int -> Gen (ClosedState, Tx)
genFanoutTx numParties numOutputs = do
  ctx <- genHydraContext numParties
  utxo <- genUTxOAdaOnlyOfSize numOutputs
  (_, toFanout, stClosed) <- genStClosed ctx utxo
  let deadlineSlotNo = slotNoFromUTCTime (getContestationDeadline stClosed)
  pure (stClosed, fanout stClosed toFanout deadlineSlotNo)

getContestationDeadline :: ClosedState -> UTCTime
getContestationDeadline
  ClosedState{closedThreadOutput = ClosedThreadOutput{closedContestationDeadline}} =
    posixToUTCTime closedContestationDeadline

genStOpen ::
  HydraContext ->
  Gen (UTxO, OpenState)
genStOpen ctx = do
  initTx <- genInitTx ctx
  commits <- genCommits ctx initTx
  cctx <- pickChainContext ctx
  let (committed, stInitial) = unsafeObserveInitAndCommits cctx initTx commits
  let collectComTx = collect stInitial
  pure (fold committed, snd . fromJust $ observeCollect stInitial collectComTx)

genStClosed ::
  HydraContext ->
  UTxO ->
  Gen (SnapshotNumber, UTxO, ClosedState)
genStClosed ctx utxo = do
  (u0, stOpen) <- genStOpen ctx
  confirmed <- arbitrary
  let (sn, snapshot, toFanout) = case confirmed of
        cf@InitialSnapshot{snapshot = s} ->
          ( 0
          , cf{snapshot = s{utxo = u0}}
          , u0
          )
        cf@ConfirmedSnapshot{snapshot = s} ->
          ( number s
          , cf{snapshot = s{utxo = utxo}}
          , utxo
          )
  pointInTime <- genPointInTime
  let closeTx = close stOpen snapshot pointInTime
  pure (sn, toFanout, snd . fromJust $ observeClose stOpen closeTx)

--
-- Here be dragons
--

unsafeCommit ::
  HasCallStack =>
  InitialState ->
  UTxO ->
  Tx
unsafeCommit st u =
  either (error . show) id $ commit st u

unsafeObserveInitAndCommits ::
  ChainContext ->
  Tx ->
  [Tx] ->
  ([UTxO], InitialState)
unsafeObserveInitAndCommits ctx initTx commits =
  (utxo, stInitial')
 where
  (_, stInitial) = fromJust $ observeInit ctx initTx
  (utxo, stInitial') = flip runState stInitial $ do
    forM commits $ \commitTx -> do
      st <- get
      let (event, st') = fromJust $ observeCommit st commitTx
      put st'
      pure $ case event of
        OnCommitTx{committed} -> committed
        _ -> mempty
