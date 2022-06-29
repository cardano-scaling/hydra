{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Context where

import Hydra.Prelude

import Data.List ((\\))
import Hydra.Cardano.Api (
  NetworkId (..),
  NetworkMagic (..),
  PaymentKey,
  Tx,
  UTxO,
  VerificationKey,
 )
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.State (
  HeadStateKind (..),
  ObserveTx,
  OnChainHeadState,
  close,
  collect,
  commit,
  contest,
  fanout,
  getContestationDeadline,
  idleOnChainHeadState,
  initialize,
  observeTx,
 )
import qualified Hydra.Crypto as Hydra
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genUTxOAdaOnlyOfSize, genVerificationKey, renderTx)
import Hydra.Ledger.Cardano.Evaluate (genPointInTime, genPointInTimeAfter, genPointInTimeBefore)
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, genConfirmedSnapshot)
import Test.QuickCheck (choose, elements, frequency, vector)

-- | Define some 'global' context from which generators can pick
-- values for generation. This allows to write fairly independent generators
-- which however still make sense with one another within the context of a head.
--
-- For example, one can generate a head's _party_ from that global list, whereas
-- other functions may rely on all parties and thus, we need both generation to
-- be coherent.
data HydraContext = HydraContext
  { ctxVerificationKeys :: [VerificationKey PaymentKey]
  , ctxHydraSigningKeys :: [Hydra.SigningKey]
  , ctxNetworkId :: NetworkId
  , ctxContestationPeriod :: NominalDiffTime
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
  ctxHydraSigningKeys <- fmap Hydra.generateSigningKey <$> vector n
  ctxNetworkId <- Testnet . NetworkMagic <$> arbitrary
  ctxContestationPeriod <- arbitrary
  pure $
    HydraContext
      { ctxVerificationKeys
      , ctxHydraSigningKeys
      , ctxNetworkId
      , ctxContestationPeriod
      }

genStIdle ::
  HydraContext ->
  Gen (OnChainHeadState 'StIdle)
genStIdle ctx@HydraContext{ctxVerificationKeys, ctxNetworkId} = do
  ownParty <- elements (ctxParties ctx)
  ownVerificationKey <- elements ctxVerificationKeys
  let peerVerificationKeys = ctxVerificationKeys \\ [ownVerificationKey]
  pure $ idleOnChainHeadState ctxNetworkId peerVerificationKeys ownVerificationKey ownParty

genStInitialized ::
  HydraContext ->
  Gen (OnChainHeadState 'StInitialized)
genStInitialized ctx = do
  stIdle <- genStIdle ctx
  seedInput <- genTxIn
  let initTx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
  pure $ snd $ unsafeObserveTx @_ @ 'StInitialized initTx stIdle

genInitTx ::
  HydraContext ->
  Gen Tx
genInitTx ctx =
  initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx)
    <$> genTxIn
    <*> genStIdle ctx

genCommits ::
  HydraContext ->
  Tx ->
  Gen (UTxO, [Tx])
genCommits ctx initTx = do
  commits <- forM (zip (ctxVerificationKeys ctx) (ctxParties ctx)) $ \(vk, p) -> do
    let peerVerificationKeys = ctxVerificationKeys ctx \\ [vk]
    let stIdle = idleOnChainHeadState (ctxNetworkId ctx) peerVerificationKeys vk p
    let (_, stInitialized) = unsafeObserveTx @_ @ 'StInitialized initTx stIdle
    utxo <- genCommit
    pure (utxo, unsafeCommit utxo stInitialized)
  pure (foldMap fst commits, map snd commits)

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]

genCollectComTx :: Int -> Gen (OnChainHeadState 'StInitialized, Tx)
genCollectComTx numParties = do
  ctx <- genHydraContextFor numParties
  initTx <- genInitTx ctx
  (_, commits) <- genCommits ctx initTx
  stIdle <- genStIdle ctx
  let (_, stInitialized) = executeCommits initTx commits stIdle
  pure (stInitialized, collect stInitialized)

genCloseTx :: Int -> Gen (OnChainHeadState 'StOpen, Tx, ConfirmedSnapshot Tx)
genCloseTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen) <- genStOpen ctx
  snapshot <- genConfirmedSnapshot 0 u0 (ctxHydraSigningKeys ctx)
  pointInTime <- genPointInTime
  pure (stOpen, close snapshot pointInTime stOpen, snapshot)

genContestTx :: Int -> Gen (OnChainHeadState 'StClosed, Tx)
genContestTx numParties = do
  ctx <- genHydraContextFor numParties
  utxo <- arbitrary
  (closedSnapshotNumber, _, stClosed) <- genStClosed ctx utxo
  snapshot <- genConfirmedSnapshot (succ closedSnapshotNumber) utxo (ctxHydraSigningKeys ctx)
  pointInTime <- genPointInTimeBefore (getContestationDeadline stClosed)
  pure (stClosed, contest snapshot pointInTime stClosed)

genFanoutTx :: Int -> Int -> Gen (OnChainHeadState 'StClosed, Tx)
genFanoutTx numParties numOutputs = do
  ctx <- genHydraContext numParties
  utxo <- genUTxOAdaOnlyOfSize numOutputs
  (_, toFanout, stClosed) <- genStClosed ctx utxo
  pointInTime <- genPointInTimeAfter (getContestationDeadline stClosed)
  pure (stClosed, fanout toFanout pointInTime stClosed)

genStOpen ::
  HydraContext ->
  Gen (UTxO, OnChainHeadState 'StOpen)
genStOpen ctx = do
  initTx <- genInitTx ctx
  (_, commits) <- genCommits ctx initTx
  (committed, stInitialized) <- executeCommits initTx commits <$> genStIdle ctx
  let collectComTx = collect stInitialized
  pure (committed, snd $ unsafeObserveTx @_ @ 'StOpen collectComTx stInitialized)

genStClosed ::
  HydraContext ->
  UTxO ->
  Gen (SnapshotNumber, UTxO, OnChainHeadState 'StClosed)
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
  let closeTx = close snapshot pointInTime stOpen
  pure (sn, toFanout, snd $ unsafeObserveTx @_ @ 'StClosed closeTx stOpen)

--
-- Here be dragons
--

unsafeObserveTx ::
  forall st st'.
  (ObserveTx st st', HasCallStack) =>
  Tx ->
  OnChainHeadState st ->
  (OnChainTx Tx, OnChainHeadState st')
unsafeObserveTx tx st =
  fromMaybe (error hopefullyInformativeMessage) (observeTx @st @st' tx st)
 where
  hopefullyInformativeMessage =
    "unsafeObserveTx:"
      <> "\n  From:\n    "
      <> show st
      <> "\n  Via:\n    "
      <> renderTx tx

unsafeCommit ::
  HasCallStack =>
  UTxO ->
  OnChainHeadState 'StInitialized ->
  Tx
unsafeCommit u =
  either (error . show) id . commit u

executeCommits ::
  Tx ->
  [Tx] ->
  OnChainHeadState 'StIdle ->
  (UTxO, OnChainHeadState 'StInitialized)
executeCommits initTx commits stIdle =
  (mconcat utxo, stInitialized')
 where
  (_, stInitialized) = unsafeObserveTx @_ @ 'StInitialized initTx stIdle
  (utxo, stInitialized') = flip runState stInitialized $ do
    forM commits $ \commitTx -> do
      st <- get
      let (event, st') = unsafeObserveTx @_ @ 'StInitialized commitTx st
      put st'
      pure $ case event of
        OnCommitTx{committed} -> committed
        _ -> mempty
