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
import Hydra.Chain (HeadParameters (..), OnChainTx)
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
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genUTxO, genVerificationKey, renderTx, simplifyUTxO)
import Hydra.Ledger.Cardano.Evaluate (genPointInTime, slotNoToPOSIXTime)
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, genConfirmedSnapshot, getSnapshot)
import Test.QuickCheck (choose, elements, frequency, resize, suchThat, vector)

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
  let stInitialized = executeCommits initTx commits stIdle
  pure (stInitialized, collect stInitialized)

genCloseTx :: Int -> Gen (OnChainHeadState 'StOpen, Tx)
genCloseTx numParties = do
  ctx <- genHydraContextFor numParties
  (utxo, stOpen) <- genStOpen ctx
  snapshot <- genConfirmedSnapshot 0 utxo (ctxHydraSigningKeys ctx)
  pointInTime <- genPointInTime
  pure (stOpen, close snapshot pointInTime stOpen)

genContestTx :: Int -> Gen (OnChainHeadState 'StClosed, Tx)
genContestTx numParties = do
  ctx <- genHydraContextFor numParties
  utxo <- arbitrary
  (closedSnapshotNumber, stClosed) <- genStClosed ctx utxo
  snapshot <- genConfirmedSnapshot closedSnapshotNumber utxo (ctxHydraSigningKeys ctx)
  pointInTime <-
    genPointInTime `suchThat` \(slot, _) ->
      slotNoToPOSIXTime slot < getContestationDeadline stClosed
  pure (stClosed, contest snapshot pointInTime stClosed)

genFanoutTx :: Int -> Gen (OnChainHeadState 'StClosed, Tx)
genFanoutTx numParties = do
  ctx <- genHydraContext numParties
  let maxAssetsSupported = 1
  utxo <- resize maxAssetsSupported $ simplifyUTxO <$> genUTxO
  (_, stClosed) <- genStClosed ctx utxo
  pointInTime <-
    genPointInTime `suchThat` \(slot, _) ->
      slotNoToPOSIXTime slot > getContestationDeadline stClosed
  pure (stClosed, fanout utxo pointInTime stClosed)

genStOpen ::
  HydraContext ->
  Gen (UTxO, OnChainHeadState 'StOpen)
genStOpen ctx = do
  initTx <- genInitTx ctx
  (utxo, commits) <- genCommits ctx initTx
  stInitialized <- executeCommits initTx commits <$> genStIdle ctx
  let collectComTx = collect stInitialized
  pure (utxo, snd $ unsafeObserveTx @_ @ 'StOpen collectComTx stInitialized)

genStClosed ::
  HydraContext ->
  UTxO ->
  Gen (SnapshotNumber, OnChainHeadState 'StClosed)
genStClosed ctx utxo = do
  (_, stOpen) <- genStOpen ctx
  -- FIXME: We need a ConfirmedSnapshot here because the utxo's in an
  -- 'InitialSnapshot' are ignored and we would not be able to fan them out
  confirmed <-
    arbitrary `suchThat` \case
      InitialSnapshot{} -> False
      ConfirmedSnapshot{} -> True
  let snapshot = confirmed{snapshot = (getSnapshot confirmed){utxo = utxo}}
      -- FIXME: this is redundant with the above filter, this whole code becomes
      -- annoyingly complicated
      sn = case confirmed of
        InitialSnapshot{} -> 0
        ConfirmedSnapshot{snapshot = Snapshot{number}} -> number
  pointInTime <- genPointInTime
  let closeTx = close snapshot pointInTime stOpen
  pure (sn, snd $ unsafeObserveTx @_ @ 'StClosed closeTx stOpen)

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
  OnChainHeadState 'StInitialized
executeCommits initTx commits stIdle =
  flip execState stInitialized $ do
    forM_ commits $ \commitTx -> do
      st <- get
      let (_, st') = unsafeObserveTx @_ @ 'StInitialized commitTx st
      put st'
 where
  (_, stInitialized) = unsafeObserveTx @_ @ 'StInitialized initTx stIdle
