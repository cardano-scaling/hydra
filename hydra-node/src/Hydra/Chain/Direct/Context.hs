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
  idleOnChainHeadState,
  initialize,
  observeTx,
 )
import qualified Hydra.Crypto as Hydra
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genVerificationKey, renderTx)
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), genConfirmedSnapshot, getSnapshot)
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
  , ctxContestationPeriod :: DiffTime
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
  Gen [Tx]
genCommits ctx initTx = do
  forM (zip (ctxVerificationKeys ctx) (ctxParties ctx)) $ \(vk, p) -> do
    let peerVerificationKeys = ctxVerificationKeys ctx \\ [vk]
    let stIdle = idleOnChainHeadState (ctxNetworkId ctx) peerVerificationKeys vk p
    let (_, stInitialized) = unsafeObserveTx @_ @ 'StInitialized initTx stIdle
    utxo <- genCommit
    pure $ unsafeCommit utxo stInitialized

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
  commits <- genCommits ctx initTx
  stIdle <- genStIdle ctx
  let stInitialized = executeCommits initTx commits stIdle
  pure (stInitialized, collect stInitialized)

genCloseTx :: Int -> Gen (OnChainHeadState 'StOpen, Tx)
genCloseTx numParties = do
  ctx <- genHydraContextFor numParties
  stOpen <- genStOpen ctx
  -- FIXME: this is problematic for generated 'InitialSnapshot' values, because
  -- then the 'utxo' contained in here needs to be consistent with the utxo of
  -- the generated open state.
  snapshot <- genConfirmedSnapshot (ctxHydraSigningKeys ctx)
  pure (stOpen, close snapshot stOpen)

genStOpen ::
  HydraContext ->
  Gen (OnChainHeadState 'StOpen)
genStOpen ctx = do
  initTx <- genInitTx ctx
  commits <- genCommits ctx initTx
  stInitialized <- executeCommits initTx commits <$> genStIdle ctx
  let collectComTx = collect stInitialized
  pure $ snd $ unsafeObserveTx @_ @ 'StOpen collectComTx stInitialized

genStClosed ::
  HydraContext ->
  UTxO ->
  Gen (OnChainHeadState 'StClosed)
genStClosed ctx utxo = do
  stOpen <- genStOpen ctx
  -- Any confirmed snapshot suffices here, no signatures are checked
  confirmed <- arbitrary
  let snapshot = confirmed{snapshot = (getSnapshot confirmed){utxo = utxo}}
  let closeTx = close snapshot stOpen
  pure $ snd $ unsafeObserveTx @_ @ 'StClosed closeTx stOpen

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
