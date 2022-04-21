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
  commit,
  idleOnChainHeadState,
  initialize,
  observeTx,
 )
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genVerificationKey, renderTx)
import Hydra.Party (Party)
import qualified Hydra.Party as Hydra
import Test.QuickCheck (choose, elements, frequency)

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
ctxParties = fmap Hydra.deriveParty . ctxHydraSigningKeys

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
  startPoint <- arbitrary
  let ctxHydraSigningKeys = Hydra.generateKey . fromIntegral <$> [startPoint .. startPoint + n]
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
