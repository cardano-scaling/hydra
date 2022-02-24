{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.List (intersect)
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (..), OnChainTx)
import Hydra.Chain.Direct.State (
  ObserveTx,
  HeadStateKind (..),
  OnChainHeadState,
  collect,
  commit,
  getKnownUTxO,
  idleOnChainHeadState,
  initialize,
  observeTx,
 )
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genVerificationKey)
import Hydra.Party (Party)
import Test.QuickCheck (
  Property,
  Testable,
  choose,
  elements,
  forAll,
  frequency,
  vector,
  (==>),
 )

spec :: Spec
spec = parallel $ do
  describe "init" $ do
    prop "is observed" $
      forAllInit $ \stIdle initTx ->
        isJust (observeTx @_ @'StInitialized initTx stIdle)

    prop "is not observed if not invited" $
      forAll2 genHydraContext genHydraContext $ \(ctxA, ctxB) ->
        null (ctxParties ctxA `intersect` ctxParties ctxB) ==>
        forAll2 (genStIdle ctxA) (genStIdle ctxB) $ \(stIdleA, stIdleB) ->
          forAll genTxIn $ \seedInput ->
            let tx = initialize
                  (ctxHeadParameters ctxA)
                  (ctxVerificationKeys ctxA)
                  seedInput
                  stIdleA
             in isNothing (observeTx @_ @'StInitialized tx stIdleB)

  describe "commit" $ do
    prop "is observed" $
      forAllCommit $ \stInitialized commitTx ->
        isJust (observeTx @_ @'StInitialized commitTx stInitialized)

    prop "consumes all inputs that are committed" $
      forAllCommit $ \st tx ->
         case observeTx @_ @'StInitialized tx st of
            Just (_, st') ->
              let knownInputs = UTxO.inputSet (getKnownUTxO st')
               in knownInputs `Set.disjoint` txInputSet tx
            Nothing ->
              False

    prop "can only be applied / observed once" $
      forAllCommit $ \st tx ->
         case observeTx @_ @'StInitialized tx st of
            Just (_, st') ->
              case observeTx @_ @'StInitialized tx st' of
                Just{} -> False
                Nothing -> True
            Nothing ->
              False

  describe "collectCom" $ do
    prop "is observed" $
      forAllCollectCom $ \stInitialized collectComTx ->
        isJust (observeTx @_ @'StOpen collectComTx stInitialized)


--
-- QuickCheck Extras
--

forAllInit
  :: Testable property
  => (OnChainHeadState 'StIdle -> Tx -> property)
  -> Property
forAllInit action =
  forAll genHydraContext $ \ctx ->
    forAll (genStIdle ctx) $ \stIdle ->
      forAll genTxIn $ \seedInput -> do
        let tx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
         in action stIdle tx

forAllCommit
  :: Testable property
  => (OnChainHeadState 'StInitialized -> Tx -> property)
  -> Property
forAllCommit action = do
  forAll genHydraContext $ \ctx ->
    forAll (genStInitialized ctx) $ \stInitialized ->
      forAll genCommit $ \utxo ->
        let tx = unsafeCommit utxo stInitialized
         in action stInitialized tx

-- | Generating some arbitrary 'CollectCom' transaction with a state that is
-- ready to accept it is a bit trickier than the others. Indeed, an
-- 'OnChainHeadState' is tied to a particular 'Party' (or verification key).
--
-- But, we still need to construct commits for the other parties, and have them
-- observed by that one party we are "incarnating" via this OnChainHeadState.
--
-- Commits depend on the initial transactions, so it is important that all
-- commits are created from the same init transaction.
forAllCollectCom
  :: Testable property
  => (OnChainHeadState 'StInitialized -> Tx -> property)
  -> Property
forAllCollectCom action = do
  forAll genHydraContext $ \ctx ->
    forAll (genInitTx ctx) $ \initTx -> do
      forAll (genCommits ctx initTx) $ \commits ->
        forAll (genStIdle ctx) $ \stIdle ->
        let
            (_, stInitialized) =
              unsafeObserveTx @_ @'StInitialized initTx stIdle

            stInitialized' = flip execState stInitialized $ do
              forM_ commits $ \commitTx -> do
                st <- get
                let (_, st') = unsafeObserveTx @_ @'StInitialized commitTx st
                put st'
         in action stInitialized' (collect stInitialized')
 where
  genInitTx
    :: HydraContext
    -> Gen Tx
  genInitTx ctx =
    initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx)
      <$> genTxIn
      <*> genStIdle ctx

  genCommits
    :: HydraContext
    -> Tx
    -> Gen [Tx]
  genCommits ctx initTx = do
    forM (zip (ctxVerificationKeys ctx) (ctxParties ctx)) $ \(p, vk) -> do
      let stIdle = idleOnChainHeadState (ctxNetworkId ctx) p vk
      let (_, stInitialized) = unsafeObserveTx @_ @'StInitialized initTx stIdle
      utxo <- genCommit
      pure $ unsafeCommit utxo stInitialized

--
-- Generators
--

-- Some 'global' (to all generator) context from which generators can pick
-- values for generation. This allows to write fairly independent generators
-- which however still make sense with one another within the context of a head.
--
-- For example, one can generate a head's _party_ from that global list, whereas
-- other functions may rely on all parties and thus, we need both generation to
-- be coherent.
data HydraContext = HydraContext
  { ctxVerificationKeys :: [VerificationKey PaymentKey]
  , ctxParties :: [Party]
  , ctxNetworkId :: NetworkId
  , ctxContestationPeriod :: DiffTime
  }
  deriving (Show)

ctxHeadParameters :: HydraContext -> HeadParameters
ctxHeadParameters HydraContext{ctxContestationPeriod, ctxParties} =
  HeadParameters ctxContestationPeriod ctxParties

genHydraContext :: Gen HydraContext
genHydraContext = do
  n <- choose (1, 10)
  ctxVerificationKeys <- replicateM n genVerificationKey
  ctxParties <- vector n
  ctxNetworkId <- Testnet . NetworkMagic <$> arbitrary
  ctxContestationPeriod <- arbitrary
  pure $
    HydraContext
      { ctxVerificationKeys
      , ctxParties
      , ctxNetworkId
      , ctxContestationPeriod
      }

genStIdle :: HydraContext -> Gen (OnChainHeadState 'StIdle)
genStIdle HydraContext{ctxVerificationKeys, ctxNetworkId, ctxParties} = do
  ownParty <- elements ctxParties
  ownVerificationKey <- elements ctxVerificationKeys
  pure $ idleOnChainHeadState ctxNetworkId ownVerificationKey ownParty

genStInitialized :: HydraContext -> Gen (OnChainHeadState 'StInitialized)
genStInitialized ctx = do
  stIdle <- genStIdle ctx
  seedInput <- genTxIn
  let initTx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
  pure $ snd $ unsafeObserveTx @_ @'StInitialized initTx stIdle

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]

--
-- Helpers
--

forAll2
  :: (Testable property, Show a, Show b)
  => Gen a
  -> Gen b
  -> ((a, b) -> property)
  -> Property
forAll2 genA genB action =
  forAll genA $ \a ->
    forAll genB $ \b ->
      action (a, b)

--
-- Here be dragons
--

unsafeCommit :: HasCallStack => UTxO -> OnChainHeadState 'StInitialized -> Tx
unsafeCommit u =
  either (error . show) id . commit u

unsafeObserveTx ::
  forall st st'. (ObserveTx st st', HasCallStack) =>
  Tx ->
  OnChainHeadState st ->
  (OnChainTx Tx, OnChainHeadState st')
unsafeObserveTx tx st =
  fromMaybe (error hopefullyInformativeMessage) (observeTx @st @st' tx st)
 where
  hopefullyInformativeMessage =
    "unsafeObserveTx:"
    <> "\n  From:\n    " <> show st
    <> "\n  Via:\n    " <> renderTx tx
