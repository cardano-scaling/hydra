{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.List (intersect)
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.State (
  HeadStateKind (..),
  OnChainHeadState,
  commit,
  getKnownUTxO,
  idleOnChainHeadState,
  initialize,
  transition,
 )
import Hydra.Ledger.Cardano (genOneUTxOFor, genTxIn, genVerificationKey)
import Hydra.Party (Party)
import Test.QuickCheck (
  Property,
  Testable,
  choose,
  elements,
  forAll,
  vector,
  (==>),
 )

spec :: Spec
spec = parallel $ do
  describe "init" $ do
    prop "is observed" $
      forAllInit $ \stIdle tx ->
        isJust (transition @_ @'StInitialized tx stIdle)

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
             in isNothing (transition @_ @'StInitialized tx stIdleB)

  describe "commit" $ do
    prop "is observed" $
      forAllCommit $ \stInitialized tx ->
        isJust (transition @_ @'StInitialized tx stInitialized)

    prop "consumes all inputs that are committed" $
      forAllCommit $ \st tx ->
         case transition @_ @'StInitialized tx st of
            Just (_, st') ->
              let knownInputs = UTxO.inputSet (getKnownUTxO st')
               in knownInputs `Set.disjoint` txInputSet tx
            Nothing ->
              False

    prop "can only be applied / observed once" $
      forAllCommit $ \st tx ->
         case transition @_ @'StInitialized tx st of
            Just (_, st') ->
              case transition @_ @'StInitialized tx st' of
                Just{} -> False
                Nothing -> True
            Nothing ->
              False

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
      forAll genSingleUTxO $ \utxo ->
        let tx = unsafeCommit utxo stInitialized
         in action stInitialized tx

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
  case transition @_ @'StInitialized initTx stIdle of
    Nothing -> error "failed to observe arbitrarily generated init tx?"
    Just (_, st') ->
      pure st'

genSingleUTxO :: Gen UTxO
genSingleUTxO =
  genVerificationKey >>= genOneUTxOFor

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

unsafeCommit :: HasCallStack => UTxO -> OnChainHeadState 'StInitialized -> Tx
unsafeCommit u =
  either (error . show) id . commit u
