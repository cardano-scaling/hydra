{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import qualified Cardano.Api.UTxO as UTxO
import qualified Data.ByteString.Lazy as LBS
import Data.List ((!!), elemIndex, intersect)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Type.Equality ((:~:)(..), testEquality)
import Hydra.Chain (HeadParameters (..), OnChainTx)
import Hydra.Chain.Direct.State (
  HasTransition (..),
  HeadStateKind (..),
  HeadStateKindVal (..),
  ObserveTx(..),
  OnChainHeadState,
  SomeOnChainHeadState (..),
  TransitionFrom (..),
  abort,
  close,
  collect,
  commit,
  fanout,
  getKnownUTxO,
  idleOnChainHeadState,
  initialize,
  observeSomeTx,
 )
import Hydra.Chain.Direct.Fixture (maxTxSize)
import Hydra.Ledger.Cardano (
  genOneUTxOFor,
  genTxIn,
  genUTxO,
  genVerificationKey,
 )
import Hydra.Party (Party)
import Hydra.Snapshot (isInitialSnapshot)
import qualified Prelude
import Test.QuickCheck (
  Property,
  Testable,
  (==>),
  checkCoverage,
  choose,
  classify,
  counterexample,
  elements,
  forAll,
  forAllBlind,
  frequency,
  label,
  sublistOf,
  vector,
 )
import Type.Reflection (typeOf)

spec :: Spec
spec = parallel $ do
  describe "observeTx" $ do
    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAllSt $ \st tx ->
          isJust (observeSomeTx tx (SomeOnChainHeadState st))

  describe "init" $ do
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
    propBelowSizeLimit maxTxSize forAllCommit

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

  describe "abort" $ do
    propBelowSizeLimit (2*maxTxSize) forAllAbort

  describe "collectCom" $ do
    propBelowSizeLimit (2*maxTxSize) forAllCollectCom

  describe "close" $ do
    propBelowSizeLimit maxTxSize forAllClose

  describe "fanout" $ do
    propBelowSizeLimit (2*maxTxSize) forAllFanout

--
-- Generic Properties
--

propBelowSizeLimit ::
  forall st.
  Int64 ->
  ((OnChainHeadState st -> Tx -> Property) -> Property) ->
  SpecWith ()
propBelowSizeLimit txSizeLimit forAllTx =
  prop ("transaction size is below " <> showKB txSizeLimit) $
    forAllTx $ \_st tx ->
      let cbor = serialize tx
          len = LBS.length cbor
       in len < txSizeLimit
            & label (showKB len)
            & counterexample (toString (renderTx tx))
            & counterexample ("Actual size: " <> show len)
 where
  showKB nb = show (nb `div` 1024) <> "kB"


--
-- QuickCheck Extras
--

forAllSt ::
  (Testable property) =>
  (forall st. (HasTransition st) => OnChainHeadState st -> Tx -> property) ->
  Property
forAllSt action =
  forAllBlind (elements
    [ ( forAllInit action
      , Transition @'StIdle (TransitionTo (Proxy @'StInitialized))
      )
    , ( forAllCommit action
      , Transition @'StInitialized (TransitionTo (Proxy @'StInitialized))
      )
    , ( forAllAbort action
      , Transition @'StInitialized (TransitionTo (Proxy @'StIdle))
      )
    , ( forAllCollectCom action
      , Transition @'StInitialized (TransitionTo (Proxy @'StOpen))
      )
    , ( forAllClose action
      , Transition @'StOpen (TransitionTo (Proxy @'StClosed))
      )
    , ( forAllFanout action
      , Transition @'StClosed (TransitionTo (Proxy @'StIdle))
      )
    ])
    (\(p, lbl) -> genericCoverTable [lbl] p)

forAllInit ::
  (Testable property) =>
  (OnChainHeadState 'StIdle -> Tx -> property) ->
  Property
forAllInit action =
  forAll genHydraContext $ \ctx ->
    forAll (genStIdle ctx) $ \stIdle ->
      forAll genTxIn $ \seedInput -> do
        let tx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
         in action stIdle tx
              & classify (length (ctxParties ctx) == 1)
                  "1 party"
              & classify (length (ctxParties ctx) > 1)
                  "2+ parties"

forAllCommit ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllCommit action = do
  forAll genHydraContext $ \ctx ->
    forAll (genStInitialized ctx) $ \stInitialized ->
      forAll genCommit $ \utxo ->
        let tx = unsafeCommit utxo stInitialized
         in action stInitialized tx
              & classify (null utxo)
                  "Empty commit"
              & classify (not (null utxo))
                  "Non-empty commit"

forAllAbort ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllAbort action = do
  forAll genHydraContext $ \ctx ->
    forAll (genInitTx ctx) $ \initTx -> do
      forAll (sublistOf =<< genCommits ctx initTx) $ \commits ->
        forAll (genStIdle ctx) $ \stIdle ->
          let stInitialized = executeCommits initTx commits stIdle
           in action stInitialized (abort stInitialized)
                & classify (null commits)
                    "Abort immediately, after 0 commits"
                & classify (not (null commits) && length commits < length (ctxParties ctx))
                    "Abort after some (but not all) commits"
                & classify (length commits == length (ctxParties ctx))
                    "Abort after all commits"

forAllCollectCom
  :: (Testable property)
  => (OnChainHeadState 'StInitialized -> Tx -> property)
  -> Property
forAllCollectCom action = do
  forAll genHydraContext $ \ctx ->
    forAll (genInitTx ctx) $ \initTx -> do
      forAll (genCommits ctx initTx) $ \commits ->
        forAll (genStIdle ctx) $ \stIdle ->
          let stInitialized = executeCommits initTx commits stIdle
           in action stInitialized (collect stInitialized)

forAllClose
  :: (Testable property)
  => (OnChainHeadState 'StOpen -> Tx -> property)
  -> Property
forAllClose action = do
  forAll genHydraContext $ \ctx ->
    forAll (genStOpen ctx) $ \stOpen ->
      forAll arbitrary $ \snapshot ->
        action stOpen (close snapshot stOpen)
          & classify (isInitialSnapshot snapshot)
              "Close with initial snapshot"
          & classify (not (isInitialSnapshot snapshot))
              "Close with multi-signed snapshot"

forAllFanout
  :: (Testable property)
  => (OnChainHeadState 'StClosed -> Tx -> property)
  -> Property
forAllFanout action = do
  forAll genHydraContext $ \ctx ->
    forAll (genStClosed ctx) $ \stClosed ->
      forAll genUTxO $ \utxo ->
        action stClosed (fanout utxo stClosed)
          & label ("Fanout size: " <> prettyLength utxo)
 where
  prettyLength :: Foldable f => f a -> String
  prettyLength (length -> len)
    | len >= 100 = "> 100"
    | len >= 50 = "50-99"
    | len >= 10 = "10-49"
    | otherwise = "00-10"

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

ctxHeadParameters ::
  HydraContext ->
  HeadParameters
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

genStIdle ::
  HydraContext ->
  Gen (OnChainHeadState 'StIdle)
genStIdle HydraContext{ctxVerificationKeys, ctxNetworkId, ctxParties} = do
  ownParty <- elements ctxParties
  ownVerificationKey <- elements ctxVerificationKeys
  pure $ idleOnChainHeadState ctxNetworkId ownVerificationKey ownParty

genStInitialized ::
  HydraContext ->
  Gen (OnChainHeadState 'StInitialized)
genStInitialized ctx = do
  stIdle <- genStIdle ctx
  seedInput <- genTxIn
  let initTx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
  pure $ snd $ unsafeObserveTx @_ @'StInitialized initTx stIdle

genStOpen ::
  HydraContext ->
  Gen (OnChainHeadState 'StOpen)
genStOpen ctx = do
  initTx <- genInitTx ctx
  commits <- genCommits ctx initTx
  stInitialized <- executeCommits initTx commits <$> genStIdle ctx
  let collectComTx = collect stInitialized
  pure $ snd $ unsafeObserveTx @_ @'StOpen collectComTx stInitialized

genStClosed ::
  HydraContext ->
  Gen (OnChainHeadState 'StClosed)
genStClosed ctx = do
  initTx <- genInitTx ctx
  commits <- genCommits ctx initTx
  stInitialized <- executeCommits initTx commits <$> genStIdle ctx
  let collectComTx = collect stInitialized
  let stOpen = snd $ unsafeObserveTx @_ @'StOpen collectComTx stInitialized
  snapshot <- arbitrary
  let closeTx = close snapshot stOpen
  pure $ snd $ unsafeObserveTx @_ @'StClosed closeTx stOpen

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
  forM (zip (ctxVerificationKeys ctx) (ctxParties ctx)) $ \(p, vk) -> do
    let stIdle = idleOnChainHeadState (ctxNetworkId ctx) p vk
    let (_, stInitialized) = unsafeObserveTx @_ @'StInitialized initTx stIdle
    utxo <- genCommit
    pure $ unsafeCommit utxo stInitialized

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]

--
-- Wrapping Transition for easy labelling
--

allTransitions :: [Transition]
allTransitions = mconcat
  [ Transition <$> transitions (Proxy @'StIdle)
  , Transition <$> transitions (Proxy @'StInitialized)
  , Transition <$> transitions (Proxy @'StOpen)
  , Transition <$> transitions (Proxy @'StClosed)
  ]

data Transition where
  Transition ::
    forall (st :: HeadStateKind). (HeadStateKindVal st, Typeable st) =>
    TransitionFrom st ->
    Transition
deriving instance Typeable Transition

instance Show Transition where
  show (Transition t) = show t

instance Eq Transition where
  (Transition from) == (Transition from') =
    case testEquality (typeOf from) (typeOf from') of
      Just Refl -> from == from'
      Nothing -> False

instance Enum Transition where
  toEnum = (!!) allTransitions
  fromEnum = fromJust . (`elemIndex` allTransitions)

instance Bounded Transition where
  minBound = Prelude.head allTransitions
  maxBound = Prelude.last allTransitions

--
-- Here be dragons
--

unsafeCommit ::
  HasCallStack =>
  UTxO ->
  OnChainHeadState 'StInitialized ->
  Tx
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

executeCommits ::
  Tx ->
  [Tx] ->
  OnChainHeadState 'StIdle ->
  OnChainHeadState 'StInitialized
executeCommits initTx commits stIdle =
  flip execState stInitialized $ do
    forM_ commits $ \commitTx -> do
      st <- get
      let (_, st') = unsafeObserveTx @_ @'StInitialized commitTx st
      put st'
 where
  (_, stInitialized) = unsafeObserveTx @_ @'StInitialized initTx stIdle
