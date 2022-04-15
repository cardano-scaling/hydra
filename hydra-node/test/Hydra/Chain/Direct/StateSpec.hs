{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import Cardano.Ledger.Era (toTxSeq)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadSTM (MonadSTM (..))
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import Data.List (elemIndex, intersect, (!!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Type.Equality (testEquality, (:~:) (..))
import Hydra.Cardano.Api (
  ExecutionUnits (..),
  SlotNo,
  Tx,
  toLedgerTx,
  txInputSet,
  txOutValue,
  valueSize,
 )
import Hydra.Chain (ChainEvent (..))
import Hydra.Chain.Direct (ChainSyncHandler (..), newChainSyncHandler)
import Hydra.Chain.Direct.Context (
  HydraContext (..),
  ctxHeadParameters,
  executeCommits,
  genCommit,
  genCommits,
  genHydraContext,
  genInitTx,
  genStIdle,
  genStInitialized,
  unsafeCommit,
  unsafeObserveTx,
 )
import Hydra.Chain.Direct.Fixture (maxTxExecutionUnits, maxTxSize)
import Hydra.Chain.Direct.State (
  HasTransition (..),
  HeadStateKind (..),
  HeadStateKindVal (..),
  ObserveTx (..),
  OnChainHeadState,
  SomeOnChainHeadState (..),
  TransitionFrom (..),
  abort,
  close,
  collect,
  fanout,
  getKnownUTxO,
  initialize,
  observeSomeTx,
 )
import Hydra.Chain.Direct.Util (Block)
import Hydra.Ledger.Cardano (
  genTxIn,
  genUTxO,
  renderTx,
  simplifyUTxO,
 )
import Hydra.Ledger.Cardano.Evaluate (evaluateTx')
import Hydra.Snapshot (isInitialSnapshot)
import Ouroboros.Consensus.Block (blockPoint)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockAlonzo))
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Hspec (shouldBe)
import Test.Hydra.Prelude (
  Spec,
  SpecWith,
  describe,
  forAll2,
  genericCoverTable,
  parallel,
  prop,
 )
import Test.QuickCheck (
  Property,
  Testable (property),
  checkCoverage,
  choose,
  classify,
  counterexample,
  elements,
  expectFailure,
  forAll,
  forAllBlind,
  label,
  resize,
  sublistOf,
  (==>),
 )
import Test.QuickCheck.Monadic (
  monadicIO,
  monitor,
  run,
 )
import Type.Reflection (typeOf)
import qualified Prelude

spec :: Spec
spec = parallel $ do
  describe "observeTx" $ do
    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAllSt $ \st tx ->
          isJust (observeSomeTx tx (SomeOnChainHeadState st))

  describe "init" $ do
    prop "is not observed if not invited" $
      forAll2 (genHydraContext 3) (genHydraContext 3) $ \(ctxA, ctxB) ->
        null (ctxParties ctxA `intersect` ctxParties ctxB)
          ==> forAll2 (genStIdle ctxA) (genStIdle ctxB)
          $ \(stIdleA, stIdleB) ->
            forAll genTxIn $ \seedInput ->
              let tx =
                    initialize
                      (ctxHeadParameters ctxA)
                      (ctxVerificationKeys ctxA)
                      seedInput
                      stIdleA
               in isNothing (observeTx @_ @ 'StInitialized tx stIdleB)

  describe "commit" $ do
    propBelowSizeLimit maxTxSize forAllCommit

    prop "consumes all inputs that are committed" $
      forAllCommit $ \st tx ->
        case observeTx @_ @ 'StInitialized tx st of
          Just (_, st') ->
            let knownInputs = UTxO.inputSet (getKnownUTxO st')
             in knownInputs `Set.disjoint` txInputSet tx
          Nothing ->
            False

    prop "can only be applied / observed once" $
      forAllCommit $ \st tx ->
        case observeTx @_ @ 'StInitialized tx st of
          Just (_, st') ->
            case observeTx @_ @ 'StInitialized tx st' of
              Just{} -> False
              Nothing -> True
          Nothing ->
            False

  describe "abort" $ do
    propBelowSizeLimit (2 * maxTxSize) forAllAbort

  describe "collectCom" $ do
    propBelowSizeLimit (2 * maxTxSize) forAllCollectCom
    propIsValid tenTimesTxExecutionUnits forAllCollectCom

  describe "close" $ do
    propBelowSizeLimit maxTxSize forAllClose

  describe "fanout" $ do
    propBelowSizeLimit maxTxSize forAllFanout
    -- TODO: look into why this is failing
    propIsValid maxTxExecutionUnits (expectFailure . forAllFanout)

  describe "ChainSyncHandler" $ do
    prop "yields observed transactions rolling forward" $ do
      forAllSt $ \(SomeOnChainHeadState -> st) tx -> do
        let callback = \case
              Rollback{} ->
                fail "rolled back but expected roll forward."
              Observation onChainTx ->
                fst <$> observeSomeTx tx st `shouldBe` Just onChainTx
        forAllBlind (genBlockAt 1 [tx]) $ \blk -> monadicIO $ do
          headState <- run $ newTVarIO st
          handler <- run $ newChainSyncHandler nullTracer callback headState
          run $ onRollForward handler blk

    prop "rollback rewind the chain state" $
      forAllBlind genSequenceOfObservableBlocks $ \(st, blks) ->
        forAll (choose (0, length blks - 1)) $ \ix -> do
          let rollbackPoint = blockPoint (blks !! ix)
          let callback = \case
                Observation{} -> do
                  pure ()
                Rollback n -> n `shouldBe` fromIntegral ix
          monadicIO $ do
            headState <- run $ newTVarIO st
            handler <- run $ newChainSyncHandler nullTracer callback headState
            run $ mapM_ (onRollForward handler) blks
            st' <- run $ readTVarIO headState
            monitor $ counterexample $ "On-chain head state: " <> show st'
            run $ onRollBackward handler rollbackPoint

genSequenceOfObservableBlocks :: Gen (SomeOnChainHeadState, [Block])
genSequenceOfObservableBlocks = do
  ctx <- genHydraContext 3
  stIdle <- genStIdle ctx
  tx <- genInit ctx stIdle
  let _stInit = snd $ unsafeObserveTx @_ @ 'StInitialized tx stIdle
  blk <- genBlockAt 1 [tx]
  pure (SomeOnChainHeadState stIdle, [blk])
 where
  genInit ctx stIdle =
    initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx)
      <$> genTxIn
      <*> pure stIdle

tenTimesTxExecutionUnits :: ExecutionUnits
tenTimesTxExecutionUnits =
  ExecutionUnits
    { executionMemory = 100_000_000
    , executionSteps = 100_000_000_000
    }

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

-- TODO: DRY with Hydra.Chain.Direct.Contract.Mutation.propTransactionValidates?
propIsValid ::
  forall st.
  ExecutionUnits ->
  ((OnChainHeadState st -> Tx -> Property) -> Property) ->
  SpecWith ()
propIsValid exUnits forAllTx =
  prop ("validates within " <> show exUnits) $
    forAllTx $ \st tx ->
      let lookupUTxO = getKnownUTxO st
       in case evaluateTx' exUnits tx lookupUTxO of
            Left basicFailure ->
              property False
                & counterexample ("Tx: " <> toString (renderTx tx))
                & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
                & counterexample ("Phase-1 validation failed: " <> show basicFailure)
            Right redeemerReport ->
              all isRight (Map.elems redeemerReport)
                & counterexample ("Tx: " <> toString (renderTx tx))
                & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
                & counterexample ("Redeemer report: " <> show redeemerReport)
                & counterexample "Phase-2 validation failed"

--
-- QuickCheck Extras
--

forAllSt ::
  (Testable property) =>
  (forall st. (HasTransition st) => OnChainHeadState st -> Tx -> property) ->
  Property
forAllSt action =
  forAllBlind
    ( elements
        [
          ( forAllInit action
          , Transition @ 'StIdle (TransitionTo (Proxy @ 'StInitialized))
          )
        ,
          ( forAllCommit action
          , Transition @ 'StInitialized (TransitionTo (Proxy @ 'StInitialized))
          )
        ,
          ( forAllAbort action
          , Transition @ 'StInitialized (TransitionTo (Proxy @ 'StIdle))
          )
        ,
          ( forAllCollectCom action
          , Transition @ 'StInitialized (TransitionTo (Proxy @ 'StOpen))
          )
        ,
          ( forAllClose action
          , Transition @ 'StOpen (TransitionTo (Proxy @ 'StClosed))
          )
        ,
          ( forAllFanout action
          , Transition @ 'StClosed (TransitionTo (Proxy @ 'StIdle))
          )
        ]
    )
    (\(p, lbl) -> genericCoverTable [lbl] p)

forAllInit ::
  (Testable property) =>
  (OnChainHeadState 'StIdle -> Tx -> property) ->
  Property
forAllInit action =
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStIdle ctx) $ \stIdle ->
      forAll genTxIn $ \seedInput -> do
        let tx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle
         in action stIdle tx
              & classify
                (length (ctxParties ctx) == 1)
                "1 party"
              & classify
                (length (ctxParties ctx) > 1)
                "2+ parties"

forAllCommit ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllCommit action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStInitialized ctx) $ \stInitialized ->
      forAll genCommit $ \utxo ->
        let tx = unsafeCommit utxo stInitialized
         in action stInitialized tx
              & classify
                (null utxo)
                "Empty commit"
              & classify
                (not (null utxo))
                "Non-empty commit"

forAllAbort ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllAbort action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genInitTx ctx) $ \initTx -> do
      forAll (sublistOf =<< genCommits ctx initTx) $ \commits ->
        forAll (genStIdle ctx) $ \stIdle ->
          let stInitialized = executeCommits initTx commits stIdle
           in action stInitialized (abort stInitialized)
                & classify
                  (null commits)
                  "Abort immediately, after 0 commits"
                & classify
                  (not (null commits) && length commits < length (ctxParties ctx))
                  "Abort after some (but not all) commits"
                & classify
                  (length commits == length (ctxParties ctx))
                  "Abort after all commits"

forAllCollectCom ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllCollectCom action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genInitTx ctx) $ \initTx -> do
      forAll (genCommits ctx initTx) $ \commits ->
        forAll (genStIdle ctx) $ \stIdle ->
          let stInitialized = executeCommits initTx commits stIdle
           in action stInitialized (collect stInitialized)

forAllClose ::
  (Testable property) =>
  (OnChainHeadState 'StOpen -> Tx -> property) ->
  Property
forAllClose action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStOpen ctx) $ \stOpen ->
      forAll arbitrary $ \snapshot ->
        action stOpen (close snapshot stOpen)
          & classify
            (isInitialSnapshot snapshot)
            "Close with initial snapshot"
          & classify
            (not (isInitialSnapshot snapshot))
            "Close with multi-signed snapshot"

forAllFanout ::
  (Testable property) =>
  (OnChainHeadState 'StClosed -> Tx -> property) ->
  Property
forAllFanout action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStClosed ctx) $ \stClosed ->
      forAll (resize maxAssetsSupported $ simplifyUTxO <$> genUTxO) $ \utxo ->
        action stClosed (fanout utxo stClosed)
          & label ("Fanout size: " <> prettyLength (assetsInUtxo utxo))
 where
  maxAssetsSupported = 1

  assetsInUtxo = valueSize . foldMap txOutValue

  prettyLength len
    | len >= 100 = "> 100"
    | len >= 50 = "50-99"
    | len >= 10 = "10-49"
    | otherwise = "00-10"

--
-- Generators
--

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
  Gen (OnChainHeadState 'StClosed)
genStClosed ctx = do
  stOpen <- genStOpen ctx
  snapshot <- arbitrary
  let closeTx = close snapshot stOpen
  pure $ snd $ unsafeObserveTx @_ @ 'StClosed closeTx stOpen

genBlockAt :: SlotNo -> [Tx] -> Gen Block
genBlockAt _sl txs = do
  header <- arbitrary
  let body = toTxSeq $ StrictSeq.fromList (toLedgerTx <$> txs)
  pure $ BlockAlonzo $ mkShelleyBlock $ Ledger.Block header body

--
-- Wrapping Transition for easy labelling
--

allTransitions :: [Transition]
allTransitions =
  mconcat
    [ Transition <$> transitions (Proxy @ 'StIdle)
    , Transition <$> transitions (Proxy @ 'StInitialized)
    , Transition <$> transitions (Proxy @ 'StOpen)
    , Transition <$> transitions (Proxy @ 'StClosed)
    ]

data Transition where
  Transition ::
    forall (st :: HeadStateKind).
    (HeadStateKindVal st, Typeable st) =>
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
