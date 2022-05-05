{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
-- Fourmolu chokes on type-applications of promoted constructors (e.g.
-- @'StInitialized) and is unable to format properly after that. Hence this
-- option to allow using unticked constructor and save Fourmolu from dying.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Hydra.Chain.Direct.StateSpec where

import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import Cardano.Ledger.Era (toTxSeq)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadSTM (MonadSTM (..))
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import Data.List (elemIndex, intersect, (!!), (\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Type.Equality (testEquality, (:~:) (..))
import Hydra.Cardano.Api (
  ExecutionUnits (..),
  SlotNo (..),
  Tx,
  UTxO,
  blockSlotNo,
  renderUTxO,
  toLedgerTx,
  txInputSet,
  txOutValue,
  valueSize,
  pattern ByronAddressInEra,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import Hydra.Chain (ChainEvent (..), PostTxError (..))
import Hydra.Chain.Direct (
  ChainSyncHandler (..),
  RecordedAt (..),
  SomeOnChainHeadStateAt (..),
  chainSyncHandler,
 )
import Hydra.Chain.Direct.Context (
  HydraContext (..),
  ctxHeadParameters,
  ctxParties,
  executeCommits,
  genCloseTx,
  genCollectComTx,
  genCommit,
  genCommits,
  genHydraContext,
  genInitTx,
  genStClosed,
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
  commit,
  fanout,
  getKnownUTxO,
  idleOnChainHeadState,
  initialize,
  observeSomeTx,
 )
import Hydra.Chain.Direct.Util (Block)
import Hydra.Ledger.Cardano (
  genTxIn,
  genUTxO,
  genValue,
  renderTx,
  renderTxs,
  simplifyUTxO,
 )
import Hydra.Ledger.Cardano.Evaluate (evaluateTx')
import Ouroboros.Consensus.Block (Point, blockPoint)
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
  forAll,
  forAllBlind,
  forAllShow,
  label,
  resize,
  sublistOf,
  (==>),
 )
import Test.QuickCheck.Monadic (
  PropertyM,
  assert,
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
               in isNothing (observeTx @_ @StInitialized tx stIdleB)

  describe "commit" $ do
    propBelowSizeLimit maxTxSize forAllCommit

    prop "consumes all inputs that are committed" $
      forAllCommit $ \st tx ->
        case observeTx @_ @StInitialized tx st of
          Just (_, st') ->
            let knownInputs = UTxO.inputSet (getKnownUTxO st')
             in knownInputs `Set.disjoint` txInputSet tx
          Nothing ->
            False

    prop "can only be applied / observed once" $
      forAllCommit $ \st tx ->
        case observeTx @_ @StInitialized tx st of
          Just (_, st') ->
            case observeTx @_ @StInitialized tx st' of
              Just{} -> False
              Nothing -> True
          Nothing ->
            False

    prop "reject Commits of Byron outputs" $
      forAllNonEmptyByronCommit $ \case
        UnsupportedLegacyOutput{} -> property True
        _ -> property False

  describe "abort" $ do
    propBelowSizeLimit (2 * maxTxSize) forAllAbort

  describe "collectCom" $ do
    propBelowSizeLimit maxTxSize forAllCollectCom
    propIsValid maxTxExecutionUnits forAllCollectCom

  describe "close" $ do
    propBelowSizeLimit maxTxSize forAllClose
    propIsValid maxTxExecutionUnits forAllClose

  describe "fanout" $ do
    propBelowSizeLimit maxTxSize forAllFanout
    propIsValid maxTxExecutionUnits forAllFanout

  describe "ChainSyncHandler" $ do
    prop "yields observed transactions rolling forward" $ do
      forAllSt $ \(SomeOnChainHeadState -> st) tx -> do
        let callback = \case
              Rollback{} ->
                fail "rolled back but expected roll forward."
              Observation onChainTx ->
                fst <$> observeSomeTx tx st `shouldBe` Just onChainTx
        forAllBlind (genBlockAt 1 [tx]) $ \blk -> monadicIO $ do
          headState <- run $ newTVarIO $ stAtGenesis st
          let handler = chainSyncHandler nullTracer callback headState
          run $ onRollForward handler blk

    prop "can replay chain on (benign) rollback" $
      forAllBlind genSequenceOfObservableBlocks $ \(st, blks) ->
        forAllShow (genRollbackPoint blks) showRollbackInfo $ \(rollbackDepth, rollbackPoint) -> do
          let callback = \case
                Observation{} -> do
                  pure ()
                Rollback n -> n `shouldBe` rollbackDepth

          monadicIO $ do
            monitor $ label ("Rollback depth: " <> show rollbackDepth)
            headState <- run $ newTVarIO st
            let handler = chainSyncHandler nullTracer callback headState

            -- 1/ Simulate some chain following
            st' <- run $ mapM_ (onRollForward handler) blks *> readTVarIO headState

            -- 2/ Inject a rollback to somewhere between any of the previous state
            result <- withCounterExample blks headState $ do
              try @_ @SomeException $ onRollBackward handler rollbackPoint
            assert (isRight result)

            -- 3/ Simulate chain-following replaying rolled back blocks, should re-apply
            let toReplay = blks \\ [blk | blk <- blks, blockPoint blk <= rollbackPoint]
            st'' <- run $ mapM_ (onRollForward handler) toReplay *> readTVarIO headState
            assert (st' == st'')

withCounterExample :: [Block] -> TVar IO SomeOnChainHeadStateAt -> IO a -> PropertyM IO a
withCounterExample blks headState step = do
  stBefore <- run $ readTVarIO headState
  a <- run step
  stAfter <- run $ readTVarIO headState
  a <$ do
    monitor $
      counterexample $
        toString $
          unlines
            [ "Head state at (before rollback): " <> showStateRecordedAt stBefore
            , "Head state at (after rollback):  " <> showStateRecordedAt stAfter
            , "Block sequence: \n"
                <> unlines
                  ( fmap
                      ("    " <>)
                      [show (blockPoint blk) | blk <- blks]
                  )
            ]

genRollbackPoint :: [Block] -> Gen (Word, Point Block)
genRollbackPoint blks = do
  let maxSlotNo = blockSlotNo (Prelude.last blks)
  ix <- SlotNo <$> choose (1, unSlotNo maxSlotNo)
  let rollbackDepth = length blks - length [blk | blk <- blks, blockSlotNo blk <= ix]
  rollbackPoint <- blockPoint <$> genBlockAt ix []
  pure (fromIntegral rollbackDepth, rollbackPoint)

-- | Generate a non-sparse sequence of blocks each containing an observable
-- transaction, starting from the returned on-chain head state.
--
-- Note that this does not generate the entire spectrum of observable
-- transactions in Hydra, but only init and commits, which is already sufficient
-- to observe at least one state transition and different levels of rollback.
genSequenceOfObservableBlocks :: Gen (SomeOnChainHeadStateAt, [Block])
genSequenceOfObservableBlocks = do
  ctx <- genHydraContext 3

  -- NOTE: commits must be generated from each participant POV, and thus, we
  -- need all their respective StIdle to move on.
  let stIdles = flip map (zip (ctxVerificationKeys ctx) (ctxParties ctx)) $ \(vk, p) ->
        let peerVerificationKeys = ctxVerificationKeys ctx \\ [vk]
         in idleOnChainHeadState (ctxNetworkId ctx) peerVerificationKeys vk p

  stIdle <- elements stIdles
  blks <- flip execStateT [] $ do
    initTx <- stepInit ctx stIdle
    void $ stepCommits ctx initTx stIdles

  pure (stAtGenesis (SomeOnChainHeadState stIdle), reverse blks)
 where
  nextSlot :: Monad m => StateT [Block] m SlotNo
  nextSlot = do
    get <&> \case
      [] -> 1
      x : _ -> SlotNo . succ . unSlotNo . blockSlotNo $ x

  putNextBlock :: Tx -> StateT [Block] Gen ()
  putNextBlock tx = do
    sl <- nextSlot
    blk <- lift $ genBlockAt sl [tx]
    modify' (blk :)

  stepInit ::
    HydraContext ->
    OnChainHeadState StIdle ->
    StateT [Block] Gen Tx
  stepInit ctx stIdle = do
    txIn <- lift genTxIn
    let initTx = initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) txIn stIdle
    initTx <$ putNextBlock initTx

  stepCommits ::
    HydraContext ->
    Tx ->
    [OnChainHeadState 'StIdle] ->
    StateT [Block] Gen [OnChainHeadState 'StInitialized]
  stepCommits ctx initTx = \case
    [] ->
      pure []
    stIdle : rest -> do
      stInitialized <- stepCommit initTx stIdle
      (stInitialized :) <$> stepCommits ctx initTx rest

  stepCommit ::
    Tx ->
    OnChainHeadState 'StIdle ->
    StateT [Block] Gen (OnChainHeadState 'StInitialized)
  stepCommit initTx stIdle = do
    let (_, stInitialized) = unsafeObserveTx @_ @StInitialized initTx stIdle
    utxo <- lift genCommit
    let commitTx = unsafeCommit utxo stInitialized
    putNextBlock commitTx
    pure $ snd $ unsafeObserveTx @_ @StInitialized commitTx stInitialized

stAtGenesis :: SomeOnChainHeadState -> SomeOnChainHeadStateAt
stAtGenesis currentOnChainHeadState =
  SomeOnChainHeadStateAt
    { currentOnChainHeadState
    , recordedAt = AtStart
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
    forAllTx $ \_ tx ->
      let cbor = serialize tx
          len = LBS.length cbor
       in len < txSizeLimit
            & label (showKB len)
            & counterexample (renderTx tx)
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
    forAllTx $ \st tx -> do
      let lookupUTxO = getKnownUTxO st
      case evaluateTx' exUnits tx lookupUTxO of
        Left basicFailure ->
          property False
            & counterexample ("Tx: " <> renderTx tx)
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
            & counterexample ("Phase-1 validation failed: " <> show basicFailure)
        Right redeemerReport ->
          all isRight (Map.elems redeemerReport)
            & counterexample ("Tx: " <> renderTx tx)
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
          ( forAllClose action
          , Transition @ 'StClosed (TransitionTo (Proxy @ 'StClosed))
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
      forAllShow genCommit renderUTxO $ \utxo ->
        let tx = unsafeCommit utxo stInitialized
         in action stInitialized tx
              & classify
                (null utxo)
                "Empty commit"
              & classify
                (not (null utxo))
                "Non-empty commit"

forAllNonEmptyByronCommit ::
  (PostTxError Tx -> Property) ->
  Property
forAllNonEmptyByronCommit action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAll (genStInitialized ctx) $ \stInitialized ->
      forAllShow genByronCommit renderUTxO $ \utxo ->
        case commit utxo stInitialized of
          Right{} -> property False
          Left e -> action e

forAllAbort ::
  (Testable property) =>
  (OnChainHeadState 'StInitialized -> Tx -> property) ->
  Property
forAllAbort action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAllShow (genInitTx ctx) renderTx $ \initTx -> do
      forAllShow (sublistOf . snd =<< genCommits ctx initTx) renderTxs $ \commits ->
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
forAllCollectCom action =
  forAll (genCollectComTx 3) $ uncurry action

forAllClose ::
  (Testable property) =>
  (OnChainHeadState 'StOpen -> Tx -> property) ->
  Property
forAllClose action = do
  -- TODO: label / classify tx and snapshots to understand test failures
  forAll (genCloseTx 3) $ uncurry action

forAllFanout ::
  (Testable property) =>
  (OnChainHeadState 'StClosed -> Tx -> property) ->
  Property
forAllFanout action = do
  forAll (genHydraContext 3) $ \ctx ->
    forAllShow (resize maxAssetsSupported $ simplifyUTxO <$> genUTxO) renderUTxO $ \utxo ->
      forAll (genStClosed ctx utxo) $ \stClosed ->
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

genByronCommit :: Gen UTxO
genByronCommit = do
  input <- arbitrary
  addr <- ByronAddressInEra <$> arbitrary
  value <- genValue
  pure $ UTxO.singleton (input, TxOut addr value TxOutDatumNone)

genBlockAt :: SlotNo -> [Tx] -> Gen Block
genBlockAt sl txs = do
  header <- adjustSlot <$> arbitrary
  let body = toTxSeq $ StrictSeq.fromList (toLedgerTx <$> txs)
  pure $ BlockAlonzo $ mkShelleyBlock $ Ledger.Block header body
 where
  adjustSlot (Ledger.BHeader body sig) =
    let body' = body{Ledger.bheaderSlotNo = sl}
     in Ledger.BHeader body' sig

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

--
-- Prettifier
--

showRollbackInfo :: (Word, Point Block) -> String
showRollbackInfo (rollbackDepth, rollbackPoint) =
  toString $
    unlines
      [ "Rollback depth: " <> show rollbackDepth
      , "Rollback point: " <> show rollbackPoint
      ]

showStateRecordedAt :: SomeOnChainHeadStateAt -> Text
showStateRecordedAt SomeOnChainHeadStateAt{recordedAt} =
  case recordedAt of
    AtStart -> "Start"
    AtPoint pt _ -> show pt
