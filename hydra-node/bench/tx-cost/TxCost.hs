module TxCost where

import Hydra.Prelude hiding (catch)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  ExecutionUnits (..),
  Lovelace,
  Tx,
  UTxO,
  genTxIn,
 )
import Hydra.Cardano.Api.TxOut (toPlutusTxOut)
import Hydra.Chain.Direct.State (
  ChainContext (contestationPeriod),
  abort,
  close,
  collect,
  commit,
  contest,
  ctxHeadParameters,
  ctxHydraSigningKeys,
  fanout,
  genCloseTx,
  genCommits,
  genCommits',
  genHydraContextFor,
  genInitTx,
  genStClosed,
  genStInitial,
  genStOpen,
  getContestationDeadline,
  getKnownUTxO,
  initialize,
  observeClose,
  pickChainContext,
  unsafeObserveInitAndCommits,
 )
import Hydra.Ledger.Cardano (
  genOutput,
  genUTxOAdaOnlyOfSize,
 )
import Hydra.Ledger.Cardano.Evaluate (
  estimateMinFee,
  evaluateTx,
  genPointInTimeBefore,
  genValidityBoundsFromContestationPeriod,
  maxTxSize,
  slotNoFromUTCTime,
  usedExecutionUnits,
 )
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (genConfirmedSnapshot)
import PlutusLedgerApi.V2 (toBuiltinData)
import PlutusTx.Builtins (lengthOfByteString, serialiseData)
import Test.QuickCheck (generate)

computeInitCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeInitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (tx, knownUtxo) <- generate $ genInitTx' numParties
    case checkSizeAndEvaluate tx knownUtxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genInitTx' numParties = do
    ctx <- genHydraContextFor numParties
    cctx <- pickChainContext ctx
    seedInput <- genTxIn
    seedOutput <- genOutput =<< arbitrary
    let utxo = UTxO.singleton (seedInput, seedOutput)
    pure (initialize cctx (ctxHeadParameters ctx) seedInput, utxo)

computeCommitCost :: IO [(NumUTxO, TxSize, MemUnit, CpuUnit, Lovelace)]
computeCommitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 11]
  pure $ interesting <> limit
 where
  compute numUTxO = do
    utxo <- generate $ genUTxOAdaOnlyOfSize numUTxO
    (commitTx, knownUtxo) <- generate $ genCommitTx utxo
    case commitTx of
      Left _ -> pure Nothing
      Right tx ->
        case checkSizeAndEvaluate tx (utxo <> knownUtxo) of
          Just (txSize, memUnit, cpuUnit, minFee) ->
            pure $ Just (NumUTxO $ length utxo, txSize, memUnit, cpuUnit, minFee)
          Nothing ->
            pure Nothing

  genCommitTx utxo = do
    -- NOTE: number of parties is irrelevant for commit tx
    ctx <- genHydraContextFor 1
    (cctx, stInitial) <- genStInitial ctx
    pure (commit cctx stInitial utxo, getKnownUTxO stInitial <> getKnownUTxO cctx)

computeCollectComCost :: IO [(NumParties, Natural, TxSize, MemUnit, CpuUnit, Lovelace)]
computeCollectComCost =
  catMaybes <$> mapM compute [1 .. 10]
 where
  compute numParties = do
    (utxo, tx, knownUtxo) <- generate $ genCollectComTx numParties
    case checkSizeAndEvaluate tx knownUtxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, serializedSize utxo, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genCollectComTx numParties = do
    ctx <- genHydraContextFor numParties
    cctx <- pickChainContext ctx
    initTx <- genInitTx ctx
    commits <- genCommits' (genUTxOAdaOnlyOfSize 1) ctx initTx
    let (committedUTxOs, stInitialized) = unsafeObserveInitAndCommits cctx initTx commits
    pure (fold committedUTxOs, collect cctx stInitialized, getKnownUTxO stInitialized <> getKnownUTxO cctx)

computeCloseCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeCloseCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [50, 49 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (ctx, st, tx, _sn) <- generate $ genCloseTx numParties
    let utxo = getKnownUTxO st <> getKnownUTxO ctx
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

computeContestCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeContestCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [50, 49 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (tx, utxo) <- generate $ genContestTx numParties
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genContestTx numParties = do
    ctx <- genHydraContextFor numParties
    utxo <- arbitrary
    (closedSnapshotNumber, _, stClosed) <- genStClosed ctx utxo
    cctx <- pickChainContext ctx
    snapshot <- genConfirmedSnapshot (succ closedSnapshotNumber) utxo (ctxHydraSigningKeys ctx)
    pointInTime <- genPointInTimeBefore (getContestationDeadline stClosed)
    pure (contest cctx stClosed snapshot pointInTime, getKnownUTxO stClosed <> getKnownUTxO cctx)

computeAbortCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeAbortCost =
  -- NOTE: We can't even close with one party right now, so no point in
  -- determining interesting values
  catMaybes <$> forM [1 .. 100] compute
 where
  compute numParties = do
    (tx, utxo) <- generate $ genAbortTx numParties
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) -> do
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genAbortTx numParties = do
    ctx <- genHydraContextFor numParties
    initTx <- genInitTx ctx
    -- NOTE: Commits are more expensive to abort, so let's use all commits
    commits <- genCommits ctx initTx
    cctx <- pickChainContext ctx
    let (committed, stInitialized) = unsafeObserveInitAndCommits cctx initTx commits
    pure (abort cctx stInitialized (fold committed), getKnownUTxO stInitialized <> getKnownUTxO cctx)

computeFanOutCost :: IO [(NumParties, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Lovelace)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM (uncurry compute) [(p, u) | p <- [5], u <- [0, 1, 5, 10, 20, 30, 40, 50]]
  limit <-
    maybeToList . getFirst
      <$> foldMapM
        (\(p, u) -> First <$> compute p u)
        [(p, u) | p <- [5], u <- [100, 99 .. 0]]
  pure $ interesting <> limit
 where
  compute parties numElems = do
    (utxo, tx, knownUTxO) <- generate $ genFanoutTx parties numElems
    let utxoSerializedSize = serializedSize utxo
    case checkSizeAndEvaluate tx knownUTxO of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties parties, NumUTxO numElems, utxoSerializedSize, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  -- Generate a fanout with a defined number of outputs.
  genFanoutTx numParties numOutputs = do
    utxo <- genUTxOAdaOnlyOfSize numOutputs
    ctx <- genHydraContextFor numParties
    (_committed, stOpen) <- genStOpen ctx
    snapshot <- genConfirmedSnapshot 1 utxo [] -- We do not validate the signatures
    cctx <- pickChainContext ctx
    let cp = contestationPeriod cctx
    (startSlot, closePoint) <- genValidityBoundsFromContestationPeriod cp
    let closeTx = close cctx stOpen snapshot startSlot closePoint
    let stClosed = snd . fromJust $ observeClose stOpen closeTx
    let deadlineSlotNo = slotNoFromUTCTime (getContestationDeadline stClosed)
    pure (utxo, fanout cctx stClosed utxo deadlineSlotNo, getKnownUTxO stClosed <> getKnownUTxO cctx)

newtype NumParties = NumParties Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype NumUTxO = NumUTxO Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype TxSize = TxSize Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype MemUnit = MemUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype CpuUnit = CpuUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

checkSizeAndEvaluate :: Tx -> UTxO -> Maybe (TxSize, MemUnit, CpuUnit, Lovelace)
checkSizeAndEvaluate tx knownUTxO = do
  guard $ txSize < maxTxSize
  case evaluateTx tx knownUTxO of
    (Right report) -> do
      guard $ all isRight report
      let ExecutionUnits
            { executionMemory = usedMemory
            , executionSteps = usedCpu
            } = usedExecutionUnits report
      let minFee = estimateMinFee tx report
      Just (TxSize txSize, MemUnit usedMemory, CpuUnit usedCpu, minFee)
    _ -> Nothing
 where
  txSize = fromIntegral $ LBS.length $ serialize tx

serializedSize :: UTxO -> Natural
serializedSize =
  fromIntegral
    . lengthOfByteString
    . foldMap (serialiseData . toBuiltinData . fromJust . toPlutusTxOut)
