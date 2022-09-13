module TxCost where

import Hydra.Prelude hiding (catch)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  ExecutionUnits (..),
  Lovelace,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  Tx,
  UTxO,
 )
import Hydra.Chain.Direct.Context (
  ctxHeadParameters,
  ctxHydraSigningKeys,
  genCloseTx,
  genCommits,
  genHydraContext,
  genHydraContextFor,
  genInitTx,
  genStClosed,
  genStInitial,
  genStOpen,
  getContestationDeadline,
  pickChainContext,
  unsafeObserveInitAndCommits,
 )
import Hydra.Chain.Direct.State (
  abort,
  close,
  collect,
  commit,
  contest,
  fanout,
  getKnownUTxO,
  initialize,
  observeClose,
 )
import Hydra.Ledger.Cardano (
  genOutput,
  genTxIn,
  genUTxOAdaOnlyOfSize,
 )
import Hydra.Ledger.Cardano.Evaluate (
  estimateMinFee,
  evaluateTx,
  genPointInTime,
  genPointInTimeBefore,
  maxCpu,
  maxMem,
  maxTxSize,
  slotNoFromUTCTime,
 )
import Hydra.Snapshot (genConfirmedSnapshot)
import Plutus.Orphans ()
import Test.QuickCheck (generate, sublistOf)

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
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50, 100]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [500, 499 .. 101]
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
    stInitial <- genStInitial ctx
    pure (commit stInitial utxo, getKnownUTxO stInitial)

computeCollectComCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeCollectComCost =
  catMaybes <$> mapM compute [1 .. 100]
 where
  compute numParties = do
    (st, tx) <- generate $ genCollectComTx numParties
    let utxo = getKnownUTxO st
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genCollectComTx numParties = do
    ctx <- genHydraContextFor numParties
    cctx <- pickChainContext ctx
    initTx <- genInitTx ctx
    commits <- genCommits ctx initTx
    let (_, stInitialized) = unsafeObserveInitAndCommits cctx initTx commits
    pure (stInitialized, collect stInitialized)

computeCloseCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeCloseCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 30]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 31]
  pure $ interesting <> limit
 where
  compute numParties = do
    (st, tx, _sn) <- generate $ genCloseTx numParties
    let utxo = getKnownUTxO st
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

computeContestCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeContestCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 30]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 31]
  pure $ interesting <> limit
 where
  compute numParties = do
    (st, tx) <- generate $ genContestTx numParties
    let utxo = getKnownUTxO st
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genContestTx numParties = do
    ctx <- genHydraContextFor numParties
    utxo <- arbitrary
    (closedSnapshotNumber, _, stClosed) <- genStClosed ctx utxo
    snapshot <- genConfirmedSnapshot (succ closedSnapshotNumber) utxo (ctxHydraSigningKeys ctx)
    pointInTime <- genPointInTimeBefore (getContestationDeadline stClosed)
    pure (stClosed, contest stClosed snapshot pointInTime)

computeAbortCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit, Lovelace)]
computeAbortCost =
  -- NOTE: We can't even close with one party right now, so no point in
  -- determining interesting values
  catMaybes <$> forM [1 .. 100] compute
 where
  compute numParties = do
    (tx, knownUtxo) <- generate $ genAbortTx numParties
    case checkSizeAndEvaluate tx knownUtxo of
      Just (txSize, memUnit, cpuUnit, minFee) -> do
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genAbortTx numParties = do
    ctx <- genHydraContextFor numParties
    initTx <- genInitTx ctx
    commits <- sublistOf =<< genCommits ctx initTx
    cctx <- pickChainContext ctx
    let (_, stInitialized) = unsafeObserveInitAndCommits cctx initTx commits
    pure (abort stInitialized, getKnownUTxO stInitialized)

computeFanOutCost :: IO [(NumUTxO, TxSize, MemUnit, CpuUnit, Lovelace)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 51]
  pure $ interesting <> limit
 where
  compute numElems = do
    (utxo, tx) <- generate $ genFanoutTx 3 numElems
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumUTxO numElems, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  -- Generate a fanout with a defined number of outputs.
  genFanoutTx numParties numOutputs = do
    utxo <- genUTxOAdaOnlyOfSize numOutputs
    ctx <- genHydraContext numParties
    (_committed, stOpen) <- genStOpen ctx
    snapshot <- genConfirmedSnapshot 1 utxo [] -- We do not validate the signatures
    closePoint <- genPointInTime
    let closeTx = close stOpen snapshot closePoint
    let stClosed = snd . fromJust $ observeClose stOpen closeTx
    let deadlineSlotNo = slotNoFromUTCTime (getContestationDeadline stClosed)
    pure (getKnownUTxO stClosed, fanout stClosed utxo deadlineSlotNo)

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
      let results = Map.elems report
      guard $ all isRight results
      let totalMemory = sum $ executionMemory <$> rights results
      let totalCpu = sum $ executionSteps <$> rights results
      guard $ totalMemory <= maxMem
      guard $ totalCpu <= maxCpu
      let minFee = estimateMinFee tx report
      Just (TxSize txSize, MemUnit totalMemory, CpuUnit totalCpu, minFee)
    _ -> Nothing
 where
  txSize = fromIntegral $ LBS.length $ serialize tx

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42
