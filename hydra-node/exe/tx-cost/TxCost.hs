{-# LANGUAGE TypeApplications #-}

module TxCost where

import Hydra.Prelude hiding (catch)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Hydra.Cardano.Api (
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  Tx,
  UTxO,
 )
import Hydra.Chain.Direct.Context (
  HydraContext (ctxVerificationKeys),
  ctxHeadParameters,
  executeCommits,
  genCloseTx,
  genCollectComTx,
  genCommits,
  genContestTx,
  genHydraContext,
  genHydraContextFor,
  genInitTx,
  genStIdle,
  genStInitialized,
  genStOpen,
  unsafeObserveTx,
 )
import Hydra.Chain.Direct.State (
  HeadStateKind (StClosed),
  abort,
  close,
  commit,
  fanout,
  getContestationDeadline,
  getKnownUTxO,
  initialize,
 )
import Hydra.Ledger.Cardano (
  genOutput,
  genTxIn,
  genUTxOAdaOnlyOfSize,
 )
import Hydra.Ledger.Cardano.Evaluate (
  evaluateTx,
  genPointInTime,
  genPointInTimeAfter,
  maxCpu,
  maxMem,
  maxTxSize,
 )
import Hydra.Snapshot (genConfirmedSnapshot)
import Plutus.Orphans ()
import Test.QuickCheck (generate, sublistOf)

computeInitCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeInitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (tx, knownUtxo) <- generate $ genInitTx' numParties
    case checkSizeAndEvaluate tx knownUtxo of
      Just (txSize, memUnit, cpuUnit) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit)
      Nothing ->
        pure Nothing

  genInitTx' numParties = do
    ctx <- genHydraContextFor numParties
    stIdle <- genStIdle ctx
    seedInput <- genTxIn
    seedOutput <- genOutput =<< arbitrary
    let utxo = UTxO.singleton (seedInput, seedOutput)
    pure (initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle, utxo)

computeCommitCost :: IO [(NumUTxO, TxSize, MemUnit, CpuUnit)]
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
          Just (txSize, memUnit, cpuUnit) ->
            pure $ Just (NumUTxO $ length utxo, txSize, memUnit, cpuUnit)
          Nothing ->
            pure Nothing

  genCommitTx utxo = do
    -- NOTE: number of parties is irrelevant for commit tx
    ctx <- genHydraContextFor 1
    stInitialized <- genStInitialized ctx
    pure (commit utxo stInitialized, getKnownUTxO stInitialized)

computeCollectComCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeCollectComCost =
  catMaybes <$> mapM compute [1 .. 100]
 where
  compute numParties = do
    (st, tx) <- generate $ genCollectComTx numParties
    let utxo = getKnownUTxO st
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit)
      Nothing ->
        pure Nothing

computeCloseCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeCloseCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 30]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 31]
  pure $ interesting <> limit
 where
  compute numParties = do
    (st, tx, _sn) <- generate $ genCloseTx numParties
    let utxo = getKnownUTxO st
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit)
      Nothing ->
        pure Nothing

computeContestCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeContestCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 30]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 31]
  pure $ interesting <> limit
 where
  compute numParties = do
    (st, tx) <- generate $ genContestTx numParties
    let utxo = getKnownUTxO st
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit)
      Nothing ->
        pure Nothing

computeAbortCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeAbortCost =
  -- NOTE: We can't even close with one party right now, so no point in
  -- determining interesting values
  catMaybes <$> forM [1 .. 100] compute
 where
  compute numParties = do
    (tx, knownUtxo) <- generate $ genAbortTx numParties
    case checkSizeAndEvaluate tx knownUtxo of
      Just (txSize, memUnit, cpuUnit) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit)
      Nothing ->
        pure Nothing

  genAbortTx numParties = do
    ctx <- genHydraContextFor numParties
    initTx <- genInitTx ctx
    commits <- sublistOf . snd =<< genCommits ctx initTx
    stIdle <- genStIdle ctx
    let (_, stInitialized) = executeCommits initTx commits stIdle
    pure (abort stInitialized, getKnownUTxO stInitialized)

computeFanOutCost :: IO [(NumUTxO, TxSize, MemUnit, CpuUnit)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 51]
  pure $ interesting <> limit
 where
  compute numElems = do
    (utxo, tx) <- generate $ genFanoutTx 3 numElems
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit) ->
        pure $ Just (NumUTxO numElems, txSize, memUnit, cpuUnit)
      Nothing ->
        pure Nothing

  -- Generate a fanout with a defined number of outputs.
  genFanoutTx numParties numOutputs = do
    utxo <- genUTxOAdaOnlyOfSize numOutputs
    ctx <- genHydraContext numParties
    (_committed, stOpen) <- genStOpen ctx
    snapshot <- genConfirmedSnapshot 1 utxo [] -- We do not validate the signatures
    closePoint <- genPointInTime
    let closeTx = close snapshot closePoint stOpen
    let stClosed = snd $ unsafeObserveTx @_ @ 'StClosed closeTx stOpen
    fanoutPoint <- genPointInTimeAfter (getContestationDeadline stClosed)
    pure (getKnownUTxO stClosed, fanout utxo fanoutPoint stClosed)

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

checkSizeAndEvaluate :: Tx -> UTxO -> Maybe (TxSize, MemUnit, CpuUnit)
checkSizeAndEvaluate tx knownUTxO = do
  guard $ txSize < maxTxSize
  case evaluateTx tx knownUTxO of
    (Right report) -> do
      let results = Map.elems report
      guard $ all isRight results
      let (Ledger.ExUnits mem cpu) = mconcat $ rights results
      guard $ fromIntegral mem <= maxMem
      guard $ fromIntegral cpu <= maxCpu
      Just (TxSize txSize, MemUnit mem, CpuUnit cpu)
    _ -> Nothing
 where
  txSize = fromIntegral $ LBS.length $ serialize tx

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42
