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
  genStClosed,
  genStIdle,
  genStInitialized,
 )
import Hydra.Chain.Direct.State (
  abort,
  commit,
  fanout,
  getKnownUTxO,
  initialize,
 )
import Hydra.Ledger.Cardano (
  adaOnly,
  genKeyPair,
  genOneUTxOFor,
  genOutput,
  genTxIn,
  simplifyUTxO,
 )
import Hydra.Ledger.Cardano.Evaluate (evaluateTx, genPointInTime, maxCpu, maxMem, maxTxSize, pparams)
import Plutus.Orphans ()
import Test.QuickCheck (generate, sublistOf, vectorOf)

computeInitCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeInitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 30]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 30]
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
    utxo <- generate $ genSimpleUTxOOfSize numUTxO
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
    (st, tx) <- generate $ genCloseTx numParties
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

  genAbortTx numParties =
    genHydraContextFor numParties >>= \ctx ->
      genInitTx ctx >>= \initTx ->
        (sublistOf . snd =<< genCommits ctx initTx) >>= \commits ->
          genStIdle ctx >>= \stIdle ->
            let stInitialized = executeCommits initTx commits stIdle
             in pure (abort stInitialized, getKnownUTxO stInitialized)

computeFanOutCost :: IO [(NumUTxO, TxSize, MemUnit, CpuUnit)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [500, 499 .. 51]
  pure $ interesting <> limit
 where
  compute numElems = do
    (tx, knownUtxo) <- generate $ genFanoutTx numElems
    case checkSizeAndEvaluate tx knownUtxo of
      Just (txSize, memUnit, cpuUnit) ->
        pure $ Just (NumUTxO numElems, txSize, memUnit, cpuUnit)
      Nothing ->
        pure Nothing

  genFanoutTx numOutputs = do
    ctx <- genHydraContext 3
    let utxo = genSimpleUTxOOfSize numOutputs `generateWith` 42
    (_, stClosed) <- genStClosed ctx utxo
    pointInTime <- genPointInTime
    pure (fanout utxo pointInTime stClosed, getKnownUTxO stClosed)

genSimpleUTxOOfSize :: Int -> Gen UTxO
genSimpleUTxOOfSize numUTxO =
  foldMap simplifyUTxO
    <$> vectorOf
      numUTxO
      ( genKeyPair >>= fmap (fmap adaOnly) . genOneUTxOFor . fst
      )

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
