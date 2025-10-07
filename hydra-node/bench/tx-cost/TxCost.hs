{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TxCost where

import Hydra.Prelude hiding (catch)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  Coin (..),
  ExecutionUnits (..),
  Tx,
  UTxO,
  genTxIn,
 )
import Hydra.Cardano.Api.TxOut (toPlutusTxOut)
import Hydra.Chain.Direct.State (
  ClosedState (..),
  InitialState (..),
  OpenState (..),
  commit,
  ctxContestationPeriod,
  ctxHeadParameters,
  ctxHydraSigningKeys,
  ctxParticipants,
  ctxVerificationKeys,
  genCloseTx,
  genCommits,
  genCommits',
  genDecrementTx,
  genHydraContextFor,
  genIncrementTx,
  genInitTx,
  genStClosed,
  genStInitial,
  genStOpen,
  getKnownUTxO,
  initialize,
  observeClose,
  pickChainContext,
  unsafeAbort,
  unsafeClose,
  unsafeCollect,
  unsafeContest,
  unsafeFanout,
  unsafeObserveInitAndCommits,
 )
import Hydra.Ledger.Cardano.Evaluate (
  estimateMinFee,
  evaluateTx,
  genPointInTimeBefore,
  genValidityBoundsFromContestationPeriod,
  maxTxSize,
  slotLength,
  systemStart,
  usedExecutionUnits,
 )
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx.Snapshot (genConfirmedSnapshot)
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusTx.Builtins (lengthOfByteString, serialiseData)
import Test.Hydra.Tx.Gen (genOutputFor, genUTxOAdaOnlyOfSize)
import Test.QuickCheck (oneof)

computeInitCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeInitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (tx, knownUtxo) <- genInitTx' numParties
    case checkSizeAndEvaluate tx knownUtxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genInitTx' :: Int -> Gen (Tx, UTxO)
  genInitTx' numParties = do
    ctx <- genHydraContextFor numParties
    cctx <- pickChainContext ctx
    seedInput <- genTxIn
    seedOutput <- genOutputFor =<< arbitrary
    let utxo = UTxO.singleton seedInput seedOutput
    pure (initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx), utxo)

computeCommitCost :: Gen [(NumUTxO, TxSize, MemUnit, CpuUnit, Coin)]
computeCommitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 11]
  pure $ interesting <> limit
 where
  compute numUTxO = do
    utxo <- genUTxOAdaOnlyOfSize numUTxO
    (commitTx, knownUtxo) <- genCommitTx utxo
    case commitTx of
      Left _ -> pure Nothing
      Right tx ->
        case checkSizeAndEvaluate tx (utxo <> knownUtxo) of
          Just (txSize, memUnit, cpuUnit, minFee) ->
            pure $ Just (NumUTxO $ UTxO.size utxo, txSize, memUnit, cpuUnit, minFee)
          Nothing ->
            pure Nothing

  genCommitTx utxo = do
    -- NOTE: number of parties is irrelevant for commit tx
    ctx <- genHydraContextFor 1
    (cctx, stInitial) <- genStInitial ctx
    let InitialState{headId} = stInitial
        knownUTxO = getKnownUTxO stInitial <> getKnownUTxO cctx
    pure (commit cctx headId knownUTxO utxo, knownUTxO)

computeCollectComCost :: Gen [(NumParties, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computeCollectComCost =
  catMaybes <$> mapM compute [1 .. 10]
 where
  compute numParties = do
    (utxo, tx, knownUtxo) <- genCollectComTx numParties
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
    let (committedUTxOs, stInitialized) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) initTx commits
    let InitialState{headId} = stInitialized
    let utxoToCollect = fold committedUTxOs
    let spendableUTxO = getKnownUTxO stInitialized
    pure (fold committedUTxOs, unsafeCollect cctx headId (ctxHeadParameters ctx) utxoToCollect spendableUTxO, getKnownUTxO stInitialized <> getKnownUTxO cctx)

computeIncrementCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeIncrementCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [50, 49 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (ctx, st, utxo', tx) <- genIncrementTx numParties
    let utxo = getKnownUTxO st <> getKnownUTxO ctx <> utxo'
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

computeDecrementCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeDecrementCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [50, 49 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    -- TODO: add decrementedOutputs to the result
    (ctx, _decrementedOutputs, st, _, tx) <- genDecrementTx numParties
    let utxo = getKnownUTxO st <> getKnownUTxO ctx
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

computeCloseCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeCloseCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [50, 49 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (ctx, st, _, tx, _sn) <- genCloseTx numParties
    let utxo = getKnownUTxO st <> getKnownUTxO ctx
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

computeContestCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeContestCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [50, 49 .. 11]
  pure $ interesting <> limit
 where
  compute numParties = do
    (tx, utxo) <- genContestTx numParties
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  genContestTx numParties = do
    ctx <- genHydraContextFor numParties
    utxo <- arbitrary
    (closedSnapshotNumber, _, _, _, stClosed@ClosedState{headId}) <- genStClosed ctx utxo mempty mempty
    cctx <- pickChainContext ctx
    snapshot <- genConfirmedSnapshot headId 0 (succ closedSnapshotNumber) utxo Nothing mempty (ctxHydraSigningKeys ctx)
    pointInTime <- genPointInTimeBefore stClosed.contestationDeadline
    let cp = ctxContestationPeriod ctx
    let contestUtxo = getKnownUTxO stClosed <> getKnownUTxO cctx
    pure (unsafeContest cctx contestUtxo headId cp 0 snapshot pointInTime, contestUtxo)

computeAbortCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeAbortCost =
  -- NOTE: We can't even close with one party right now, so no point in
  -- determining interesting values
  catMaybes <$> forM [1 .. 100] compute
 where
  compute numParties = do
    (tx, utxo) <- genAbortTx numParties
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
    let (committed, stInitialized) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) initTx commits
    let InitialState{seedTxIn} = stInitialized
    let spendableUTxO = getKnownUTxO stInitialized <> getKnownUTxO cctx
    pure (unsafeAbort cctx seedTxIn spendableUTxO (fold committed), spendableUTxO)

computeFanOutCost :: Gen [(NumParties, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM (uncurry compute) [(p, u) | p <- [numberOfParties], u <- [0, 1, 5, 10, 20, 30, 40, 50]]
  limit <-
    maybeToList
      . getFirst
      <$> foldMapM
        (\(p, u) -> First <$> compute p u)
        [(p, u) | p <- [numberOfParties], u <- [100, 99 .. 0]]
  pure $ interesting <> limit
 where
  numberOfParties = 10

  compute parties numElems = do
    (utxo, tx, knownUTxO) <- genFanoutTx parties numElems
    let utxoSerializedSize = serializedSize utxo
    case checkSizeAndEvaluate tx knownUTxO of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties parties, NumUTxO numElems, utxoSerializedSize, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

  -- Generate a fanout with a defined number of outputs.
  -- TODO: why are we not re-using the same functions from the Direct.State module?
  genFanoutTx numParties numOutputs = do
    utxo <- genUTxOAdaOnlyOfSize numOutputs
    ctx <- genHydraContextFor numParties
    (_committed, stOpen@OpenState{headId, seedTxIn}) <- genStOpen ctx
    utxoToCommit' <- oneof [arbitrary, pure Nothing]
    utxoToDecommit' <- oneof [arbitrary, pure Nothing]
    let (utxoToCommit, utxoToDecommit) = if isNothing utxoToCommit' then (mempty, utxoToDecommit') else (utxoToCommit', mempty)
    snapshot <- genConfirmedSnapshot headId 0 1 utxo utxoToCommit utxoToDecommit [] -- We do not validate the signatures
    cctx <- pickChainContext ctx
    let cp = ctxContestationPeriod ctx
    (startSlot, closePoint) <- genValidityBoundsFromContestationPeriod cp
    let closeTx = unsafeClose cctx (getKnownUTxO stOpen) headId (ctxHeadParameters ctx) 0 snapshot startSlot closePoint
        stClosed = snd . fromJust $ observeClose stOpen closeTx
        deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
        utxoToFanout = getKnownUTxO stClosed <> getKnownUTxO cctx
    pure (utxo, unsafeFanout cctx utxoToFanout seedTxIn utxo mempty mempty deadlineSlotNo, getKnownUTxO stClosed <> getKnownUTxO cctx)

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

checkSizeAndEvaluate :: Tx -> UTxO -> Maybe (TxSize, MemUnit, CpuUnit, Coin)
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
    . UTxO.foldMap (serialiseData . toBuiltinData . fromJust . toPlutusTxOut)
