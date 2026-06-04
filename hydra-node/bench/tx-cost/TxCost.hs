{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TxCost where

import Hydra.Prelude hiding (catch)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  Coin (..),
  ExecutionUnits (..),
  Tx,
  UTxO,
  modifyTxOutValue,
 )
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Cardano.Api.TxOut (toPlutusTxOut)
import Hydra.Chain.Direct.Handlers (findLargestFitting)
import Hydra.Chain.Direct.State (
  ClosedState (..),
  OpenState (..),
  ctxContestationPeriod,
  ctxHeadParameters,
  ctxHydraSigningKeys,
  ctxParticipants,
  getKnownUTxO,
  initialize,
  observeClose,
  unsafeClose,
  unsafeContest,
  unsafeFanout,
  unsafeFinalPartialFanout,
  unsafePartialFanout,
 )
import Hydra.Ledger.Cardano.Evaluate (
  usedExecutionUnits,
 )
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (utxoFromTx)
import Hydra.Tx.KZGTrustedSetup (maxAccumulatorSize)
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusTx.Builtins (lengthOfByteString, serialiseData)
import Test.Hydra.Chain.Direct.State (
  genCloseTx,
  genDecrementTx,
  genHydraContextFor,
  genIncrementTx,
  genStClosed,
  genStOpen,
  pickChainContext,
 )
import Test.Hydra.Ledger.Cardano.Fixtures (
  estimateMinFee,
  evaluateTx,
  maxTxSize,
  slotLength,
  systemStart,
 )
import Test.Hydra.Tx.Fixture (fanoutOutputThreshold)
import Test.Hydra.Tx.Gen (genConfirmedSnapshot, genOutputFor, genPointInTimeBefore, genUTxOAdaOnlyOfSize, genUTxOWithTokensOfSize, genValidityBoundsFromContestationPeriod)
import Test.QuickCheck (oneof)

computeInitCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeInitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50, 100]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [125, 124 .. 101]
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

computeIncrementCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeIncrementCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [75, 74 .. 51]
  pure $ interesting <> limit
 where
  compute numParties = do
    (ctx, st, utxo', tx) <- genIncrementTx numParties
    cctx <- pickChainContext ctx
    let utxo = getKnownUTxO st <> getKnownUTxO cctx <> utxo'
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

computeDecrementCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeDecrementCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [75, 74 .. 51]
  pure $ interesting <> limit
 where
  compute numParties = do
    -- TODO: add decrementedOutputs to the result
    (ctx, _decrementedOutputs, st, utxo', tx) <- genDecrementTx numParties
    let utxo = getKnownUTxO st <> getKnownUTxO ctx <> utxo'
    case checkSizeAndEvaluate tx utxo of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumParties numParties, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

computeCloseCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeCloseCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [75, 74 .. 51]
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
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [75, 74 .. 51]
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

computeFanOutCost :: Gen [(NumParties, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM (uncurry compute) [(p, u) | p <- [numberOfParties], u <- [0, 1, 5, 10, 20, 30, 50, 100, 200, 500, 1000]]
  limit <-
    maybeToList
      . getFirst
      <$> foldMapM
        (\(p, u) -> First <$> compute p u)
        -- Sparse descending search: find the largest UTxO count that still fits in a tx.
        [(p, u) | p <- [numberOfParties], u <- [2000, 1500, 1000, 500, 200, 100, 60, 50, 40, 30, 20, 15, 10]]
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
    let maxExtra = min 50 (maxAccumulatorSize - numOutputs)
    utxoToCommit' <- oneof [Just <$> genUTxOAdaOnlyOfSize maxExtra, pure Nothing]
    utxoToDecommit' <- oneof [Just <$> genUTxOAdaOnlyOfSize maxExtra, pure Nothing]
    let (utxoToCommit, utxoToDecommit) = if isNothing utxoToCommit' then (mempty, utxoToDecommit') else (utxoToCommit', mempty)
    snapshot <- genConfirmedSnapshot headId 0 1 utxo utxoToCommit utxoToDecommit [] -- We do not validate the signatures
    cctx <- pickChainContext ctx
    let cp = ctxContestationPeriod ctx
    (startSlot, closePoint) <- genValidityBoundsFromContestationPeriod cp
    let closeTx = unsafeClose cctx (getKnownUTxO stOpen) headId (ctxHeadParameters ctx) 0 snapshot startSlot closePoint
        stClosed = snd . fromJust $ observeClose stOpen closeTx
        deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
        allFanoutValue =
          UTxO.totalValue utxo
            <> maybe mempty UTxO.totalValue utxoToCommit
            <> maybe mempty UTxO.totalValue utxoToDecommit
        utxoToFanout =
          UTxO.map (modifyTxOutValue (<> allFanoutValue)) (getKnownUTxO stClosed)
            <> getKnownUTxO cctx
    let utxoForProof = utxo <> fold utxoToCommit <> fold utxoToDecommit
    pure (utxo, unsafeFanout cctx utxoToFanout seedTxIn utxo utxoToCommit utxoToDecommit utxoForProof deadlineSlotNo, utxoToFanout)

-- | Compute costs of partial fanout transactions across a range of per-step
-- distribution sizes.
--
-- For each total UTxO count N, we build one partial fanout that distributes
-- all-but-one outputs (the benchmark shows max-chunk scaling). The tx size
-- grows with the total UTxO count because the accumulator serialisation stored
-- in the output datum grows linearly with remaining UTxO count.
computePartialFanOutNominalCost :: Gen [(NumUTxO, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computePartialFanOutNominalCost = do
  -- Show how partial fanout scales across the full new accumulator range (up to 4095).
  -- The tx size grows with remaining UTxO count (larger datum), so we probe widely.
  interesting <-
    catMaybes
      <$> mapM
        compute
        [fanoutOutputThreshold + 1, 25, 30, 40, 50, 100, 150, 200]
  limit <-
    maybeToList . getFirst
      <$> foldMapM
        (fmap First . compute)
        -- Sparse descending search for the maximum total UTxO count that still fits.
        [200, 100, 60, 50, 40, 30, 20]
  pure $ interesting <> limit
 where
  numberOfParties = 3

  compute totalUTxO = do
    utxo <- genUTxOAdaOnlyOfSize totalUTxO
    ctx <- genHydraContextFor numberOfParties
    (_committed, stOpen@OpenState{headId, seedTxIn}) <- genStOpen ctx
    snapshot <- genConfirmedSnapshot headId 0 1 utxo mempty mempty []
    cctx <- pickChainContext ctx
    let cp = ctxContestationPeriod ctx
    (startSlot, closePoint) <- genValidityBoundsFromContestationPeriod cp
    let closeTx = unsafeClose cctx (getKnownUTxO stOpen) headId (ctxHeadParameters ctx) 0 snapshot startSlot closePoint
        stClosed = snd . fromJust $ observeClose stOpen closeTx
        deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
        spendableUTxO =
          UTxO.map (modifyTxOutValue (<> UTxO.totalValue utxo)) (getKnownUTxO stClosed)
            <> getKnownUTxO cctx
        tryChunk n =
          let tx = unsafePartialFanout cctx spendableUTxO seedTxIn n utxo deadlineSlotNo
              utxoDistributed = UTxO.fromList . take n $ UTxO.toList utxo
           in fmap
                (\(txSize, memUnit, cpuUnit, minFee) -> (NumUTxO totalUTxO, NumUTxO (totalUTxO - n), serializedSize utxoDistributed, txSize, memUnit, cpuUnit, minFee))
                (checkSizeAndEvaluate tx spendableUTxO)
    either (const Nothing) Just <$> findLargestFitting (pure . tryChunk) (totalUTxO - 1)

-- | Like 'computePartialFanOutNominalCost' but uses outputs carrying native
-- tokens (all sharing one policy ID so the accumulated head value stays
-- bounded). Reveals how multi-asset outputs affect script execution costs.
computePartialFanOutMixedCost :: Gen [(NumUTxO, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computePartialFanOutMixedCost = do
  interesting <-
    catMaybes
      <$> mapM
        compute
        [fanoutOutputThreshold + 1, 25, 30, 40, 50, 100, 150, 200]
  limit <-
    maybeToList . getFirst
      <$> foldMapM
        (fmap First . compute)
        [200, 100, 60, 50, 40, 30, 20]
  pure $ interesting <> limit
 where
  numberOfParties = 3

  compute totalUTxO = do
    utxo <- genUTxOWithTokensOfSize totalUTxO
    ctx <- genHydraContextFor numberOfParties
    (_committed, stOpen@OpenState{headId, seedTxIn}) <- genStOpen ctx
    snapshot <- genConfirmedSnapshot headId 0 1 utxo mempty mempty []
    cctx <- pickChainContext ctx
    let cp = ctxContestationPeriod ctx
    (startSlot, closePoint) <- genValidityBoundsFromContestationPeriod cp
    let closeTx = unsafeClose cctx (getKnownUTxO stOpen) headId (ctxHeadParameters ctx) 0 snapshot startSlot closePoint
        stClosed = snd . fromJust $ observeClose stOpen closeTx
        deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
        spendableUTxO =
          UTxO.map (modifyTxOutValue (<> UTxO.totalValue utxo)) (getKnownUTxO stClosed)
            <> getKnownUTxO cctx
        tryChunk n =
          let tx = unsafePartialFanout cctx spendableUTxO seedTxIn n utxo deadlineSlotNo
              utxoDistributed = UTxO.fromList . take n $ UTxO.toList utxo
           in fmap
                (\(txSize, memUnit, cpuUnit, minFee) -> (NumUTxO totalUTxO, NumUTxO (totalUTxO - n), serializedSize utxoDistributed, txSize, memUnit, cpuUnit, minFee))
                (checkSizeAndEvaluate tx spendableUTxO)
    either (const Nothing) Just <$> findLargestFitting (pure . tryChunk) (totalUTxO - 1)

-- | Compute costs of the final partial fanout transaction (FanoutProgress → Final)
-- with mixed UTxOs. This is the terminal step that burns all head tokens and
-- proves the accumulator is fully exhausted via KZG proof.
--
-- Setup chains through a preceding PartialFanout to produce a FanoutProgress
-- head output, since FinalPartialFanout requires that datum as input.
computeFinalPartialFanOutCost :: Gen [(NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computeFinalPartialFanOutCost = do
  interesting <- catMaybes <$> mapM compute [1, 5, 10, 20, 30, 50, 100, 200]
  limit <-
    maybeToList . getFirst
      <$> foldMapM
        (fmap First . compute)
        [200, 100, 60, 50, 40, 30, 20, 10, 5, 1]
  pure $ interesting <> limit
 where
  numberOfParties = 3

  compute numFinal = do
    -- 1 UTxO for the preceding PartialFanout (minimal setup to reach FanoutProgress),
    -- numFinal UTxOs for the FinalPartialFanout being measured.
    let totalUTxO = 1 + numFinal
    utxo <- genUTxOWithTokensOfSize totalUTxO
    ctx <- genHydraContextFor numberOfParties
    (_committed, stOpen@OpenState{headId, seedTxIn}) <- genStOpen ctx
    snapshot <- genConfirmedSnapshot headId 0 1 utxo mempty mempty []
    cctx <- pickChainContext ctx
    let cp = ctxContestationPeriod ctx
    (startSlot, closePoint) <- genValidityBoundsFromContestationPeriod cp
    let closeTx = unsafeClose cctx (getKnownUTxO stOpen) headId (ctxHeadParameters ctx) 0 snapshot startSlot closePoint
        stClosed = snd . fromJust $ observeClose stOpen closeTx
        deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
        u0Value = UTxO.totalValue utxo
        spendableUTxO =
          UTxO.map (modifyTxOutValue (<> u0Value)) (getKnownUTxO stClosed)
            <> getKnownUTxO cctx
        utxoRemaining = UTxO.fromList . drop 1 $ UTxO.toList utxo
        -- Step 1: minimal PartialFanout (1 output) to produce a FanoutProgress head output
        partialTx = unsafePartialFanout cctx spendableUTxO seedTxIn 1 utxo deadlineSlotNo
        fanoutProgressUTxO = utxoFromTx partialTx <> getKnownUTxO cctx
        -- Step 2: FinalPartialFanout distributes all remaining outputs in one tx
        tx = unsafeFinalPartialFanout cctx fanoutProgressUTxO seedTxIn utxoRemaining deadlineSlotNo
    case checkSizeAndEvaluate tx fanoutProgressUTxO of
      Just (txSize, memUnit, cpuUnit, minFee) ->
        pure $ Just (NumUTxO numFinal, serializedSize utxoRemaining, txSize, memUnit, cpuUnit, minFee)
      Nothing ->
        pure Nothing

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
