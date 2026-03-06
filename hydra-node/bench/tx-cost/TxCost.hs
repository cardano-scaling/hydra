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
 )
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Cardano.Api.TxOut (toPlutusTxOut)
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
 )
import Hydra.Ledger.Cardano.Evaluate (
  usedExecutionUnits,
 )
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Plutus.Orphans ()
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
import Test.Hydra.Tx.Gen (genConfirmedSnapshot, genOutputFor, genPointInTimeBefore, genUTxOAdaOnlyOfSize, genValidityBoundsFromContestationPeriod)
import Test.QuickCheck (oneof)

computeInitCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeInitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50, 100]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [200, 199 .. 101]
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
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50, 100]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [200, 199 .. 101]
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
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50, 100]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [200, 199 .. 101]
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
