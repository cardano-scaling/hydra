{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TxCost where

import Hydra.Prelude hiding (catch)

import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed (E2, Fixed)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  UTxO,
 )
import Hydra.Chain.Direct.Context (
  HydraContext (ctxVerificationKeys),
  ctxHeadParameters,
  executeCommits,
  genCloseTx,
  genCollectComTx,
  genCommits,
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
  genTxIn,
  simplifyUTxO,
 )
import Hydra.Ledger.Cardano.Evaluate (evaluateTx, pparams)
import Plutus.MerkleTree (rootHash)
import qualified Plutus.MerkleTree as MT
import Plutus.Orphans ()
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Plutus.Validator (
  ExUnits (ExUnits),
  evaluateScriptExecutionUnits,
 )
import Test.QuickCheck (generate, sublistOf, vectorOf)
import Validators (merkleTreeBuilderValidator, merkleTreeMemberValidator)

newtype NumParties = NumParties Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype NumUTxO = NumUTxO Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype TxSize = TxSize Int64
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype MemUnit = MemUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype CpuUnit = CpuUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

computeInitCost :: IO [(NumParties, TxSize)]
computeInitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 30]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 30]
  pure $ interesting <> limit
 where
  compute numParties = do
    tx <- generate $ genInitTx' numParties
    let txSize = LBS.length $ serialize tx
    if txSize < fromIntegral (Ledger._maxTxSize pparams)
      then pure (Just (NumParties numParties, TxSize txSize))
      else pure Nothing

  genInitTx' numParties = do
    genHydraContextFor numParties >>= \ctx ->
      genStIdle ctx >>= \stIdle ->
        genTxIn >>= \seedInput ->
          pure $ initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle

computeCommitCost :: IO [(NumUTxO, TxSize, MemUnit, CpuUnit)]
computeCommitCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50, 100]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100 .. 500]
  pure $ interesting <> limit
 where
  compute numUTxO = do
    utxo <- generate $ genSimpleUTxOOfSize numUTxO
    (commitTx, knownUtxo) <- generate $ genCommitTx utxo
    case commitTx of
      Left _ -> pure Nothing
      Right tx -> do
        let txSize = LBS.length $ serialize tx
        if txSize < fromIntegral (Ledger._maxTxSize pparams)
          then case evaluateTx tx (utxo <> knownUtxo) of
            (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) ->
              pure $ Just (NumUTxO $ length utxo, TxSize txSize, MemUnit mem, CpuUnit cpu)
            _ -> pure Nothing
          else pure Nothing

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
    let txSize = LBS.length $ serialize tx
    if txSize < fromIntegral (Ledger._maxTxSize pparams)
      then case evaluateTx tx utxo of
        (Right (mconcat . rights . Map.elems -> (Ledger.ExUnits mem cpu)))
          | fromIntegral mem <= maxMem && fromIntegral cpu <= maxCpu ->
            pure $ Just (NumParties numParties, TxSize txSize, MemUnit mem, CpuUnit cpu)
        _ -> pure Nothing
      else pure Nothing

computeCloseCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeCloseCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 30]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100, 99 .. 30]
  pure $ interesting <> limit
 where
  compute numParties = do
    (st, tx) <- generate $ genCloseTx numParties
    let utxo = getKnownUTxO st
    let txSize = LBS.length $ serialize tx
    if txSize < fromIntegral (Ledger._maxTxSize pparams)
      then case evaluateTx tx utxo of
        (Right (mconcat . rights . Map.elems -> (Ledger.ExUnits mem cpu)))
          | fromIntegral mem <= maxMem && fromIntegral cpu <= maxCpu ->
            pure $ Just (NumParties numParties, TxSize txSize, MemUnit mem, CpuUnit cpu)
        _ -> pure Nothing
      else pure Nothing

computeAbortCost :: IO [(NumParties, TxSize, MemUnit, CpuUnit)]
computeAbortCost =
  -- NOTE: We can't even close with one party right now, so no point in
  -- determining interesting values
  catMaybes <$> forM [1 .. 100] compute
 where
  compute numParties = do
    (tx, knownUtxo) <- generate $ genAbortTx numParties
    let txSize = LBS.length $ serialize tx
    if txSize < fromIntegral (Ledger._maxTxSize pparams)
      then case evaluateTx tx knownUtxo of
        (Right (mconcat . rights . Map.elems -> (Ledger.ExUnits mem cpu)))
          | fromIntegral mem <= maxMem && fromIntegral cpu <= maxCpu ->
            pure $ Just (NumParties numParties, TxSize txSize, MemUnit mem, CpuUnit cpu)
        _ -> pure Nothing
      else pure Nothing

  genAbortTx numParties =
    genHydraContextFor numParties >>= \ctx ->
      genInitTx ctx >>= \initTx ->
        (sublistOf =<< genCommits ctx initTx) >>= \commits ->
          genStIdle ctx >>= \stIdle ->
            let stInitialized = executeCommits initTx commits stIdle
             in pure (abort stInitialized, getKnownUTxO stInitialized)

computeFanOutCost :: IO [(NumUTxO, TxSize, MemUnit, CpuUnit)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10, 50, 100]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [100 .. 500]
  pure $ interesting <> limit
 where
  compute numElems = do
    (tx, knownUtxo) <- generate $ genFanoutTx numElems
    let txSize = LBS.length $ serialize tx
    case evaluateTx tx knownUtxo of
      (Right (mconcat . rights . Map.elems -> (Ledger.ExUnits mem cpu)))
        | fromIntegral mem <= maxMem && fromIntegral cpu <= maxCpu ->
          pure (Just (NumUTxO numElems, TxSize txSize, MemUnit mem, CpuUnit cpu))
      _ -> pure Nothing

  genFanoutTx numOutputs = do
    ctx <- genHydraContext 3
    utxo <- genSimpleUTxOOfSize numOutputs
    stClosed <- genStClosed ctx utxo
    pure (fanout utxo stClosed, getKnownUTxO stClosed)

genSimpleUTxOOfSize :: Int -> Gen UTxO
genSimpleUTxOOfSize numUTxO =
  foldMap simplifyUTxO
    <$> vectorOf
      numUTxO
      ( genKeyPair >>= fmap (fmap adaOnly) . genOneUTxOFor . fst
      )

computeMerkleTreeCost :: IO [(Int, MemUnit, CpuUnit, MemUnit, CpuUnit)]
computeMerkleTreeCost =
  mapM compute ([1 .. 10] <> [20, 30 .. 100] <> [120, 140 .. 500])
 where
  compute numElems = do
    utxo <- fmap Plutus.toBuiltin <$> genFakeUTxOs numElems

    let (memberMem, memberCpu) = fromRight (0, 0) $ executionCostForMember utxo
        (builderMem, builderCpu) = fromRight (0, 0) $ executionCostForBuilder utxo
    pure (numElems, MemUnit memberMem, CpuUnit memberCpu, MemUnit builderMem, CpuUnit builderCpu)

  -- NOTE: assume size of a UTXO is around  60 bytes
  genFakeUTxOs numElems = generate (vectorOf numElems $ BS.pack <$> vectorOf 60 arbitrary)

executionCostForMember :: [Plutus.BuiltinByteString] -> Either Text (Natural, Natural)
executionCostForMember utxo =
  let tree = MT.fromList utxo
      accumulateCost e acc =
        acc >>= \(curMem, curCpu) ->
          let proof = fromJust $ MT.mkProof e tree
           in case evaluateScriptExecutionUnits merkleTreeMemberValidator (e, MT.rootHash tree, proof) of
                Right (ExUnits mem cpu) ->
                  Right (mem + curMem, cpu + curCpu)
                Left err -> Left err
   in foldr accumulateCost (Right (0, 0)) utxo

executionCostForBuilder :: [Plutus.BuiltinByteString] -> Either Text (Natural, Natural)
executionCostForBuilder utxo =
  let tree = MT.fromList utxo
      root = rootHash tree
   in evaluateScriptExecutionUnits merkleTreeBuilderValidator (utxo, root) <&> \case
        ExUnits mem cpu -> (mem, cpu)

maxMem, maxCpu :: Fixed E2
Ledger.ExUnits (fromIntegral @_ @(Fixed E2) -> maxMem) (fromIntegral @_ @(Fixed E2) -> maxCpu) =
  Ledger._maxTxExUnits pparams

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42
