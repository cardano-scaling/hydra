{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module TxCost where

import Hydra.Prelude hiding (catch)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed (E2, Fixed)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  BuildTxWith (BuildTxWith),
  ExecutionUnits (..),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PlutusScriptV1,
  UTxO,
  fromLedgerExUnits,
  fromLedgerValue,
  fromPlutusScript,
  lovelaceToValue,
  mkScriptAddress,
  mkScriptDatum,
  mkScriptWitness,
  mkTxOutDatum,
  modifyTxOutValue,
  scriptWitnessCtx,
  toCtxUTxOTxOut,
  toScriptData,
  pattern ScriptWitness,
  pattern TxOut,
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
import qualified Hydra.Contract.Hash as Hash
import Hydra.Ledger.Cardano (
  adaOnly,
  addInputs,
  emptyTxBody,
  genKeyPair,
  genOneUTxOFor,
  genTxIn,
  simplifyUTxO,
  unsafeBuildTransaction,
 )
import Hydra.Ledger.Cardano.Evaluate (evaluateTx, pparams)
import Plutus.MerkleTree (rootHash)
import qualified Plutus.MerkleTree as MT
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (toBuiltin)
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Plutus.Validator (
  ExUnits (ExUnits),
  evaluateScriptExecutionUnits,
 )
import Test.QuickCheck (generate, resize, sublistOf, vectorOf)
import Validators (merkleTreeBuilderValidator, merkleTreeMemberValidator)

newtype NumParties = NumParties Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype NumUTxO = NumUTxO Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype ValueSize = ValueSize Int
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

-- REVIEW: This is resulting in many "holes" -> some generate utxos/values very expensive?
computeCommitCost :: IO [(NumUTxO, ValueSize, TxSize, MemUnit, CpuUnit)]
computeCommitCost =
  concat
    <$> forM
      [0 .. 100]
      ( \numUTxO -> do
          let valueSizes =
                if numUTxO == 0
                  then [0]
                  else 1 : [5, 10 .. 100]
          catMaybes
            <$> forM
              valueSizes
              (compute numUTxO)
      )
 where
  compute numUTxO sz = do
    -- FIXME: genSimpleUTxOOfSize only produces ada-only, so the separation of
    -- generating and resizing is moot
    utxo <- generate $ genSimpleUTxOOfSize numUTxO >>= resizeValuesTo sz
    (commitTx, knownUtxo) <- generate $ genCommitTx utxo
    case commitTx of
      Left _ -> pure Nothing
      Right tx -> do
        let txSize = LBS.length $ serialize tx
        if txSize < fromIntegral (Ledger._maxTxSize pparams)
          then case evaluateTx tx (utxo <> knownUtxo) of
            (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) ->
              pure $ Just (NumUTxO $ length utxo, ValueSize sz, TxSize txSize, MemUnit mem, CpuUnit cpu)
            _ -> pure Nothing
          else pure Nothing

  genCommitTx utxo = do
    -- NOTE: number of parties is irrelevant for commit tx
    genHydraContextFor 1
      >>= ( genStInitialized
              >=> ( \stInitialized ->
                      pure (commit utxo stInitialized, getKnownUTxO stInitialized)
                  )
          )
  resizeValuesTo sz utxo =
    UTxO.fromPairs <$> foldMapM (resizeValue sz) (UTxO.pairs utxo)

  resizeValue sz (i, o) = do
    value <- fromLedgerValue <$> resize sz arbitrary
    pure [(i, modifyTxOutValue (const value) o)]

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
  forM
    ([1 .. 10] <> [20, 30 .. 100] <> [120, 140 .. 500])
    ( \numElems -> do
        utxo <- fmap Plutus.toBuiltin <$> genFakeUTxOs numElems

        let (memberMem, memberCpu) = fromRight (0, 0) $ executionCostForMember utxo
            (builderMem, builderCpu) = fromRight (0, 0) $ executionCostForBuilder utxo
        pure (numElems, MemUnit memberMem, CpuUnit memberCpu, MemUnit builderMem, CpuUnit builderCpu)
    )
 where
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

computeHashingCost :: [(Int, Int, [Either Text (Hash.HashAlgorithm, CpuUnit, MemUnit, CpuUnit, MemUnit)])]
computeHashingCost =
  [0 .. 5] <&> \(power :: Integer) ->
    let n = 8 ^ power
        s = n `quot` 8
     in ( n
        , s
        , [minBound .. maxBound] <&> \algorithm ->
            costOfHashingFor n algorithm
        )
 where
  costOfHashingFor n algorithm =
    let ExecutionUnits
          { executionSteps = baseCpu
          , executionMemory = baseMem
          } = either (error . ("unexpected failure evaluating baseline " <>) . show) id $ calculateHashExUnits n Hash.Base
     in calculateHashExUnits n algorithm <&> \ExecutionUnits{executionSteps = cpu, executionMemory = mem} ->
          (algorithm, CpuUnit baseCpu, MemUnit baseMem, CpuUnit cpu, MemUnit mem)

calculateHashExUnits :: Int -> Hash.HashAlgorithm -> Either Text ExecutionUnits
calculateHashExUnits n algorithm =
  case evaluateTx tx utxo of
    Left basicFailure ->
      Left ("Basic failure: " <> show basicFailure)
    Right report ->
      case Map.elems report of
        [Right units] ->
          Right $ fromLedgerExUnits units
        _ ->
          error $ "Too many redeemers in report: " <> show report
 where
  tx = unsafeBuildTransaction $ emptyTxBody & addInputs [(input, witness)]
  utxo = UTxO.singleton (input, output)
  input = generateWith arbitrary 42
  output = toCtxUTxOTxOut $ TxOut address value (mkTxOutDatum datum)
  value = lovelaceToValue 1_000_000
  address = mkScriptAddress @PlutusScriptV1 networkId script
  witness = BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness script (mkScriptDatum datum) redeemer
  script = fromPlutusScript @PlutusScriptV1 Hash.validatorScript
  datum = Hash.datum $ toBuiltin bytes
  redeemer = toScriptData $ Hash.redeemer algorithm
  bytes = fold $ replicate n ("0" :: ByteString)

maxMem, maxCpu :: Fixed E2
Ledger.ExUnits (fromIntegral @_ @(Fixed E2) -> maxMem) (fromIntegral @_ @(Fixed E2) -> maxCpu) =
  Ledger._maxTxExUnits pparams

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42
