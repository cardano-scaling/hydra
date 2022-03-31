{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module TxCost where

import Hydra.Prelude hiding (catch)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import Cardano.Ledger.Era (hashScript)
import qualified Cardano.Ledger.Shelley.API as API
import qualified Cardano.Ledger.Val as Ledger
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import Hydra.Cardano.Api (
  BuildTxWith (BuildTxWith),
  ExecutionUnits (..),
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PlutusScriptV1,
  StandardCrypto,
  Tx,
  UTxO,
  fromLedgerExUnits,
  fromLedgerScript,
  fromLedgerTxOut,
  fromLedgerValue,
  fromPlutusData,
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
  genCommits,
  genHydraContextFor,
  genInitTx,
  genStIdle,
  genStInitialized,
 )
import Hydra.Chain.Direct.State (collect, commit, getKnownUTxO, initialize)
import Hydra.Chain.Direct.Tx (fanoutTx)
import qualified Hydra.Contract.Hash as Hash
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Ledger.Cardano (
  adaOnly,
  addInputs,
  emptyTxBody,
  genKeyPair,
  genOneUTxOFor,
  genTxIn,
  hashTxOuts,
  simplifyUTxO,
  unsafeBuildTransaction,
 )
import Hydra.Ledger.Cardano.Evaluate (evaluateTx, pparams)
import Plutus.MerkleTree (rootHash)
import qualified Plutus.MerkleTree as MT
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (toBuiltin, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Plutus.Validator (
  ExUnits (ExUnits),
  evaluateScriptExecutionUnits,
 )
import Test.QuickCheck (generate, resize, vectorOf)
import Validators (merkleTreeValidator, mtBuilderValidator)

computeInitCost :: IO [(Int, Int64)]
computeInitCost =
  catMaybes
    <$> forM
      [1 .. 100]
      ( \numParties -> do
          tx <- generate $ genInitTx' numParties
          let txSize = LBS.length $ serialize tx
          if txSize < fromIntegral (Ledger._maxTxSize pparams)
            then pure (Just (numParties, txSize))
            else pure Nothing
      )
 where
  genInitTx' numParties = do
    genHydraContextFor numParties >>= \ctx ->
      genStIdle ctx >>= \stIdle ->
        genTxIn >>= \seedInput ->
          pure $ initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle

computeCommitCost :: IO [(Int, Int, Int64, Natural, Natural)]
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
              ( \sz -> do
                  utxo <-
                    generate $
                      genSimpleUTxOOfSize numUTxO
                        >>= resizeValuesTo sz
                  (commitTx, knownUtxo) <- generate $ genCommitTx utxo
                  case commitTx of
                    Left _ -> pure Nothing
                    Right tx -> do
                      let txSize = LBS.length $ serialize tx
                      if txSize < fromIntegral (Ledger._maxTxSize pparams)
                        then case evaluateTx tx (utxo <> knownUtxo) of
                          (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) ->
                            pure $ Just (length utxo, sz, txSize, mem, cpu)
                          _ -> pure Nothing
                        else pure Nothing
              )
      )
 where
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

computeCollectComCost :: IO [(Int, Int64, Natural, Natural)]
computeCollectComCost =
  catMaybes
    <$> forM
      [1 .. 100]
      ( \numParties -> do
          (tx, knownUtxo) <- generate $ genCollectComTx numParties
          let txSize = LBS.length $ serialize tx
          if txSize < fromIntegral (Ledger._maxTxSize pparams)
            then case evaluateTx tx knownUtxo of
              (Right (mconcat . rights . Map.elems -> (Ledger.ExUnits mem cpu))) ->
                pure $ Just (numParties, txSize, mem, cpu)
              _ -> pure Nothing
            else pure Nothing
      )
 where
  genCollectComTx numParties = do
    genHydraContextFor numParties
      >>= \ctx ->
        genInitTx ctx >>= \initTx ->
          genCommits ctx initTx >>= \commits ->
            genStIdle ctx >>= \stIdle ->
              let stInitialized = executeCommits initTx commits stIdle
               in pure (collect stInitialized, getKnownUTxO stInitialized)

computeFanOutCost :: IO [(Int, Int64, Natural, Natural)]
computeFanOutCost =
  catMaybes
    <$> forM
      [1 .. 100]
      ( \numElems -> do
          utxo <- generate (genSimpleUTxOOfSize numElems)
          let (tx, lookupUTxO) = mkFanoutTx utxo
          case evaluateTx tx lookupUTxO of
            (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) ->
              pure (Just (numElems, LBS.length $ serialize tx, mem, cpu))
            _ ->
              pure Nothing
      )

genSimpleUTxOOfSize :: Int -> Gen UTxO
genSimpleUTxOOfSize numUTxO =
  foldMap simplifyUTxO
    <$> vectorOf
      numUTxO
      ( genKeyPair >>= fmap (fmap adaOnly) . genOneUTxOFor . fst
      )

mkFanoutTx :: UTxO -> (Tx, UTxO)
mkFanoutTx utxo =
  (tx, lookupUTxO)
 where
  tx = fanoutTx utxo (headInput, headOutput, fromPlutusData headDatum) (fromLedgerScript headScript)
  headInput = generateWith arbitrary 42
  headScript = plutusScript Head.validatorScript
  headOutput = fromLedgerTxOut $ mkHeadOutput (SJust $ Ledger.Data headDatum)
  headDatum =
    toData $
      Head.Closed 1 (toBuiltin $ hashTxOuts $ toList utxo)

  lookupUTxO = UTxO.singleton (headInput, headOutput)

mkHeadOutput :: StrictMaybe (Ledger.Data LedgerEra) -> Ledger.Alonzo.TxOut LedgerEra
mkHeadOutput headDatum =
  Ledger.Alonzo.TxOut headAddress headValue headDatumHash
 where
  headAddress = scriptAddr $ plutusScript Head.validatorScript
  headValue = Ledger.inject (API.Coin 2_000_000)
  headDatumHash = Ledger.hashData @LedgerEra <$> headDatum

computeMerkleTreeCost :: IO [(Int, Natural, Natural, Natural, Natural)]
computeMerkleTreeCost =
  forM
    ([1 .. 10] <> [20, 30 .. 100] <> [120, 140 .. 500])
    ( \numElems -> do
        utxo <- fmap Plutus.toBuiltin <$> genFakeUTxOs numElems

        let (memberMem, memberCpu) = fromRight (0, 0) $ executionCostForMember utxo
            (builderMem, builderCpu) = fromRight (0, 0) $ executionCostForBuilder utxo
        pure (numElems, memberMem, memberCpu, builderMem, builderCpu)
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
           in case evaluateScriptExecutionUnits merkleTreeValidator (e, MT.rootHash tree, proof) of
                Right (ExUnits mem cpu) ->
                  Right (mem + curMem, cpu + curCpu)
                Left err -> Left err
   in foldr accumulateCost (Right (0, 0)) utxo

executionCostForBuilder :: [Plutus.BuiltinByteString] -> Either Text (Natural, Natural)
executionCostForBuilder utxo =
  let tree = MT.fromList utxo
      root = rootHash tree
   in evaluateScriptExecutionUnits mtBuilderValidator (utxo, root) <&> \case
        ExUnits mem cpu -> (mem, cpu)

computeHashingCost :: [(Int, Int, [Either Text (Hash.HashAlgorithm, Natural, Natural, Natural, Natural)])]
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
     in calculateHashExUnits n algorithm <&> \ExecutionUnits{executionSteps = cpu, executionMemory = mem} -> (algorithm, baseCpu, baseMem, cpu, mem)

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
  address = mkScriptAddress @PlutusScriptV1 (Testnet $ NetworkMagic 42) script
  witness = BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness script (mkScriptDatum datum) redeemer
  script = fromPlutusScript @PlutusScriptV1 Hash.validatorScript
  datum = Hash.datum $ toBuiltin bytes
  redeemer = toScriptData $ Hash.redeemer algorithm
  bytes = fold $ replicate n ("0" :: ByteString)

-- | Get the ledger address for a given plutus script.
scriptAddr :: Ledger.Script LedgerEra -> API.Addr StandardCrypto
scriptAddr script =
  API.Addr
    API.Testnet
    (API.ScriptHashObj $ hashScript @LedgerEra script)
    API.StakeRefNull

plutusScript :: Plutus.Script -> Ledger.Script LedgerEra
plutusScript = Ledger.PlutusScript PlutusV1 . toShort . fromLazy . serialize
