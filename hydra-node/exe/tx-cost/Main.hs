{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Monad.Writer (runWriterT, tell)
import Data.ByteString (hPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed (E2, Fixed)
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
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  progDesc,
  short,
  strOption,
 )
import Plutus.MerkleTree (rootHash)
import qualified Plutus.MerkleTree as MT
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (toBuiltin, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Plutus.Validator (
  ExUnits (ExUnits),
  evaluateScriptExecutionUnits,
 )
import Test.QuickCheck (generate, resize, vectorOf)
import Validators (merkleTreeValidator, mtBuilderValidator)

newtype Options = Options {outputDirectory :: Maybe FilePath}

txCostOptionsParser :: Parser Options
txCostOptionsParser =
  Options
    <$> optional
      ( strOption
          ( long "output-directory"
              <> short 'o'
              <> metavar "DIR"
              <> help
                "Directory where benchmark files should be output to. \
                \ If none is given, output is sent to stdout"
          )
      )

logFilterOptions :: ParserInfo Options
logFilterOptions =
  info
    (txCostOptionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Runs benchmarks assessing the execution cost of various on-chain \
          \ constructs: Some specific Plutus code, all OCV transactions,... \
          \ The output is valid markdown that can be used as is to be processed \
          \ and published."
        <> header "tx-cost - Hydra OCV Code Benchmarks"
    )

main :: IO ()
main =
  execParser logFilterOptions >>= \case
    Options{outputDirectory = Nothing} -> writeTransactionCostMarkdown stdout
    Options{outputDirectory = Just outputDir} -> do
      unlessM (doesDirectoryExist outputDir) $ createDirectoryIfMissing True outputDir
      withFile (outputDir </> "transaction-cost.md") WriteMode writeTransactionCostMarkdown

writeTransactionCostMarkdown :: Handle -> IO ()
writeTransactionCostMarkdown hdl = do
  initC <- costOfInit
  commitC <- costOfCommit
  collectComC <- costOfCollectCom
  fanout <- costOfFanOut
  mt <- costOfMerkleTree
  h <- costOfHashing
  hPut hdl $ encodeUtf8 $ unlines $ pageHeader <> intersperse "" [initC, commitC, collectComC, fanout, mt, h]

pageHeader :: [Text]
pageHeader =
  [ "--- "
  , "sidebar_label: 'Transactions Costs' "
  , "sidebar_position: 3 "
  , "--- "
  , ""
  , "# Transactions Costs "
  , ""
  , "|  |   |"
  , "| ---    | ----    |"
  , "| _Generated at_ | " <> show now <> " |"
  , "| _Max. memory units_ | " <> show maxMem <> " |"
  , "| _Max. CPU units_ | " <> show maxCpu <> " |"
  , "| _Max. tx size (kB)_ | " <> show (Ledger._maxTxSize pparams) <> " |"
  , ""
  ]

{-# NOINLINE now #-}
now :: UTCTime
now = unsafePerformIO getCurrentTime

costOfInit :: IO Text
costOfInit = fmap (unlines . snd) $
  runWriterT $ do
    tell ["## Cost of Init Transaction"]
    tell [""]
    tell ["| UTXO  | Tx. size |"]
    tell ["| :---- | -------: |"]
    forM_ [1 .. 100] $ \numParties -> do
      tx <- lift $ generate $ genInitTx' numParties
      let txSize = LBS.length $ serialize tx
      when (txSize < fromIntegral (Ledger._maxTxSize pparams)) $
        tell
          [ "| " <> show numParties
              <> "| "
              <> show txSize
              <> " | "
          ]
 where
  genInitTx' numParties = do
    genHydraContextFor numParties >>= \ctx ->
      genStIdle ctx >>= \stIdle ->
        genTxIn >>= \seedInput ->
          pure $ initialize (ctxHeadParameters ctx) (ctxVerificationKeys ctx) seedInput stIdle

costOfCommit :: IO Text
costOfCommit = fmap (unlines . snd) $
  runWriterT $ do
    tell ["## Cost of Commit Transaction"]
    tell [""]
    tell ["| # UTxO Committed | Assets size | Tx. size | % max Mem |   % max CPU |"]
    tell ["| :--------------- | ----------: | -------: | --------: | ----------: |"]
    forM_ [0 .. 100] $ \numUTxO -> do
      let valueSizes =
            if numUTxO == 0
              then [0]
              else 1 : [5, 10 .. 100]
      forM_ valueSizes $ \sz -> do
        utxo <-
          lift $
            generate $
              genSimpleUTxOOfSize numUTxO
                >>= resizeValuesTo sz
        (commitTx, knownUtxo) <- lift $ generate $ genCommitTx utxo
        case commitTx of
          Left _ -> pure ()
          Right tx -> do
            let txSize = LBS.length $ serialize tx
            when (txSize < fromIntegral (Ledger._maxTxSize pparams)) $
              case evaluateTx tx (utxo <> knownUtxo) of
                (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) ->
                  tell
                    [ "| " <> show (length utxo)
                        <> "| "
                        <> show sz
                        <> "| "
                        <> show txSize
                        <> " | "
                        <> show (100 * fromIntegral mem / maxMem)
                        <> " | "
                        <> show (100 * fromIntegral cpu / maxCpu)
                        <> " |"
                    ]
                _ -> pure ()
 where
  genCommitTx utxo = do
    -- NOTE: number of parties is irrelevant for commit tx
    genHydraContextFor 3
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

costOfCollectCom :: IO Text
costOfCollectCom = markdownCollectComCost <$> computeCollectComCost
 where
  markdownCollectComCost stats =
    unlines $
      [ "## Cost of CollectCom Transaction"
      , ""
      , "| # Parties | Tx. size | % max Mem |   % max CPU |"
      , "| :-------- | -------: | --------: | ----------: |"
      ]
        <> fmap
          ( \(numParties, txSize, mem, cpu) ->
              "| " <> show numParties
                <> "| "
                <> show txSize
                <> " | "
                <> show (100 * fromIntegral mem / maxMem)
                <> " | "
                <> show (100 * fromIntegral cpu / maxCpu)
                <> " |"
          )
          stats

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

  genCollectComTx numParties = do
    genHydraContextFor numParties
      >>= \ctx ->
        genInitTx ctx >>= \initTx ->
          genCommits ctx initTx >>= \commits ->
            genStIdle ctx >>= \stIdle ->
              let stInitialized = executeCommits initTx commits stIdle
               in pure (collect stInitialized, getKnownUTxO stInitialized)

costOfFanOut :: IO Text
costOfFanOut = fmap (unlines . snd) $
  runWriterT $ do
    tell ["## Cost of FanOut validator"]
    tell [""]
    tell ["| UTXO  | Tx. size | % max Mem |   % max CPU |"]
    tell ["| :---- | -------: | --------: | ----------: |"]
    forM_ [1 .. 100] $ \numElems -> do
      utxo <- lift $ generate (genSimpleUTxOOfSize numElems)
      let (tx, lookupUTxO) = mkFanoutTx utxo
      case evaluateTx tx lookupUTxO of
        (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) ->
          tell
            [ "| " <> show numElems
                <> "| "
                <> show (LBS.length $ serialize tx)
                <> " | "
                <> show (100 * fromIntegral mem / maxMem)
                <> " | "
                <> show (100 * fromIntegral cpu / maxCpu)
                <> " |"
            ]
        _ ->
          pure ()

maxMem, maxCpu :: Fixed E2
Ledger.ExUnits (fromIntegral @_ @(Fixed E2) -> maxMem) (fromIntegral @_ @(Fixed E2) -> maxCpu) =
  Ledger._maxTxExUnits pparams

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

costOfMerkleTree :: IO Text
costOfMerkleTree = fmap (unlines . snd) $
  runWriterT $ do
    tell ["## Cost of on-chain Merkle-Tree"]
    tell [""]
    tell ["| Size | % member max mem | % member max cpu | % builder max mem | % builder max cpu |"]
    tell ["| :--- | ---------------: | ---------------: | ----------------: | ----------------: |"]
    forM_ ([1 .. 10] <> [20, 30 .. 100] <> [120, 140 .. 500]) $ \numElems -> do
      utxo <- lift $ fmap Plutus.toBuiltin <$> genFakeUTxOs numElems

      let (memberMem, memberCpu) = fromRight (0, 0) $ executionCostForMember utxo
          (builderMem, builderCpu) = fromRight (0, 0) $ executionCostForBuilder utxo
      tell
        [ "| "
            <> show numElems
            <> " | "
            <> show (100 * fromIntegral (fromIntegral memberMem `div` numElems) / maxMem)
            <> " | "
            <> show (100 * fromIntegral (fromIntegral memberCpu `div` numElems) / maxCpu)
            <> " | "
            <> show (100 * fromIntegral builderMem / maxMem)
            <> " | "
            <> show (100 * fromIntegral builderCpu / maxCpu)
            <> " |"
        ]
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

costOfHashing :: IO Text
costOfHashing = fmap (unlines . snd) $
  runWriterT $ do
    tell ["##  Cost of on-chain Hashing"]
    tell [""]
    for_ [0 .. 5] $ \(power :: Integer) -> do
      let n = 8 ^ power
          s = n `quot` 8
      tell ["###  n = " <> show n <> ", s = " <> show s]
      tell [""]
      tell ["| Algorithm | Cpu  | Mem  | Δcpu | Δmem |"]
      tell ["| :-------- | ---: | ---: | ---: | ---: |"]
      for_ [minBound .. maxBound] $ \algorithm ->
        costOfHashingFor n algorithm
      tell [""]
 where
  costOfHashingFor n algorithm = do
    let ExecutionUnits
          { executionSteps = baseCpu
          , executionMemory = baseMem
          } = either (error . ("unexpected failure evaluating baseline " <>) . show) id $ calculateHashExUnits n Hash.Base

    case calculateHashExUnits n algorithm of
      Right
        ExecutionUnits
          { executionSteps = cpu
          , executionMemory = mem
          } ->
          tell
            [ "| " <> show algorithm
                <> " | "
                <> show baseCpu
                <> " | "
                <> show baseMem
                <> " | "
                <> show (toInteger cpu - toInteger baseCpu)
                <> " | "
                <> show (toInteger mem - toInteger baseMem)
                <> " |"
            ]
      Left _ -> tell ["| - | - |"]

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
