{-# LANGUAGE DuplicateRecordFields #-}

module Test.BlockfrostChainSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import Test.DirectChainSpec (
  CardanoChainTest (..),
  DirectChainTestLog (..),
  externalCommit',
  hasInitTxWith,
  loadParticipants,
  observesInTime',
  observesInTimeSatisfying',
  waitMatch,
 )
import "QuickCheck" Test.QuickCheck (generate)
import "base" Control.Exception (IOException)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-cluster" Hydra.Cluster.Faucet (
  seedFromFaucetBlockfrost,
 )
import "hydra-cluster" Hydra.Cluster.Fixture (
  Actor (Alice, Faucet),
  alice,
  aliceSk,
  blockfrostcperiod,
 )
import "hydra-cluster" Hydra.Cluster.Util (chainConfigFor', keysFor)
import "hydra-node" Hydra.Chain (
  Chain (Chain, draftCommitTx, postTx),
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  initHistory,
 )
import "hydra-node" Hydra.Chain.Backend (blockfrostProjectPath)
import "hydra-node" Hydra.Chain.Blockfrost (BlockfrostBackend (..), withBlockfrostChain)
import "hydra-node" Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import "hydra-node" Hydra.Chain.Cardano (loadChainContext, mkTinyWallet)
import "hydra-node" Hydra.Chain.Direct.Handlers (CardanoChainLog)
import "hydra-node" Hydra.Chain.Direct.State (initialChainState)
import "hydra-node" Hydra.Chain.ScriptRegistry (publishHydraScripts)
import "hydra-node" Hydra.Logging (Tracer, showLogsOnFailure)
import "hydra-node" Hydra.Node.DepositPeriod (DepositPeriod (..))
import "hydra-node" Hydra.Options (
  BlockfrostOptions (..),
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  defaultBlockfrostOptions,
 )
import "hydra-tx" Hydra.Ledger.Cardano (Tx)
import "hydra-tx" Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import "hydra-tx" Hydra.Tx.Crypto (aggregate, sign)
import "hydra-tx" Hydra.Tx.HeadParameters (HeadParameters (..))
import "hydra-tx" Hydra.Tx.IsTx (IsTx (..))
import "hydra-tx" Hydra.Tx.Party (Party)
import "hydra-tx" Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import "hydra-tx" Hydra.Tx.Snapshot qualified as Snapshot
import "hydra-tx" Test.Hydra.Tx.Gen (genKeyPair)
import "stm" Control.Concurrent.STM (takeTMVar)
import "stm" Control.Concurrent.STM.TMVar (putTMVar)
import "time" Data.Time (secondsToNominalDiffTime)

spec :: Spec
spec = around (onlyWithBlockfrostProjectFile . showLogsOnFailure "BlockfrostChainSpec") $ do
  it "can open, close & fanout a Head using Blockfrost" $ \tracer -> do
    pendingWith "Blockfrost tests should run only as part of smoke-tests because they are very slow"
    withTempDir "hydra-cluster" $ \tmp -> do
      (_, sk) <- keysFor Faucet
      prj <- Blockfrost.projectFromFile blockfrostProjectPath
      (aliceCardanoVk, _) <- keysFor Alice
      (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
      let backend = BlockfrostBackend $ defaultBlockfrostOptions{projectPath = blockfrostProjectPath}
      hydraScriptsTxId <- publishHydraScripts backend sk

      Blockfrost.Genesis
        { _genesisNetworkMagic
        , _genesisSystemStart
        } <-
        Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters

      -- Alice setup
      aliceChainConfig <- chainConfigFor' Alice tmp backend hydraScriptsTxId [] blockfrostcperiod (DepositPeriod 100)

      withBlockfrostChainTest (contramap (FromBlockfrostChain "alice") tracer) aliceChainConfig alice $
        \aliceChain@CardanoChainTest{postTx} -> do
          _ <- Blockfrost.runBlockfrostM prj $ seedFromFaucetBlockfrost defaultBlockfrostOptions aliceCardanoVk 100_000_000
          someUTxO <- Blockfrost.runBlockfrostM prj $ seedFromFaucetBlockfrost defaultBlockfrostOptions aliceExternalVk 7_000_000
          -- Scenario
          participants <- loadParticipants [Alice]
          let headParameters = HeadParameters blockfrostcperiod [alice]
          postTx $ InitTx{participants, headParameters}
          (headId, headSeed) <- observesInTimeSatisfying' aliceChain (secondsToNominalDiffTime $ fromIntegral $ queryTimeout defaultBlockfrostOptions) $ hasInitTxWith headParameters participants

          let blueprintTx = txSpendingUTxO someUTxO
          externalCommit' backend aliceChain [aliceExternalSk] headId someUTxO blueprintTx
          aliceChain `observesInTime'` OnCommitTx headId alice someUTxO

          postTx $ CollectComTx someUTxO headId headParameters
          aliceChain `observesInTime'` OnCollectComTx{headId}

          let snapshotVersion = 0
          let snapshot =
                Snapshot
                  { headId
                  , number = 1
                  , utxo = someUTxO
                  , confirmed = []
                  , utxoToCommit = Nothing
                  , utxoToDecommit = Nothing
                  , version = snapshotVersion
                  }

          postTx $ CloseTx headId headParameters snapshotVersion (ConfirmedSnapshot{snapshot, signatures = aggregate [sign aliceSk snapshot]})

          deadline <-
            waitMatch aliceChain $ \case
              Observation{observedTx = OnCloseTx{snapshotNumber, contestationDeadline}}
                | snapshotNumber == 1 -> Just contestationDeadline
              _ -> Nothing

          waitMatch aliceChain $ \case
            Tick t _ | t > deadline -> Just ()
            _ -> Nothing
          postTx $
            FanoutTx
              { utxo = Snapshot.utxo snapshot
              , utxoToCommit = Nothing
              , utxoToDecommit = Nothing
              , headSeed
              , contestationDeadline = deadline
              }
          let expectedUTxO =
                (Snapshot.utxo snapshot <> fromMaybe mempty (Snapshot.utxoToCommit snapshot))
                  `withoutUTxO` fromMaybe mempty (Snapshot.utxoToDecommit snapshot)
          observesInTimeSatisfying' aliceChain (secondsToNominalDiffTime $ fromIntegral $ queryTimeout defaultBlockfrostOptions) $ \case
            OnFanoutTx{headId = headId', fanoutUTxO}
              | headId' == headId ->
                  if UTxO.containsOutputs fanoutUTxO (UTxO.txOutputs expectedUTxO)
                    then pure ()
                    else failure "OnFanoutTx does not contain expected UTxO"
            _ -> failure "expected OnFanoutTx"
 where
  onlyWithBlockfrostProjectFile action = do
    try (Blockfrost.projectFromFile blockfrostProjectPath) >>= \case
      Left (_ :: IOException) -> pendingWith "Requires Blockfrost project file"
      Right _ -> action

-- | Wrapper around 'withBlockfrostChain' that threads a 'ChainStateType tx' through
-- 'postTx' and 'waitCallback' calls.
withBlockfrostChainTest ::
  Tracer IO CardanoChainLog ->
  ChainConfig ->
  Party ->
  (CardanoChainTest Tx IO -> IO a) ->
  IO a
withBlockfrostChainTest tracer config party action = do
  (configuration, backend) <-
    case config of
      Cardano cfg@CardanoChainConfig{chainBackendOptions} ->
        case chainBackendOptions of
          Blockfrost blockfrostOptions -> pure (cfg, BlockfrostBackend blockfrostOptions)
          _ -> failure $ "unexpected chainBackendOptions: " <> show chainBackendOptions
      otherConfig -> failure $ "unexpected chainConfig: " <> show otherConfig
  ctx <- loadChainContext backend configuration party
  eventMVar <- newLabelledEmptyTMVarIO "blockfrost-chain-events"

  let callback event = atomically $ putTMVar eventMVar event

  wallet <- mkTinyWallet backend tracer configuration
  withBlockfrostChain backend tracer configuration ctx wallet (initHistory initialChainState) callback $ \Chain{postTx, draftCommitTx} -> do
    action
      CardanoChainTest
        { postTx
        , waitCallback = atomically $ takeTMVar eventMVar
        , draftCommitTx = \headId utxo blueprintTx -> do
            eTx <- draftCommitTx headId CommitBlueprintTx{lookupUTxO = utxo, blueprintTx}
            case eTx of
              Left e -> throwIO e
              Right tx -> pure tx
        }
