{-# LANGUAGE DuplicateRecordFields #-}

module Test.BlockfrostChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (HydraTestnet (..))

import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.STM (takeTMVar)
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Exception (IOException)
import Control.Tracer.JSON (Tracer, showLogsOnFailure)
import Data.Secret (mkSecret, withSecret)
import Hydra.Cardano.Api (CardanoSigningKey (..), TxIn (..), TxIx (..), pattern TxOut, pattern TxOutDatumInline)
import Hydra.Chain (
  Chain (Chain, postTx),
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  initHistory,
 )
import Hydra.Chain.Backend (blockfrostProjectPath)
import Hydra.Chain.Blockfrost (newBlockfrostEnv, runBlockfrostBackend, runBlockfrostBackendWith, withBlockfrostChain)
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.Cardano (loadChainContext, mkTinyWallet)
import Hydra.Chain.Direct.Handlers (CardanoChainLog)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.ScriptRegistry (publishHydraScripts)
import Hydra.Cluster.Faucet (
  seedFromFaucetBlockfrost,
 )
import Hydra.Cluster.Fixture (
  Actor (Alice, Faucet),
  alice,
  aliceSk,
  blockfrostcperiod,
 )
import Hydra.Cluster.Util (chainConfigFor', keysFor)
import Hydra.Ledger.Cardano (Tx)
import Hydra.NetworkVersions (hydraNodeVersion, parseNetworkTxIds)
import Hydra.Options (
  BlockfrostOptions (..),
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  defaultBlockfrostOptions,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregate, sign)
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Hydra.Tx.Snapshot qualified as Snapshot
import Test.DirectChainSpec (
  CardanoChainTest (..),
  DirectChainTestLog (..),
  hasInitTxWith,
  loadParticipants,
  observesInTimeSatisfying',
  waitMatch,
 )

spec :: Spec
spec = around (onlyWithBlockfrostProjectFile . showLogsOnFailure "BlockfrostChainSpec") $ do
  -- Regression test for https://github.com/cardano-scaling/hydra/issues/2751: the
  -- Blockfrost API returns inline datums as base16-encoded CBOR text. Dropping the
  -- datum when converting queried UTxO loses the CRS datum of the script registry,
  -- which makes every fanout fail validation with H9 (NoOutputDatumError).
  -- We assert the published reference scripts keep their inline datum, skipping
  -- newScriptRegistry (its script-hash validation is unrelated here and breaks
  -- on any on-chain validator change).
  it "preserves inline datums when querying reference scripts @requiresBlockfrost" $ \_tracer -> do
    prj <- Blockfrost.projectFromFile blockfrostProjectPath
    -- Officially published hydra scripts for this network as recorded in networks.json
    hydraScriptsTxIds <- parseNetworkTxIds hydraNodeVersion "preview"
    utxo <-
      Blockfrost.runBlockfrostM prj $ do
        Blockfrost.Genesis{_genesisNetworkMagic} <- Blockfrost.queryGenesisParameters
        let networkId = Blockfrost.toCardanoNetworkId _genesisNetworkMagic
            candidates = [TxIn txid (TxIx 0) | txid <- hydraScriptsTxIds]
        Blockfrost.queryUTxOByTxIn networkId candidates
    let inlineOutputs = [txin | (txin, TxOut _ _ (TxOutDatumInline _) _) <- UTxO.toList utxo]
    when (null inlineOutputs) $
      failure $
        "Expected a published reference output to preserve its inline datum, but none did: " <> show utxo

  -- NOTE: re-running within a minute of an aborted run can fail on submission with "all inputs are spent",
  -- because the shared faucet's address index may not yet reflect the previous process's transactions;
  -- wait a minute and re-run. (The retry hardening stays deferred; it's recorded in the plan.)
  it "can open, close & fanout a Head using Blockfrost @requiresBlockfrost" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      (_, sk) <- keysFor Faucet
      prj <- Blockfrost.projectFromFile blockfrostProjectPath
      (aliceCardanoVk, _) <- keysFor Alice
      let blockfrostOpts = defaultBlockfrostOptions{projectPath = blockfrostProjectPath}
      hydraScriptsTxId <- runBlockfrostBackend blockfrostOpts $ publishHydraScripts (withSecret sk (mkSecret . CardanoSigningKey))

      Blockfrost.Genesis
        { _genesisNetworkMagic
        , _genesisSystemStart
        , _genesisSlotLength
        , _genesisActiveSlotsCoefficient
        } <-
        Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters

      let blockTime :: NominalDiffTime
          blockTime = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient
      -- Inclusion takes 1-2 blocks and the follower observes with ~1 block of
      -- confirmation lag plus one poll interval; 6 block times gives margin.
      let observationTimeout = 6 * blockTime

      -- Alice setup
      aliceChainConfig <- chainConfigFor' Alice tmp (Blockfrost blockfrostOpts) hydraScriptsTxId [] blockfrostcperiod (DepositPeriod 100) (DepositPeriod 100)

      withBlockfrostChainTest (contramap (FromBlockfrostChain "alice") tracer) aliceChainConfig alice $
        \aliceChain@CardanoChainTest{postTx} -> do
          _ <- Blockfrost.runBlockfrostM prj $ seedFromFaucetBlockfrost aliceCardanoVk 100_000_000
          -- Scenario
          participants <- loadParticipants [Alice]
          let headParameters = HeadParameters blockfrostcperiod (DepositPeriod 100) [alice]
          postTx $ InitTx{participants, headParameters}
          (headId, headSeed) <- observesInTimeSatisfying' aliceChain observationTimeout $ hasInitTxWith headParameters participants

          let snapshotVersion = 0
          let emptyUTxO :: UTxOType Tx = mempty
          let accumulator = Accumulator.buildFromUTxO emptyUTxO
          let snapshot =
                Snapshot
                  { headId
                  , number = 1
                  , utxo = emptyUTxO
                  , confirmed = []
                  , utxoToCommit = Nothing
                  , utxoToDecommit = Nothing
                  , depositTxId = Nothing
                  , version = snapshotVersion
                  , accumulator
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
              , utxoForProof = Snapshot.utxo snapshot <> fold (Snapshot.utxoToCommit snapshot) <> fold (Snapshot.utxoToDecommit snapshot)
              , headSeed
              , contestationDeadline = deadline
              }
          let expectedUTxO =
                (Snapshot.utxo snapshot <> fromMaybe mempty (Snapshot.utxoToCommit snapshot))
                  `withoutUTxO` fromMaybe mempty (Snapshot.utxoToDecommit snapshot)
          observesInTimeSatisfying' aliceChain observationTimeout $ \case
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
  (configuration, blockfrostOptions) <-
    case config of
      Cardano cfg@CardanoChainConfig{chainBackendOptions} ->
        case chainBackendOptions of
          Blockfrost bfOpts -> pure (cfg, bfOpts)
          _ -> failure $ "unexpected chainBackendOptions: " <> show chainBackendOptions
      otherConfig -> failure $ "unexpected chainConfig: " <> show otherConfig
  env <- newBlockfrostEnv blockfrostOptions
  ctx <- runBlockfrostBackendWith env $ loadChainContext configuration party
  eventMVar <- newLabelledEmptyTMVarIO "blockfrost-chain-events"

  let callback event = atomically $ putTMVar eventMVar event

  wallet <- mkTinyWallet (runBlockfrostBackendWith env) tracer configuration
  withBlockfrostChain env tracer configuration ctx wallet (initHistory initialChainState) callback $ \Chain{postTx} -> do
    action
      CardanoChainTest
        { postTx
        , waitCallback = atomically $ takeTMVar eventMVar
        }
