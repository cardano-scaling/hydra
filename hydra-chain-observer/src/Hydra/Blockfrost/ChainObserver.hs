{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Blockfrost.ChainObserver where

import Hydra.Prelude

import Hydra.Logging (Tracer, traceWith)

import Blockfrost.Client (
  runBlockfrost,
 )
import Control.Retry (RetryPolicyM, exponentialBackoff, limitRetries)

import Hydra.Blockfrost.Adapter (toChainPoint)
import Hydra.Cardano.Api (BlockNo, HasTypeProxy (..), NetworkId, SerialiseAsCBOR (..), Tx, UTxO)
import Hydra.Cardano.Api.Prelude (ChainPoint (..))
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.ChainObserver.NodeClient (
  ChainObservation (..),
  ChainObserverLog (..),
  NodeClient (..),
  ObserverHandler,
  logOnChainTx,
  observeAll,
 )
import Hydra.Tx (txId)

import Blockfrost.Client qualified as Blockfrost

blockfrostClient ::
  Tracer IO ChainObserverLog ->
  NodeClient IO
blockfrostClient tracer = do
  -- TODO! state mgmt
  let initial :: UTxO
      initial = mempty
  NodeClient
    { follow = \networkId _startChainFrom observerHandler -> do
        -- reads token from BLOCKFROST_TOKEN_PATH
        -- environment variable. It expects token
        -- prefixed with Blockfrost environment name
        -- e.g.: testnet-someTokenHash
        prj <- Blockfrost.projectFromEnv
        -- TODO! prj carries an environment and a token itself.
        traceWith tracer ConnectingToExternalNode{networkId}
        latestBlock <-
          either (error . show) id
            <$> runBlockfrost prj Blockfrost.getLatestBlock

        let chainPoint = toChainPoint latestBlock
        traceWith tracer StartObservingFrom{chainPoint}
        loop tracer prj latestBlock networkId observerHandler initial
    }

loop ::
  Tracer IO ChainObserverLog ->
  Blockfrost.Project ->
  Blockfrost.Block ->
  NetworkId ->
  ObserverHandler IO ->
  UTxO ->
  IO b
loop tracer prj latestBlock@Blockfrost.Block{_blockHeight} networkId observerHandler utxo = do
  -- TODO! handle missing blockNo
  let blockNo = maybe 0 fromInteger _blockHeight

  -- TODO! use pagination: now it queries 100 entries.
  txHashes <-
    either (error . show) id
      <$> runBlockfrost prj (Blockfrost.getBlockTxs $ Right (Blockfrost._blockHash latestBlock))

  -- FIXME!
  txs <-
    either (error . show) toCardanoAPI . sequence
      <$> traverse
        getTxCBOR
        txHashes

  let (utxo', observations) = observeAll networkId utxo txs
      onChainTxs = mapMaybe convertObservation observations

  let point = toChainPoint latestBlock
  forM_ onChainTxs (traceWith tracer . logOnChainTx)
  let observationsAt = HeadObservation point blockNo <$> onChainTxs
  observerHandler $
    if null observationsAt
      then [Tick point blockNo]
      else observationsAt

  -- wait some time
  latestBlock' <-
    either (error . show) id
      <$> runBlockfrost prj Blockfrost.getLatestBlock

  loop tracer prj latestBlock' networkId observerHandler utxo'

-- FIXME: (runBlockfrost prj . Blockfrost.getTxCBOR)
getTxCBOR :: Blockfrost.TxHash -> IO (Either Text Text)
getTxCBOR = const $ pure $ Right "TxCbor"

toCardanoAPI :: [Text] -> [Tx]
toCardanoAPI txs =
  ( \txCbor ->
      case decodeBase16 txCbor of
        Left decodeErr -> error $ "Bad Base16 Tx CBOR: " <> decodeErr
        Right bytes ->
          case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
            Left deserializeErr -> error $ "Bad Tx CBOR: " <> show deserializeErr
            Right tx -> tx
  )
    <$> txs

-- TODO! DRY
rollForward ::
  Tracer IO ChainObserverLog ->
  NetworkId ->
  ChainPoint ->
  BlockNo ->
  UTxO ->
  [Tx] ->
  ObserverHandler IO ->
  IO UTxO
rollForward tracer networkId point blockNo currentUTxO receivedTxs observerHandler = do
  let receivedTxIds = txId <$> receivedTxs
  traceWith tracer RollForward{point, receivedTxIds}
  let (adjustedUTxO, observations) = observeAll networkId currentUTxO receivedTxs
  let onChainTxs = mapMaybe convertObservation observations
  forM_ onChainTxs (traceWith tracer . logOnChainTx)
  let observationsAt = HeadObservation point blockNo <$> onChainTxs
  observerHandler $
    if null observationsAt
      then [Tick point blockNo]
      else observationsAt
  pure adjustedUTxO

-- TODO! DRY
rollBackward ::
  Tracer IO ChainObserverLog ->
  ChainPoint ->
  b ->
  IO b
rollBackward tracer point currentUTxO = do
  traceWith tracer Rollback{point}
  pure currentUTxO

-- * Helpers

retryPolicy :: MonadIO m => RetryPolicyM m
retryPolicy = exponentialBackoff 50000 <> limitRetries 5
