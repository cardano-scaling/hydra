{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Blockfrost.ChainObserver where

import Hydra.Prelude

import Hydra.Logging (Tracer, traceWith)

import Blockfrost.Client (
  Block (..),
  getLatestBlock,
  projectFromEnv,
  runBlockfrost,
  unBlockHash,
  unSlot,
 )
import Control.Retry (RetryPolicyM, exponentialBackoff, limitRetries)

import Hydra.Cardano.Api (BlockNo, Hash, NetworkId, SlotNo, Tx, UTxO)
import Hydra.Cardano.Api.Prelude (BlockHeader (..), ChainPoint (..))
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.ChainObserver.NodeClient (ChainObservation (..), ChainObserverLog (..), NodeClient (..), ObserverHandler, logOnChainTx, observeAll)
import Hydra.Tx (txId)

blockfrostClient ::
  Tracer IO ChainObserverLog ->
  NodeClient IO
blockfrostClient tracer = do
  NodeClient
    { follow = \networkId startChainFrom _observerHandler -> do
        -- reads token from BLOCKFROST_TOKEN_PATH
        -- environment variable. It expects token
        -- prefixed with Blockfrost environment name
        -- e.g.: testnet-someTokenHash
        prj <- projectFromEnv
        -- TODO! prj carries an environment and a token itself.
        traceWith tracer ConnectingToExternalNode{networkId}
        chainPoint <- case startChainFrom of
          Nothing -> do
            latestBlocks <- runBlockfrost prj getLatestBlock
            case latestBlocks of
              Left blockfrostError -> do
                -- TODO! retry depending on error kind
                fail $ show blockfrostError
              Right block ->
                pure $ toChainPoint block
          Just x -> pure x
        traceWith tracer StartObservingFrom{chainPoint}

        void $ runBlockfrost prj $ do
          pure ()
    }

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

toChainPoint :: Block -> ChainPoint
toChainPoint Block{_blockSlot, _blockHash} =
  ChainPoint slotNo headerHash
 where
  slotNo :: SlotNo
  slotNo = maybe 0 (fromInteger . unSlot) _blockSlot

  headerHash :: Hash BlockHeader
  headerHash = fromString . toString $ unBlockHash _blockHash

retryPolicy :: MonadIO m => RetryPolicyM m
retryPolicy = exponentialBackoff 50000 <> limitRetries 5
