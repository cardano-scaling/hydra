{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Blockfrost.ChainObserver where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTVarIO),
  writeTVar,
 )
import Control.Retry (RetryPolicyM, RetryStatus, constantDelay, retrying)
import Control.Tracer.JSON (Tracer, traceWith)
import Data.ByteString.Base16 qualified as Base16
import Hydra.Cardano.Api (
  ChainPoint (..),
  Hash,
  NetworkId (..),
  NetworkMagic (..),
  SlotNo (..),
  UTxO,
  serialiseToRawBytes,
 )
import Hydra.Cardano.Api.Prelude (
  BlockHeader (..),
 )
import Hydra.Chain.Blockfrost (toTx)
import Hydra.Chain.Blockfrost.Client (APIBlockfrostError (..), isRetryable, runBlockfrostM)
import Hydra.ChainObserver.NodeClient (
  ChainObservation (..),
  ChainObserverLog (..),
  NodeClient (..),
  ObserverHandler,
  logObservation,
  observeAll,
 )
import Hydra.ChainObserver.VersionRegistry (KnownVersion)
import Hydra.Tx (IsTx (..))
import Hydra.Tx.Observe (HeadObservation (..))

blockfrostClient ::
  Tracer IO ChainObserverLog ->
  [KnownVersion] ->
  FilePath ->
  Integer ->
  IO (NodeClient IO)
blockfrostClient tracer knownVersions projectPath blockConfirmations = do
  prj <- Blockfrost.projectFromFile projectPath
  Blockfrost.Genesis
    { _genesisActiveSlotsCoefficient
    , _genesisSlotLength
    , _genesisNetworkMagic
    } <-
    runBlockfrostM prj Blockfrost.getLedgerGenesis
  let networkId = fromNetworkMagic _genesisNetworkMagic
  pure
    NodeClient
      { networkId
      , follow = \startChainFrom observerHandler -> do
          Blockfrost.Block{_blockHash = (Blockfrost.BlockHash genesisBlockHash)} <-
            runBlockfrostM prj (Blockfrost.getBlock (Left 0))
          traceWith tracer ConnectingToExternalNode{networkId}

          chainPoint <-
            case startChainFrom of
              Just point -> pure point
              Nothing -> do
                toChainPoint <$> runBlockfrostM prj Blockfrost.getLatestBlock

          traceWith tracer StartObservingFrom{chainPoint}

          let blockTime = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

          let blockHash = fromChainPoint chainPoint genesisBlockHash

          stateTVar <- newLabelledTVarIO "blockfrost-client-state" (blockHash, mempty)
          either throwIO pure
            =<< retrying
              (retryPolicy blockTime)
              shouldRetry
              ( \_ -> do
                  loop tracer prj knownVersions networkId blockTime observerHandler blockConfirmations stateTVar
                    `catch` \(ex :: APIBlockfrostError) ->
                      pure $ Left ex
              )
      }
 where
  shouldRetry :: MonadIO m => RetryStatus -> Either APIBlockfrostError b -> m Bool
  shouldRetry _ = \case
    Right{} -> pure False
    Left err -> pure $ isRetryable err

  retryPolicy :: DiffTime -> RetryPolicyM IO
  retryPolicy blockTime = constantDelay (truncate blockTime * 1000 * 1000)

-- | Iterative process that follows the chain using a naive roll-forward approach,
-- keeping track of the latest known current block and UTxO view.
-- This process operates at full speed without waiting between calls,
-- favoring the catch-up process.
loop ::
  (MonadIO m, MonadThrow m, MonadSTM m) =>
  Tracer m ChainObserverLog ->
  Blockfrost.Project ->
  [KnownVersion] ->
  NetworkId ->
  DiffTime ->
  ObserverHandler m ->
  Integer ->
  TVar m (Blockfrost.BlockHash, UTxO) ->
  m a
loop tracer prj knownVersions networkId blockTime observerHandler blockConfirmations stateTVar = do
  current <- readTVarIO stateTVar
  next <- rollForward tracer prj knownVersions networkId observerHandler blockConfirmations current
  atomically $ writeTVar stateTVar next
  loop tracer prj knownVersions networkId blockTime observerHandler blockConfirmations stateTVar

-- | From the current block and UTxO view, we collect Hydra observations
-- and yield the next block and adjusted UTxO view.
rollForward ::
  (MonadIO m, MonadThrow m) =>
  Tracer m ChainObserverLog ->
  Blockfrost.Project ->
  [KnownVersion] ->
  NetworkId ->
  ObserverHandler m ->
  Integer ->
  (Blockfrost.BlockHash, UTxO) ->
  m (Blockfrost.BlockHash, UTxO)
rollForward tracer prj knownVersions networkId observerHandler blockConfirmations (blockHash, utxo) = do
  block@Blockfrost.Block
    { _blockHash
    , _blockConfirmations
    , _blockNextBlock
    , _blockHeight
    } <-
    runBlockfrostM prj $ Blockfrost.getBlock (Right blockHash)

  -- Check if block within the safe zone to be processes
  when (_blockConfirmations < blockConfirmations) $
    throwIO (NotEnoughBlockConfirmations _blockHash)

  -- Check if block contains a reference to its next
  nextBlockHash <- maybe (throwIO $ MissingNextBlockHash _blockHash) pure _blockNextBlock

  -- Search block transactions
  txHashesCBOR <- runBlockfrostM prj . Blockfrost.allPages $ \p ->
    Blockfrost.getBlockTxsCBOR' (Right _blockHash) p Blockfrost.def

  -- Convert to cardano-api Tx
  receivedTxs <- mapM (toTx . (\(Blockfrost.TxHashCBOR (_txHash, cbor)) -> cbor)) txHashesCBOR
  let receivedTxIds = txId <$> receivedTxs
  let point = toChainPoint block
  traceWith tracer RollForward{point, receivedTxIds}

  -- Collect head observations
  let (adjustedUTxO, observations) = observeAll knownVersions networkId utxo receivedTxs
  mapM_ (traceWith tracer) $ mapMaybe (logObservation . snd) observations

  blockNo <- maybe (throwIO $ MissingBlockNo _blockHash) (pure . fromInteger) _blockHeight
  let observationsAt = [(Just v, ChainObservation point blockNo obs) | (v, obs) <- observations]

  -- Call observer handler
  observerHandler $
    if null observationsAt
      then [(Nothing, ChainObservation point blockNo NoHeadTx)]
      else observationsAt

  -- Next
  pure (nextBlockHash, adjustedUTxO)

-- * Helpers

toChainPoint :: Blockfrost.Block -> ChainPoint
toChainPoint Blockfrost.Block{_blockSlot, _blockHash} =
  ChainPoint slotNo headerHash
 where
  slotNo :: SlotNo
  slotNo = maybe 0 (fromInteger . Blockfrost.unSlot) _blockSlot

  headerHash :: Hash BlockHeader
  headerHash = fromString . toString $ Blockfrost.unBlockHash _blockHash

fromNetworkMagic :: Integer -> NetworkId
fromNetworkMagic = \case
  0 -> Mainnet
  magicNbr -> Testnet (NetworkMagic (fromInteger magicNbr))

fromChainPoint :: ChainPoint -> Text -> Blockfrost.BlockHash
fromChainPoint chainPoint genesisBlockHash = case chainPoint of
  ChainPoint _ headerHash -> Blockfrost.BlockHash (decodeUtf8 . Base16.encode . serialiseToRawBytes $ headerHash)
  ChainPointAtGenesis -> Blockfrost.BlockHash genesisBlockHash
