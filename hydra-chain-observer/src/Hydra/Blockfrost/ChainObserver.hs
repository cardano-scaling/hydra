{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Blockfrost.ChainObserver where

import Hydra.Prelude

import Blockfrost.Client (
  BlockfrostClientT,
  runBlockfrost,
 )
import Blockfrost.Client qualified as Blockfrost
import Control.Retry (RetryPolicyM, RetryStatus, exponentialBackoff, limitRetries, retrying)
import Hydra.Cardano.Api (
  BlockHeader,
  ChainPoint (..),
  HasTypeProxy (..),
  Hash,
  NetworkId (..),
  NetworkMagic (..),
  SerialiseAsCBOR (..),
  SlotNo (..),
  Tx,
  UTxO,
  throwError,
 )
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.ChainObserver.NodeClient (
  ChainObservation (..),
  ChainObserverLog (..),
  NodeClient (..),
  ObserverHandler,
  logOnChainTx,
  observeAll,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Tx (IsTx (..))

data APIBlockfrostError
  = BlockfrostError Text
  | DecodeError Text
  deriving (Show, Exception)

runBlockfrostM ::
  Blockfrost.Project ->
  BlockfrostClientT IO a ->
  ExceptT APIBlockfrostError IO a
runBlockfrostM prj action = do
  result <- liftIO $ runBlockfrost prj action
  case result of
    Left err -> throwError (BlockfrostError $ show err)
    Right val -> return val

blockfrostClient ::
  Tracer IO ChainObserverLog ->
  NodeClient IO
blockfrostClient tracer = do
  NodeClient
    { follow = \_ startChainFrom observerHandler -> do
        -- reads token from BLOCKFROST_TOKEN_PATH
        -- environment variable. It expects token
        -- prefixed with Blockfrost environment name
        -- e.g.: testnet-someTokenHash
        prj <- Blockfrost.projectFromEnv

        Blockfrost.Block{_blockHash = (Blockfrost.BlockHash genesisBlockHash)} <-
          either (error . show) id
            <$> runExceptT (runBlockfrostM prj (Blockfrost.getBlock (Left 0)))

        Blockfrost.Genesis
          { _genesisActiveSlotsCoefficient
          , _genesisSlotLength
          , _genesisNetworkMagic
          } <-
          either (error . show) id
            <$> runExceptT (runBlockfrostM prj Blockfrost.getLedgerGenesis)

        let networkId = fromNetworkMagic _genesisNetworkMagic
        traceWith tracer ConnectingToExternalNode{networkId}

        chainPoint <-
          case startChainFrom of
            Just point -> pure point
            Nothing -> do
              latestBlock@Blockfrost.Block{_blockHash} <-
                either (error . show) id
                  <$> runExceptT (runBlockfrostM prj Blockfrost.getLatestBlock)
              pure $ toChainPoint latestBlock
        traceWith tracer StartObservingFrom{chainPoint}
        let blockHash = fromChainPoint chainPoint genesisBlockHash
        let blockTime = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

        void $
          retrying retryPolicy shouldRetry $ \_ ->
            either (error . show) id
              <$> runExceptT (loop tracer prj blockHash networkId blockTime observerHandler mempty)
    }

loop ::
  Tracer IO ChainObserverLog ->
  Blockfrost.Project ->
  Blockfrost.BlockHash ->
  NetworkId ->
  DiffTime ->
  ObserverHandler IO ->
  UTxO ->
  ExceptT APIBlockfrostError IO a
loop tracer prj blockHash networkId blockTime observerHandler utxo = do
  -- [1] Find block by hash.
  latestBlock@Blockfrost.Block
    { _blockHeight
    , _blockNextBlock
    , _blockConfirmations
    } <-
    runBlockfrostM prj (Blockfrost.getBlock $ Right blockHash)

  -- [2] Check if block within the safe zone to be processes.
  when (_blockConfirmations < 50) $ do
    -- XXX: wait some time & retry
    threadDelay blockTime
    loop tracer prj blockHash networkId blockTime observerHandler utxo

  -- [3] Search block transactions.
  txHashes <-
    runBlockfrostM prj $
      Blockfrost.allPages
        ( \p ->
            Blockfrost.getBlockTxs' (Right blockHash) p Blockfrost.def
        )

  -- [4] Collect CBOR representations
  txResults <- traverse (lift . getTxCBOR) txHashes
  cborTxs <- either (throwError . DecodeError) return (sequence txResults)

  -- [5] Convert CBOR to Cardano API Tx.
  let receivedTxs = toCardanoAPI cborTxs

  let receivedTxIds = txId <$> receivedTxs
  let point = toChainPoint latestBlock
  lift $ traceWith tracer RollForward{point, receivedTxIds}

  -- [6] Collect head observations.
  let (adjustedUTxO, observations) = observeAll networkId utxo receivedTxs
  let onChainTxs = mapMaybe convertObservation observations
  lift $ forM_ onChainTxs (traceWith tracer . logOnChainTx)
  -- FIXME! handle missing blockNo
  let blockNo = maybe 0 fromInteger _blockHeight
  let observationsAt = HeadObservation point blockNo <$> onChainTxs

  -- [7] Call observer handler.
  lift . observerHandler $
    if null observationsAt
      then [Tick point blockNo]
      else observationsAt

  -- [8] Loop next.
  case _blockNextBlock of
    Just nextBlockHash -> do
      Blockfrost.Block{_blockHash = _blockHash'} <-
        runBlockfrostM prj (Blockfrost.getBlock $ Right nextBlockHash)
      loop tracer prj _blockHash' networkId blockTime observerHandler adjustedUTxO
    Nothing -> do
      -- XXX: wait some time & retry
      threadDelay blockTime
      loop tracer prj blockHash networkId blockTime observerHandler utxo

-- FIXME: (runBlockfrost prj . Blockfrost.getTxCBOR)
getTxCBOR :: Blockfrost.TxHash -> IO (Either Text Text)
getTxCBOR = const $ pure $ Right "TxCbor"

toCardanoAPI :: [Text] -> [Tx]
toCardanoAPI txs =
  txs <&> \txCbor ->
    case decodeBase16 txCbor of
      Left decodeErr -> error $ "Bad Base16 Tx CBOR: " <> decodeErr
      Right bytes ->
        case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
          Left deserializeErr -> error $ "Bad Tx CBOR: " <> show deserializeErr
          Right tx -> tx

-- * Helpers

retryPolicy :: MonadIO m => RetryPolicyM m
retryPolicy = exponentialBackoff 50000 <> limitRetries 5

isRetryable :: APIBlockfrostError -> Bool
isRetryable (BlockfrostError _) = True
isRetryable (DecodeError _) = False

shouldRetry :: RetryStatus -> APIBlockfrostError -> IO Bool
shouldRetry _ err = return $ isRetryable err

toChainPoint :: Blockfrost.Block -> ChainPoint
toChainPoint Blockfrost.Block{_blockSlot, _blockHash} =
  ChainPoint slotNo headerHash
 where
  slotNo :: SlotNo
  slotNo = maybe 0 (fromInteger . Blockfrost.unSlot) _blockSlot

  headerHash :: Hash BlockHeader
  headerHash = fromString . toString $ Blockfrost.unBlockHash _blockHash

fromChainPoint :: ChainPoint -> Text -> Blockfrost.BlockHash
fromChainPoint chainPoint genesisBlockHash = case chainPoint of
  ChainPoint _ headerHash -> Blockfrost.BlockHash $ show headerHash
  ChainPointAtGenesis -> Blockfrost.BlockHash genesisBlockHash

fromNetworkMagic :: Integer -> NetworkId
fromNetworkMagic = \case
  0 -> Mainnet
  magicNbr -> Testnet (NetworkMagic (fromInteger magicNbr))
