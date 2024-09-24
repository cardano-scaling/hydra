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
  except,
  liftMaybe,
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
  | NotEnoughBlockConfirmations Blockfrost.BlockHash
  | MissingBlockNo Blockfrost.BlockHash
  | MissingNextBlockHash Blockfrost.BlockHash
  deriving (Show, Exception)

runBlockfrostM ::
  Blockfrost.Project ->
  BlockfrostClientT IO a ->
  ExceptT APIBlockfrostError IO a
runBlockfrostM prj action = do
  result <- liftIO $ runBlockfrost prj action
  case result of
    Left err -> throwError (BlockfrostError $ show err)
    Right val -> pure val

blockfrostClient ::
  Tracer IO ChainObserverLog ->
  FilePath ->
  Maybe Text ->
  NodeClient IO
blockfrostClient tracer projectPath startFromBlockHash = do
  NodeClient
    { follow = \_ _ observerHandler -> do
        prj <- Blockfrost.projectFromFile projectPath

        Blockfrost.Genesis
          { _genesisActiveSlotsCoefficient
          , _genesisSlotLength
          , _genesisNetworkMagic
          } <-
          either (error . show) id
            <$> runExceptT (runBlockfrostM prj Blockfrost.getLedgerGenesis)

        let networkId = fromNetworkMagic _genesisNetworkMagic
        traceWith tracer ConnectingToExternalNode{networkId}

        block <-
          case startFromBlockHash of
            Just bh -> do
              either (error . show) id
                <$> runExceptT
                  ( runBlockfrostM prj $
                      Blockfrost.getBlock (Right $ Blockfrost.BlockHash bh)
                  )
            Nothing -> do
              either (error . show) id
                <$> runExceptT (runBlockfrostM prj Blockfrost.getLatestBlock)

        let chainPoint = toChainPoint block
        traceWith tracer StartObservingFrom{chainPoint}

        let blockTime = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

        void $
          retrying retryPolicy shouldRetry $ \_ ->
            either (error . show) id
              <$> runExceptT
                ( do
                    threadDelay blockTime
                    loop tracer prj block networkId blockTime observerHandler mempty
                )
    }

loop ::
  Tracer IO ChainObserverLog ->
  Blockfrost.Project ->
  Blockfrost.Block ->
  NetworkId ->
  DiffTime ->
  ObserverHandler IO ->
  UTxO ->
  ExceptT APIBlockfrostError IO a
loop tracer prj block networkId blockTime observerHandler utxo = do
  let Blockfrost.Block
        { _blockHash
        , _blockConfirmations
        , _blockNextBlock
        , _blockHeight
        } = block

  -- [1] Check if block within the safe zone to be processes.
  when (_blockConfirmations < 50) $
    throwError (NotEnoughBlockConfirmations _blockHash)

  -- [2] Search block transactions.
  txHashes <- runBlockfrostM prj . Blockfrost.allPages $ \p ->
    Blockfrost.getBlockTxs' (Right _blockHash) p Blockfrost.def

  -- [3] Collect CBOR representations
  cborTxs <- traverse (runBlockfrostM prj . Blockfrost.getTxCBOR) txHashes

  -- [4] Convert CBOR to Cardano API Tx.
  receivedTxs <- except $ mapM toTx (concat cborTxs)
  let receivedTxIds = txId <$> receivedTxs
  let point = toChainPoint block
  lift $ traceWith tracer RollForward{point, receivedTxIds}

  -- [5] Collect head observations.
  let (adjustedUTxO, observations) = observeAll networkId utxo receivedTxs
  let onChainTxs = mapMaybe convertObservation observations
  lift $ forM_ onChainTxs (traceWith tracer . logOnChainTx)

  blockNo <- liftMaybe (MissingBlockNo _blockHash) (fromInteger <$> _blockHeight)
  let observationsAt = HeadObservation point blockNo <$> onChainTxs

  -- [6] Call observer handler.
  lift . observerHandler $
    if null observationsAt
      then [Tick point blockNo]
      else observationsAt

  -- [7] Loop next.
  case _blockNextBlock of
    Just nextBlockHash -> do
      block' <- runBlockfrostM prj (Blockfrost.getBlock $ Right nextBlockHash)
      loop tracer prj block' networkId blockTime observerHandler adjustedUTxO
    Nothing ->
      throwError (MissingNextBlockHash _blockHash)

-- * Helpers

retryPolicy :: MonadIO m => RetryPolicyM m
retryPolicy = exponentialBackoff 50000 <> limitRetries 5

isRetryable :: APIBlockfrostError -> Bool
isRetryable (BlockfrostError _) = True
isRetryable (DecodeError _) = False
isRetryable (NotEnoughBlockConfirmations _) = True
isRetryable (MissingBlockNo _) = True
isRetryable (MissingNextBlockHash _) = True

shouldRetry :: RetryStatus -> APIBlockfrostError -> IO Bool
shouldRetry _ err = pure $ isRetryable err

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

toTx :: Blockfrost.TransactionCBOR -> Either APIBlockfrostError Tx
toTx (Blockfrost.TransactionCBOR txCbor) =
  case decodeBase16 txCbor of
    Left decodeErr -> throwError . DecodeError $ "Bad Base16 Tx CBOR: " <> decodeErr
    Right bytes ->
      case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
        Left deserializeErr -> throwError . DecodeError $ "Bad Tx CBOR: " <> show deserializeErr
        Right tx -> pure tx
