module Hydra.Chain.Blockfrost where

import Hydra.Prelude

import Blockfrost.Client qualified as BlockfrostAPI
import Control.Concurrent.Class.MonadSTM (putTMVar, readTQueue, readTVarIO, takeTMVar, writeTQueue, writeTVar)
import Control.Exception (IOException)
import Control.Monad.Catch (Handler (Handler))
import Control.Monad.Catch qualified as Catch
import Control.Retry (RetryPolicyM, RetryStatus (..), constantDelay, recovering, retrying)
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as T
import Hydra.Cardano.Api (
  BlockHeader (..),
  ChainPoint (..),
  Hash,
  SlotNo (..),
  Tx,
  deserialiseFromCBOR,
  getTxBody,
  getTxId,
  proxyToAsType,
  serialiseToRawBytes,
 )
import Hydra.Chain (ChainComponent, ChainStateHistory, PostTxError (..), prefixOf)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.Blockfrost.Client (APIBlockfrostError (..), blockfrostRetryPolicy, isRetryable)
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Chain.Direct.Handlers (
  CardanoChainLog (..),
  ChainSyncHandler (..),
  chainSyncHandler,
  mkChain,
  newLocalChainState,
 )
import Hydra.Chain.Direct.State (ChainContext)
import Hydra.Chain.Direct.TimeHandle (newTimeHandleCache, queryTimeHandle)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (BlockfrostOptions (..), CardanoChainConfig (..))

newtype BlockfrostBackend a = BlockfrostBackend (ReaderT BlockfrostEnv IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

data BlockfrostEnv = BlockfrostEnv
  { project :: Blockfrost.Project
  , genesisVar :: TVar IO (Maybe Blockfrost.Genesis)
  }

newBlockfrostEnv :: BlockfrostOptions -> IO BlockfrostEnv
newBlockfrostEnv BlockfrostOptions{projectPath} =
  BlockfrostEnv <$> Blockfrost.projectFromFile projectPath <*> newLabelledTVarIO "blockfrost-genesis-cache" Nothing

runBlockfrostBackend :: BlockfrostOptions -> BlockfrostBackend a -> IO a
runBlockfrostBackend opts action = newBlockfrostEnv opts >>= (`runBlockfrostBackendWith` action)

runBlockfrostBackendWith :: BlockfrostEnv -> BlockfrostBackend a -> IO a
runBlockfrostBackendWith env (BlockfrostBackend m) = runReaderT m env

-- | Return the cached value or run the action once and store its result.
-- Concurrent callers may run the action more than once (last write wins),
-- which is safe for immutable values.
memoizeIO :: TVar IO (Maybe a) -> IO a -> IO a
memoizeIO var action = do
  memoized <- readTVarIO var
  case memoized of
    Nothing -> do
      result <- action
      atomically $ writeTVar var (Just result)
      pure result
    Just d -> pure d

cachedGenesis :: BlockfrostBackend Blockfrost.Genesis
cachedGenesis = BlockfrostBackend $ do
  BlockfrostEnv{project, genesisVar} <- ask
  liftIO $ memoizeIO genesisVar $ Blockfrost.runBlockfrostM project Blockfrost.queryGenesisParameters

instance ChainBackend BlockfrostBackend where
  queryGenesisParameters = Blockfrost.toCardanoGenesisParameters <$> cachedGenesis

  queryScriptRegistry txIds = do
    networkId <- queryNetworkId
    withProject $ \prj ->
      Blockfrost.runBlockfrostM prj $ Blockfrost.queryScriptRegistry networkId txIds

  queryNetworkId = Blockfrost.toCardanoNetworkId . Blockfrost._genesisNetworkMagic <$> cachedGenesis

  queryTip = withProject $ \prj ->
    Blockfrost.runBlockfrostM prj Blockfrost.queryTip

  queryUTxO addresses = do
    networkId <- queryNetworkId
    withProject $ \prj ->
      Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxO networkId addresses

  queryUTxOByTxIn txins = do
    networkId <- queryNetworkId
    withProject $ \prj ->
      Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxOByTxIn networkId txins

  queryEraHistory _ = withProject $ \prj ->
    Blockfrost.runBlockfrostM prj Blockfrost.queryEraHistory

  querySystemStart _ = Blockfrost.toCardanoSystemStart <$> cachedGenesis

  queryProtocolParameters _ = withProject $ \prj ->
    Blockfrost.runBlockfrostM prj Blockfrost.queryProtocolParameters

  queryStakePools _ = withProject $ \prj ->
    Blockfrost.runBlockfrostM prj Blockfrost.queryStakePools

  queryUTxOFor _ vk = do
    networkId <- queryNetworkId
    withProject $ \prj ->
      Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxOFor networkId vk

  submitTransaction tx = withProject $ \prj ->
    void $ Blockfrost.runBlockfrostM prj $ Blockfrost.submitTransaction tx

  awaitTransaction tx vk = do
    networkId <- queryNetworkId
    withProject $ \prj ->
      Blockfrost.runBlockfrostM prj $ Blockfrost.awaitTransaction networkId tx vk

  getBlockTime = do
    Blockfrost.Genesis{_genesisActiveSlotsCoefficient, _genesisSlotLength} <- cachedGenesis
    pure $ CardanoClient.computeBlockTime (fromInteger _genesisSlotLength) _genesisActiveSlotsCoefficient

withProject :: (Blockfrost.Project -> IO a) -> BlockfrostBackend a
withProject f = BlockfrostBackend $ do
  BlockfrostEnv{project} <- ask
  liftIO $ f project

withBlockfrostChain ::
  BlockfrostEnv ->
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  ChainContext ->
  TinyWallet IO ->
  -- | Chain state loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withBlockfrostChain env tracer config ctx wallet chainStateHistory callback action = do
  -- Known points on chain as loaded from persistence.
  let persistedPoints = prefixOf chainStateHistory

  -- Select a prefix chain from which to start synchronizing
  let startFromPrefix =
        -- Only use start chain from if its more recent than persisted points.
        case startChainFrom of
          Just sc
            | sc > head persistedPoints -> sc :| []
            | otherwise -> persistedPoints -- TODO: should warn the user about this
          _ -> persistedPoints

  -- Use the tip if we would otherwise start at the genesis (it can't be a good choice).
  prefix <-
    case head startFromPrefix of
      ChainPointAtGenesis -> runBlockfrostBackendWith env queryTip <&> (:| [])
      _ -> pure startFromPrefix

  let getTimeHandle = runBlockfrostBackendWith env queryTimeHandle
  -- The chain-sync path only converts slots and never needs the chain tip, so
  -- it uses cached time conversions instead of 3 API requests per block.
  cachedTimeHandle <-
    newTimeHandleCache
      (runBlockfrostBackendWith env (querySystemStart CardanoClient.QueryTip))
      (runBlockfrostBackendWith env (queryEraHistory CardanoClient.QueryTip))
  localChainState <- newLocalChainState chainStateHistory
  queue <- newLabelledTQueueIO "blockfrost-chain-queue"
  let chainHandle =
        mkChain
          tracer
          getTimeHandle
          wallet
          ctx
          localChainState
          (submitTx queue)

  let handler = chainSyncHandler tracer callback cachedTimeHandle ctx localChainState
  let getGenesis = liftIO (runBlockfrostBackendWith env cachedGenesis)
  res <-
    raceLabelled
      ( "blockfrost-chain-connection"
      , handle onIOException $ do
          blockfrostChain tracer queue project getGenesis prefix handler wallet
      )
      ("blockfrost-chain-handle", action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  BlockfrostEnv{project} = env
  CardanoChainConfig{startChainFrom} = config

  submitTx :: TQueue IO (Tx, TMVar IO (Maybe (PostTxError Tx))) -> Tx -> IO ()
  submitTx queue tx = do
    response <- atomically $ do
      response <- newLabelledEmptyTMVar "blockfrost-chain-submit-tx-response"
      writeTQueue queue (tx, response)
      return response
    atomically (takeTMVar response)
      >>= maybe (pure ()) throwIO

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      BlockfrostConnectException
        { ioException
        }

newtype BlockfrostConnectException = BlockfrostConnectException
  { ioException :: IOException
  }
  deriving stock (Show)

instance Exception BlockfrostConnectException

blockfrostChain ::
  (MonadIO m, MonadFail m, MonadCatch m, MonadAsync m, MonadDelay m, MonadLabelledSTM m, Catch.MonadMask m) =>
  Tracer m CardanoChainLog ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  Blockfrost.Project ->
  m Blockfrost.Genesis ->
  NonEmpty ChainPoint ->
  ChainSyncHandler m ->
  TinyWallet m ->
  m ()
blockfrostChain tracer queue prj getGenesis prefix handler wallet = do
  forever $
    raceLabelled_
      ("blockfrost-chain-follow", blockfrostChainFollow tracer prj getGenesis prefix handler wallet)
      ("blockfrost-submission", blockfrostSubmissionClient tracer (submitViaBlockfrost prj) queue)

blockfrostChainFollow ::
  forall m.
  (MonadIO m, MonadFail m, MonadCatch m, MonadDelay m, MonadLabelledSTM m, Catch.MonadMask m) =>
  Tracer m CardanoChainLog ->
  Blockfrost.Project ->
  m Blockfrost.Genesis ->
  NonEmpty ChainPoint ->
  ChainSyncHandler m ->
  TinyWallet m ->
  m ()
blockfrostChainFollow tracer prj getGenesis prefix handler wallet = do
  -- Genesis query and start point resolution are wrapped in retry to survive
  -- transient HTTP errors (e.g. 403 rate limiting, connection resets).
  (blockTime, stateTVar) <-
    retryOnBlockfrostError tracer blockfrostRetryPolicy $ \_ -> do
      Blockfrost.Genesis{_genesisSlotLength, _genesisActiveSlotsCoefficient} <- getGenesis
      let blockTime :: Double = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient
      -- Start from the latest point and fall back to older ones (best effort)
      -- If none of them can be resolved, we fall back to the tip of the chain.
      blockHash <- resolvePrefixPoints (toList prefix)
      stateTVar <- newLabelledTVarIO "blockfrost-chain-state" blockHash
      pure (blockTime, stateTVar)

  void $
    retrying (retryPolicy blockTime) shouldRetry $ \_ -> do
      pollForNewBlocks blockTime stateTVar
        `catch` \(ex :: APIBlockfrostError) ->
          pure $ Left ex
 where
  shouldRetry :: x -> Either APIBlockfrostError a -> m Bool
  shouldRetry _ = \case
    Right{} -> pure False
    Left err -> pure $ isRetryable err

  retryPolicy :: Double -> RetryPolicyM m
  retryPolicy blockTime' = constantDelay (truncate blockTime' * 1000 * 1000)

  -- Process every already-confirmed successor of the last processed block,
  -- then sleep one block time only once we caught up to the tip. Blocks with
  -- zero confirmations are left for a later iteration: we only ever observe
  -- blocks that have at least one successor.
  pollForNewBlocks blockTime' stateTVar = do
    current <- readTVarIO stateTVar
    blocks <-
      Blockfrost.runBlockfrostM prj $
        BlockfrostAPI.getNextBlocks' (Right current) (BlockfrostAPI.paged maxBlockBatch 1)
    let confirmed = filter ((>= 1) . Blockfrost._blockConfirmations) blocks
    forM_ confirmed $ \block -> do
      processBlock tracer prj handler wallet block
      atomically $ writeTVar stateTVar (Blockfrost._blockHash block)
    when (length blocks < maxBlockBatch) $
      threadDelay (realToFrac blockTime')
    pollForNewBlocks blockTime' stateTVar

  resolvePrefixPoints :: [ChainPoint] -> m Blockfrost.BlockHash
  resolvePrefixPoints = \case
    [] -> resolveTip
    cp : cps -> do
      res <- try (resolveChainPoint cp)
      case res of
        Right bh -> pure bh
        Left (_ :: SomeException) -> resolvePrefixPoints cps

  resolveTip :: m Blockfrost.BlockHash
  resolveTip = do
    (ChainPoint _ headerHash) <- Blockfrost.runBlockfrostM prj Blockfrost.queryTip
    pure $ Blockfrost.BlockHash (decodeUtf8 . Base16.encode . serialiseToRawBytes $ headerHash)

  resolveChainPoint :: ChainPoint -> m Blockfrost.BlockHash
  resolveChainPoint = \case
    ChainPointAtGenesis -> do
      result <- liftIO $ Blockfrost.tryError $ Blockfrost.runBlockfrost prj (Blockfrost.getBlock (Left 0))
      case result of
        Right (Right (Blockfrost.Block{_blockHash = Blockfrost.BlockHash genesisBlockHash})) -> do
          pure $ Blockfrost.BlockHash genesisBlockHash
        _ -> do
          Blockfrost.Block{_blockHash = Blockfrost.BlockHash block1Hash} <-
            Blockfrost.runBlockfrostM prj (Blockfrost.getBlock (Left 1))
          pure $ Blockfrost.BlockHash block1Hash
    ChainPoint _ headerHash ->
      pure $ Blockfrost.BlockHash (decodeUtf8 . Base16.encode . serialiseToRawBytes $ headerHash)

processBlock ::
  (MonadIO m, MonadThrow m) =>
  Tracer m CardanoChainLog ->
  Blockfrost.Project ->
  ChainSyncHandler m ->
  TinyWallet m ->
  Blockfrost.Block ->
  m ()
processBlock tracer prj handler wallet block@Blockfrost.Block{_blockHash, _blockTxCount, _blockHeight, _blockSlot} = do
  -- A block's transactions are a separate paginated request; the header
  -- already tells us when there is nothing to fetch.
  receivedTxs <-
    if _blockTxCount == 0
      then pure []
      else do
        txHashesCBOR <-
          Blockfrost.runBlockfrostM prj . Blockfrost.allPages $ \p ->
            Blockfrost.getBlockTxsCBOR' (Right _blockHash) p Blockfrost.def
        mapM (toTx . (\(Blockfrost.TxHashCBOR (_txHash, cbor)) -> cbor)) txHashesCBOR
  let receivedTxIds = getTxId . getTxBody <$> receivedTxs
  let point = toChainPoint block
  traceWith tracer RolledForward{point, receivedTxIds}

  blockNo <- maybe (throwIO $ MissingBlockNo _blockHash) (pure . fromInteger) _blockHeight
  blockSlot <- maybe (throwIO $ MissingBlockSlot _blockSlot) (pure . fromInteger . Blockfrost.unSlot) _blockSlot
  let Blockfrost.BlockHash blockHashText = _blockHash
  let header = BlockHeader (SlotNo blockSlot) (fromString $ T.unpack blockHashText) blockNo
  update wallet header receivedTxs
  onRollForward handler header receivedTxs

blockfrostSubmissionClient ::
  forall m.
  MonadSTM m =>
  Tracer m CardanoChainLog ->
  -- | How to submit a transaction, yielding a rendered failure reason or the
  -- transaction hash. Must not throw.
  (Tx -> m (Either Text Blockfrost.TxHash)) ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  m ()
blockfrostSubmissionClient tracer submit queue = bfClient
 where
  bfClient = do
    (tx, response) <- atomically $ readTQueue queue
    let txId = getTxId $ getTxBody tx
    traceWith tracer PostingTx{txId}
    res <- submit tx
    case res of
      Left err -> do
        let postTxError = FailedToPostTx{failureReason = err, failingTx = tx}
        traceWith tracer PostingFailed{tx, postTxError}
        atomically (putTMVar response (Just postTxError))
      Right _ -> do
        traceWith tracer PostedTx{txId}
        atomically (putTMVar response Nothing)
    bfClient

-- | Submit a transaction via Blockfrost, rendering both transport and API
-- level failures into a reason.
submitViaBlockfrost :: MonadIO m => Blockfrost.Project -> Tx -> m (Either Text Blockfrost.TxHash)
submitViaBlockfrost prj tx =
  liftIO $
    (Right <$> Blockfrost.runBlockfrostM prj (Blockfrost.submitTransaction tx))
      `catch` (\(e :: APIBlockfrostError) -> pure . Left $ show e)
      `catch` (\(e :: IOException) -> pure . Left $ show e)

toChainPoint :: Blockfrost.Block -> ChainPoint
toChainPoint Blockfrost.Block{_blockSlot, _blockHash} =
  ChainPoint slotNo headerHash
 where
  slotNo :: SlotNo
  slotNo = maybe 0 (fromInteger . Blockfrost.unSlot) _blockSlot

  headerHash :: Hash BlockHeader
  headerHash = fromString . toString $ Blockfrost.unBlockHash _blockHash

-- | Maximum number of blocks fetched per poll iteration (the Blockfrost page
-- size limit).
maxBlockBatch :: Int
maxBlockBatch = 100

retryOnBlockfrostError ::
  (MonadIO m, Catch.MonadMask m) =>
  Tracer m CardanoChainLog ->
  RetryPolicyM m ->
  (RetryStatus -> m a) ->
  m a
retryOnBlockfrostError tracer policy =
  recovering
    policy
    [ \RetryStatus{rsCumulativeDelay} -> Handler $ \(ex :: APIBlockfrostError) -> do
        traceWith tracer $ BlockfrostTransientError{reason = show ex, retryDelay = rsCumulativeDelay}
        pure (isRetryable ex)
    ]

-- * Helpers

toTx :: MonadThrow m => Blockfrost.TransactionCBOR -> m Tx
toTx (Blockfrost.TransactionCBOR txCbor) =
  case decodeBase16 txCbor of
    Left decodeErr -> throwIO . DecodeError $ "Bad Base16 Tx CBOR: " <> decodeErr
    Right bytes ->
      case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
        Left deserializeErr -> throwIO . DecodeError $ "Bad Tx CBOR: " <> show deserializeErr
        Right tx -> pure tx
