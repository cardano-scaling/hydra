module Hydra.Chain.Blockfrost where

import "hydra-prelude" Hydra.Prelude

import "base" Control.Exception (IOException)
import "base16-bytestring" Data.ByteString.Base16 qualified as Base16
import "blockfrost-client" Blockfrost.Client qualified as BlockfrostAPI
import "hydra-cardano-api" Hydra.Cardano.Api (
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
import "hydra-node" Hydra.Chain (ChainComponent, ChainStateHistory, PostTxError (..), prefixOf)
import "hydra-node" Hydra.Chain.Backend (ChainBackend (..))
import "hydra-node" Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import "hydra-node" Hydra.Chain.Direct.Handlers (
  CardanoChainLog (..),
  ChainSyncHandler (..),
  chainSyncHandler,
  mkChain,
  newLocalChainState,
 )
import "hydra-node" Hydra.Chain.Direct.State (ChainContext)
import "hydra-node" Hydra.Chain.Direct.TimeHandle (queryTimeHandle)
import "hydra-node" Hydra.Chain.Direct.Wallet (TinyWallet (..))
import "hydra-node" Hydra.Logging (Tracer, traceWith)
import "hydra-node" Hydra.Options (BlockfrostOptions (..), CardanoChainConfig (..), ChainBackendOptions (..))
import "io-classes" Control.Concurrent.Class.MonadSTM (putTMVar, readTQueue, readTVarIO, takeTMVar, writeTQueue, writeTVar)
import "retry" Control.Retry (RetryPolicyM, constantDelay, retrying)
import "text" Data.Text qualified as T

newtype BlockfrostBackend = BlockfrostBackend {options :: BlockfrostOptions} deriving (Eq, Show)

instance ChainBackend BlockfrostBackend where
  queryGenesisParameters (BlockfrostBackend BlockfrostOptions{projectPath}) = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.toCardanoGenesisParameters <$> Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters

  queryScriptRegistry (BlockfrostBackend opts@BlockfrostOptions{projectPath}) txIds = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj $ Blockfrost.queryScriptRegistry opts txIds

  queryNetworkId (BlockfrostBackend BlockfrostOptions{projectPath}) = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    -- TODO: This calls to queryGenesisParameters again, but we only need the network magic
    Blockfrost.Genesis{_genesisNetworkMagic} <- Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
    pure $ Blockfrost.toCardanoNetworkId _genesisNetworkMagic

  queryTip (BlockfrostBackend BlockfrostOptions{projectPath}) = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.queryTip

  queryUTxO (BlockfrostBackend opts@BlockfrostOptions{projectPath}) addresses = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.Genesis
      { _genesisNetworkMagic
      , _genesisSystemStart
      } <-
      Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
    let networkId = Blockfrost.toCardanoNetworkId _genesisNetworkMagic
    Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxO opts networkId addresses

  queryUTxOByTxIn (BlockfrostBackend opts@BlockfrostOptions{projectPath}) txins = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.Genesis
      { _genesisNetworkMagic
      , _genesisSystemStart
      } <-
      Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
    let networkId = Blockfrost.toCardanoNetworkId _genesisNetworkMagic
    Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxOByTxIn opts networkId txins

  queryEraHistory (BlockfrostBackend BlockfrostOptions{projectPath}) _ = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.queryEraHistory

  querySystemStart (BlockfrostBackend BlockfrostOptions{projectPath}) _ = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.querySystemStart

  queryProtocolParameters (BlockfrostBackend BlockfrostOptions{projectPath}) _ = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.queryProtocolParameters

  queryStakePools (BlockfrostBackend BlockfrostOptions{projectPath}) _ = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.queryStakePools

  queryUTxOFor (BlockfrostBackend opts@BlockfrostOptions{projectPath}) _ vk = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxOFor opts vk

  submitTransaction (BlockfrostBackend BlockfrostOptions{projectPath}) tx = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    void $ Blockfrost.runBlockfrostM prj $ Blockfrost.submitTransaction tx

  awaitTransaction (BlockfrostBackend opts@BlockfrostOptions{projectPath}) tx vk = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj $ Blockfrost.awaitTransaction opts tx vk

  getOptions (BlockfrostBackend blockfrostOptions) = Blockfrost blockfrostOptions

  getBlockTime (BlockfrostBackend BlockfrostOptions{projectPath}) = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.Genesis{_genesisActiveSlotsCoefficient, _genesisSlotLength} <- Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
    pure $ fromInteger _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

withBlockfrostChain ::
  BlockfrostBackend ->
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  ChainContext ->
  TinyWallet IO ->
  -- | Chain state loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withBlockfrostChain backend tracer config ctx wallet chainStateHistory callback action = do
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
      ChainPointAtGenesis -> queryTip backend <&> (:| [])
      _ -> pure startFromPrefix

  let getTimeHandle = queryTimeHandle backend
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

  let handler = chainSyncHandler tracer callback getTimeHandle ctx localChainState
  res <-
    raceLabelled
      ( "blockfrost-chain-connection"
      , handle onIOException $ do
          prj <- Blockfrost.projectFromFile projectPath
          blockfrostChain tracer queue prj prefix handler wallet
      )
      ("blockfrost-chain-handle", action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  BlockfrostBackend{options = BlockfrostOptions{projectPath}} = backend
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
  (MonadIO m, MonadFail m, MonadCatch m, MonadAsync m, MonadDelay m, MonadLabelledSTM m) =>
  Tracer m CardanoChainLog ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  Blockfrost.Project ->
  NonEmpty ChainPoint ->
  ChainSyncHandler m ->
  TinyWallet m ->
  m ()
blockfrostChain tracer queue prj prefix handler wallet = do
  forever $
    raceLabelled_
      ("blockfrost-chain-follow", blockfrostChainFollow tracer prj prefix handler wallet)
      ("blockfrost-submission", blockfrostSubmissionClient prj tracer queue)

blockfrostChainFollow ::
  forall m.
  (MonadIO m, MonadFail m, MonadCatch m, MonadDelay m, MonadLabelledSTM m) =>
  Tracer m CardanoChainLog ->
  Blockfrost.Project ->
  NonEmpty ChainPoint ->
  ChainSyncHandler m ->
  TinyWallet m ->
  m ()
blockfrostChainFollow tracer prj prefix handler wallet = do
  Blockfrost.Genesis{_genesisSlotLength, _genesisActiveSlotsCoefficient} <-
    Blockfrost.runBlockfrostM prj Blockfrost.getLedgerGenesis

  let blockTime :: Double = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

  -- Start from the latest point and fall back to older ones (best effort)
  -- If none of them can be resolved, we fall back to the tip of the chain.
  blockHash <- resolvePrefixPoints (toList prefix)

  stateTVar <- newLabelledTVarIO "blockfrost-chain-state" blockHash

  void $ catchUpToLatest blockHash stateTVar

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

  catchUpToLatest currentHash stateTVar = do
    latestBlock <- Blockfrost.runBlockfrostM prj BlockfrostAPI.getLatestBlock
    let targetHash = BlockfrostAPI._blockHash latestBlock

    catchUpLoop currentHash targetHash stateTVar

  catchUpLoop currentHash targetHash stateTVar = do
    if currentHash == targetHash
      then do
        pure currentHash
      else do
        nextBlockHash <- rollForward tracer prj handler wallet 0 currentHash
        atomically $ writeTVar stateTVar nextBlockHash

        if nextBlockHash == targetHash
          then do
            pure nextBlockHash
          else catchUpLoop nextBlockHash targetHash stateTVar

  pollForNewBlocks blockTime' stateTVar = do
    threadDelay (realToFrac blockTime')
    current <- readTVarIO stateTVar
    nextBlockHash <-
      rollForward tracer prj handler wallet 1 current
        `catch` \case
          MissingNextBlockHash{} -> do
            pure current
          ex -> throwIO ex

    when (nextBlockHash /= current) $
      atomically $
        writeTVar stateTVar nextBlockHash

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

rollForward ::
  (MonadIO m, MonadThrow m) =>
  Tracer m CardanoChainLog ->
  Blockfrost.Project ->
  ChainSyncHandler m ->
  TinyWallet m ->
  Integer ->
  Blockfrost.BlockHash ->
  m Blockfrost.BlockHash
rollForward tracer prj handler wallet blockConfirmations blockHash = do
  block@Blockfrost.Block
    { _blockHash
    , _blockConfirmations
    , _blockNextBlock
    , _blockHeight
    , _blockSlot
    , _blockTime
    } <-
    Blockfrost.runBlockfrostM prj $ Blockfrost.getBlock (Right blockHash)

  -- Check if block within the safe zone to be processes
  when (_blockConfirmations < blockConfirmations) $
    throwIO (NotEnoughBlockConfirmations _blockHash)

  -- Search block transactions
  txHashesCBOR <- Blockfrost.runBlockfrostM prj . Blockfrost.allPages $ \p ->
    Blockfrost.getBlockTxsCBOR' (Right _blockHash) p Blockfrost.def

  -- Check if block contains a reference to its next
  nextBlockHash <- maybe (throwIO $ MissingNextBlockHash _blockHash) pure _blockNextBlock

  -- Convert to cardano-api Tx
  receivedTxs <- mapM (toTx . (\(Blockfrost.TxHashCBOR (_txHash, cbor)) -> cbor)) txHashesCBOR
  let receivedTxIds = getTxId . getTxBody <$> receivedTxs
  let point = toChainPoint block
  traceWith tracer RolledForward{point, receivedTxIds}

  blockNo <- maybe (throwIO $ MissingBlockNo _blockHash) (pure . fromInteger) _blockHeight
  let Blockfrost.BlockHash blockHash' = _blockHash
  let blockHash'' = fromString $ T.unpack blockHash'
  blockSlot <- maybe (throwIO $ MissingBlockSlot _blockSlot) (pure . fromInteger . Blockfrost.unSlot) _blockSlot
  let header = BlockHeader (SlotNo blockSlot) blockHash'' blockNo
  -- wallet update
  update wallet header receivedTxs

  onRollForward handler header receivedTxs

  pure nextBlockHash

blockfrostSubmissionClient ::
  forall m.
  (MonadIO m, MonadDelay m, MonadSTM m) =>
  Blockfrost.Project ->
  Tracer m CardanoChainLog ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  m ()
blockfrostSubmissionClient prj tracer queue = bfClient
 where
  bfClient = do
    (tx, response) <- atomically $ readTQueue queue
    let txId = getTxId $ getTxBody tx
    traceWith tracer PostingTx{txId}
    res <- liftIO $ Blockfrost.tryError $ Blockfrost.runBlockfrost prj $ Blockfrost.submitTransaction tx
    case res of
      Left err -> do
        let postTxError = FailedToPostTx{failureReason = show err, failingTx = tx}
        traceWith tracer PostingFailed{tx, postTxError}
        threadDelay 1
        atomically (putTMVar response (Just postTxError))
      Right _ -> do
        traceWith tracer PostedTx{txId}
        atomically (putTMVar response Nothing)
        bfClient

toChainPoint :: Blockfrost.Block -> ChainPoint
toChainPoint Blockfrost.Block{_blockSlot, _blockHash} =
  ChainPoint slotNo headerHash
 where
  slotNo :: SlotNo
  slotNo = maybe 0 (fromInteger . Blockfrost.unSlot) _blockSlot

  headerHash :: Hash BlockHeader
  headerHash = fromString . toString $ Blockfrost.unBlockHash _blockHash

-- * Helpers

data APIBlockfrostError
  = BlockfrostError Text
  | DecodeError Text
  | NotEnoughBlockConfirmations Blockfrost.BlockHash
  | MissingBlockNo Blockfrost.BlockHash
  | MissingBlockSlot (Maybe Blockfrost.Slot)
  | MissingNextBlockHash Blockfrost.BlockHash
  deriving (Show, Exception)

isRetryable :: APIBlockfrostError -> Bool
isRetryable (BlockfrostError _) = True
isRetryable (DecodeError _) = False
isRetryable (NotEnoughBlockConfirmations _) = True
isRetryable (MissingBlockNo _) = True
isRetryable (MissingBlockSlot _) = True
isRetryable (MissingNextBlockHash _) = True

toTx :: MonadThrow m => Blockfrost.TransactionCBOR -> m Tx
toTx (Blockfrost.TransactionCBOR txCbor) =
  case decodeBase16 txCbor of
    Left decodeErr -> throwIO . DecodeError $ "Bad Base16 Tx CBOR: " <> decodeErr
    Right bytes ->
      case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
        Left deserializeErr -> throwIO . DecodeError $ "Bad Tx CBOR: " <> show deserializeErr
        Right tx -> pure tx
