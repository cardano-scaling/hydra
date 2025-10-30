module Hydra.Chain.Blockfrost where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (putTMVar, readTQueue, readTVarIO, takeTMVar, writeTQueue, writeTVar)
import Control.Exception (IOException)
import Control.Retry (RetryPolicyM, constantDelay, retrying)
import Data.ByteString.Base16 qualified as Base16
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GHC.IO.Exception (userError)
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
import Hydra.Chain (ChainComponent, ChainStateHistory, PostTxError (..), currentState)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.Direct.Handlers (
  CardanoChainLog (..),
  ChainSyncHandler (..),
  chainSyncHandler,
  mkChain,
  newLocalChainState,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainStateAt (..),
  close,
  fanout,
 )
import Hydra.Chain.Direct.TimeHandle (queryTimeHandle)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (BlockfrostOptions (..), CardanoChainConfig (..), ChainBackendOptions (..))
import Hydra.Tx (headSeedToTxIn)

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
  -- Last known point on chain as loaded from persistence.
  let persistedPoint = recordedAt (currentState chainStateHistory)
  queue <- newLabelledTQueueIO "blockfrost-chain-queue"
  -- Select a chain point from which to start synchronizing
  chainPoint <- maybe (queryTip backend) pure $ do
    (max <$> startChainFrom <*> persistedPoint)
      <|> persistedPoint
      <|> startChainFrom

  let getTimeHandle = queryTimeHandle backend
  localChainState <- newLocalChainState chainStateHistory
  let buildFanoutTx spendableUTxO headSeed fanoutTxDetails =
        let (utxo, utxosToCommit, utxosToDecommit, deadlineSlot) = fanoutTxDetails
            seedTxIn = fromJust $ headSeedToTxIn headSeed
         in fanout ctx spendableUTxO seedTxIn utxo utxosToCommit utxosToDecommit deadlineSlot
              >>= either (throwIO . userError . show) pure
  let buildCloseTx spendableUTxO headId (headParameters, openVersion, closingSnapshot, currentSlot, upperBound) = do
        let res = close ctx spendableUTxO headId headParameters openVersion closingSnapshot currentSlot upperBound
        either (throwIO . userError . show) pure =<< res
  let chainHandle =
        mkChain
          tracer
          getTimeHandle
          wallet
          ctx
          localChainState
          (submitTx queue)
          buildFanoutTx
          buildCloseTx

  let handler = chainSyncHandler tracer callback getTimeHandle ctx localChainState
  res <-
    raceLabelled
      ( "blockfrost-chain-connection"
      , handle onIOException $ do
          prj <- Blockfrost.projectFromFile projectPath
          blockfrostChain tracer queue prj chainPoint handler wallet
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
  (MonadIO m, MonadCatch m, MonadAsync m, MonadDelay m, MonadLabelledSTM m) =>
  Tracer m CardanoChainLog ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  Blockfrost.Project ->
  ChainPoint ->
  ChainSyncHandler m ->
  TinyWallet m ->
  m ()
blockfrostChain tracer queue prj chainPoint handler wallet = do
  forever $
    raceLabelled_
      ("blockfrost-chain-follow", blockfrostChainFollow tracer prj chainPoint handler wallet)
      ("blockfrost-submission", blockfrostSubmissionClient prj tracer queue)

blockfrostChainFollow ::
  forall m.
  (MonadIO m, MonadCatch m, MonadDelay m, MonadLabelledSTM m) =>
  Tracer m CardanoChainLog ->
  Blockfrost.Project ->
  ChainPoint ->
  ChainSyncHandler m ->
  TinyWallet m ->
  m ()
blockfrostChainFollow tracer prj chainPoint handler wallet = do
  Blockfrost.Genesis{_genesisSlotLength, _genesisActiveSlotsCoefficient} <- Blockfrost.runBlockfrostM prj Blockfrost.getLedgerGenesis

  Blockfrost.Block{_blockHash = (Blockfrost.BlockHash genesisBlockHash)} <-
    Blockfrost.runBlockfrostM prj (Blockfrost.getBlock (Left 0))

  let blockTime :: Double = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

  let blockHash = fromChainPoint chainPoint genesisBlockHash

  stateTVar <- newLabelledTVarIO "blockfrost-chain-state" blockHash

  void $
    retrying (retryPolicy blockTime) shouldRetry $ \_ -> do
      loop stateTVar
        `catch` \(ex :: APIBlockfrostError) ->
          pure $ Left ex
 where
  shouldRetry :: x -> Either APIBlockfrostError a -> m Bool
  shouldRetry _ = \case
    Right{} -> pure False
    Left err -> pure $ isRetryable err

  retryPolicy :: Double -> RetryPolicyM m
  retryPolicy blockTime' = constantDelay (truncate blockTime' * 1000 * 1000)

  loop stateTVar = do
    current <- readTVarIO stateTVar
    nextBlockHash <- rollForward tracer prj handler wallet 1 current
    threadDelay 1
    atomically $ writeTVar stateTVar nextBlockHash
    loop stateTVar

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

fromChainPoint :: ChainPoint -> Text -> Blockfrost.BlockHash
fromChainPoint chainPoint genesisBlockHash = case chainPoint of
  ChainPoint _ headerHash -> Blockfrost.BlockHash (decodeUtf8 . Base16.encode . serialiseToRawBytes $ headerHash)
  ChainPointAtGenesis -> Blockfrost.BlockHash genesisBlockHash
