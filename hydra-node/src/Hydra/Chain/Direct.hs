{-# LANGUAGE DuplicateRecordFields #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions.
module Hydra.Chain.Direct (
  NetworkMagic (NetworkMagic),
  module Hydra.Chain.Direct,
) where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Slot (EpochInfo, SlotNo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Control.Concurrent.Class.MonadSTM (
  newEmptyTMVar,
  newTQueueIO,
  newTVarIO,
  putTMVar,
  readTQueue,
  readTVarIO,
  takeTMVar,
  writeTQueue,
  writeTVar,
 )
import Control.Exception (IOException)
import Control.Monad.Trans.Except (runExcept)
import Control.Retry (constantDelay, retrying)
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as T
import Hydra.Cardano.Api (
  BlockHeader (..),
  BlockInMode (..),
  CardanoEra (..),
  ChainPoint (..),
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  EraHistory (EraHistory),
  Hash,
  IsShelleyBasedEra (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  SerialiseAsCBOR (..),
  SlotNo (..),
  Tx,
  TxInMode (..),
  TxValidationErrorInCardanoMode,
  UTxO,
  chainTipToChainPoint,
  connectToLocalNode,
  getBlockHeader,
  getBlockTxs,
  getTxBody,
  getTxId,
  proxyToAsType,
  serialiseToRawBytes,
  toLedgerUTxO,
 )
import Hydra.Chain (
  ChainComponent,
  ChainStateHistory,
  PostTxError (FailedToPostTx, failureReason),
  currentState,
 )
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
 )
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler,
  DirectChainLog (..),
  chainSyncHandler,
  mkChain,
  newLocalChainState,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainStateAt (..),
 )
import Hydra.Chain.Direct.Wallet (
  TinyWallet (..),
  WalletInfoOnChain (..),
  newTinyWallet,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node (BackendOps (..))
import Hydra.Node.Util (readKeyPair)
import Hydra.Options (CardanoChainConfig (..), ChainBackend (..))
import Hydra.Tx (Party)
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (
  LocalTxClientStIdle (..),
  LocalTxSubmissionClient (..),
  SubmitResult (..),
 )
import Text.Printf (printf)

-- | Build the 'ChainContext' from a 'ChainConfig' and additional information.
loadChainContext ::
  CardanoChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | The current running era we can use to query the node
  IO ChainContext
loadChainContext config party = do
  (vk, _) <- readKeyPair cardanoSigningKey
  scriptRegistry <- queryScriptRegistry chainBackend hydraScriptsTxId
  networkId <- queryNetworkId chainBackend
  pure $
    ChainContext
      { networkId
      , ownVerificationKey = vk
      , ownParty = party
      , scriptRegistry
      }
 where
  CardanoChainConfig
    { chainBackend
    , hydraScriptsTxId
    , cardanoSigningKey
    } = config

mkTinyWallet ::
  Tracer IO DirectChainLog ->
  CardanoChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet tracer config = do
  keyPair <- readKeyPair cardanoSigningKey
  networkId <- queryNetworkId chainBackend
  newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo querySomePParams
 where
  CardanoChainConfig{chainBackend, cardanoSigningKey} = config

  queryEpochInfo = toEpochInfo <$> queryEraHistory chainBackend QueryTip

  querySomePParams = queryProtocolParameters chainBackend QueryTip
  queryWalletInfo queryPoint address = do
    point <- case queryPoint of
      QueryAt point -> pure point
      QueryTip -> queryTip chainBackend
    walletUTxO <- Ledger.unUTxO . toLedgerUTxO <$> queryUTxO chainBackend [address]
    systemStart <- querySystemStart chainBackend QueryTip
    pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}

  toEpochInfo :: EraHistory -> EpochInfo (Either Text)
  toEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

withDirectChain ::
  Tracer IO DirectChainLog ->
  CardanoChainConfig ->
  ChainContext ->
  TinyWallet IO ->
  -- | Chain state loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withDirectChain tracer config ctx wallet chainStateHistory callback action = do
  -- Last known point on chain as loaded from persistence.
  let persistedPoint = recordedAt (currentState chainStateHistory)
  queue <- newTQueueIO
  -- Select a chain point from which to start synchronizing
  chainPoint <- maybe (queryTip chainBackend) pure $ do
    (max <$> startChainFrom <*> persistedPoint)
      <|> persistedPoint
      <|> startChainFrom

  let getTimeHandle = queryTimeHandle chainBackend
  localChainState <- newLocalChainState chainStateHistory
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
    race
      ( handle onIOException $
          case chainBackend of
            DirectBackend{networkId, nodeSocket} ->
              connectToLocalNode
                (connectInfo networkId nodeSocket)
                (clientProtocols chainPoint queue handler)
            BlockfrostBackend{projectPath} -> do
              prj <- Blockfrost.projectFromFile projectPath
              blockfrostChainFollow prj chainPoint handler wallet
      )
      (action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  CardanoChainConfig{chainBackend, startChainFrom} = config

  blockfrostChainFollow prj chainPoint handler wallet = do
    Blockfrost.Genesis{_genesisSlotLength, _genesisActiveSlotsCoefficient} <- Blockfrost.runBlockfrostM prj Blockfrost.getLedgerGenesis

    Blockfrost.Block{_blockHash = (Blockfrost.BlockHash genesisBlockHash)} <-
      Blockfrost.runBlockfrostM prj (Blockfrost.getBlock (Left 0))

    let blockTime = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

    let blockHash = fromChainPoint chainPoint genesisBlockHash

    void $
      retrying (retryPolicy blockTime) shouldRetry $ \_ -> do
        loop tracer prj blockTime handler wallet 1 blockHash
          `catch` \(ex :: APIBlockfrostError) ->
            pure $ Left ex

  connectInfo networkId nodeSocket =
    LocalNodeConnectInfo
      { -- REVIEW: This was 432000 before, but all usages in the
        -- cardano-node repository are using this value. This is only
        -- relevant for the Byron era.
        localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

  clientProtocols point queue handler =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ chainSyncClient handler wallet point
      , localTxSubmissionClient = Just $ txSubmissionClient tracer queue
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

  submitTx queue tx = do
    response <- atomically $ do
      response <- newEmptyTMVar
      writeTQueue queue (tx, response)
      return response
    atomically (takeTMVar response)
      >>= maybe (pure ()) throwIO

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      ConnectException
        { ioException
        }

  shouldRetry _ = \case
    Right{} -> pure False
    Left err -> pure $ isRetryable err

  retryPolicy blockTime = constantDelay (truncate blockTime * 1000 * 1000)

loop ::
  (MonadIO m, MonadThrow m, MonadSTM m) =>
  Tracer IO DirectChainLog ->
  Blockfrost.Project ->
  DiffTime ->
  ChainSyncHandler m ->
  TinyWallet m ->
  Integer ->
  Blockfrost.BlockHash ->
  m a
loop tracer prj blockTime handler wallet blockConfirmations current = do
  next <- rollForward tracer prj handler wallet blockConfirmations current
  loop tracer prj blockTime handler wallet blockConfirmations next

rollForward ::
  (MonadIO m, MonadThrow m) =>
  Tracer IO DirectChainLog ->
  Blockfrost.Project ->
  ChainSyncHandler m ->
  TinyWallet m ->
  Integer ->
  Blockfrost.BlockHash ->
  m Blockfrost.BlockHash
rollForward tracer prj handler wallet blockConfirmations blockHash = do
  Blockfrost.Block
    { _blockHash
    , _blockConfirmations
    , _blockNextBlock
    , _blockHeight
    , _blockSlot
    } <-
    Blockfrost.runBlockfrostM prj $ Blockfrost.getBlock (Right blockHash)

  -- Check if block within the safe zone to be processes
  when (_blockConfirmations < blockConfirmations) $
    throwIO (NotEnoughBlockConfirmations _blockHash)

  -- Search block transactions
  txHashesCBOR <- Blockfrost.runBlockfrostM prj . Blockfrost.allPages $ \p ->
    Blockfrost.getBlockTxsCBOR' (Right _blockHash) p Blockfrost.def

  -- Convert to cardano-api Tx
  receivedTxs <- mapM (toTx . (\(Blockfrost.TxHashCBOR (_txHash, cbor)) -> cbor)) txHashesCBOR

  -- Check if block contains a reference to its next
  nextBlockHash <- maybe (throwIO $ MissingNextBlockHash _blockHash) pure _blockNextBlock

  blockNo <- maybe (throwIO $ MissingBlockNo _blockHash) (pure . fromInteger) _blockHeight
  let Blockfrost.BlockHash blockHash' = _blockHash
  let blockHash'' = fromString $ T.unpack blockHash'
  blockSlot <- maybe (throwIO $ MissingBlockSlot _blockSlot) (pure . fromInteger . Blockfrost.unSlot) _blockSlot
  let header = BlockHeader (SlotNo blockSlot) blockHash'' blockNo
  update wallet header receivedTxs
  -- Observe Hydra transactions
  onRollForward handler header receivedTxs

  pure nextBlockHash

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

newtype ConnectException = ConnectException
  { ioException :: IOException
  }
  deriving stock (Show)

instance Exception ConnectException

-- | Thrown when the user-provided custom point of intersection is unknown to
-- the local node. This may happen if users shut down their node quickly after
-- starting them and hold on a not-so-stable point of the chain. When they turn
-- the node back on, that point may no longer exist on the network if a fork
-- with deeper roots has been adopted in the meantime.
newtype IntersectionNotFoundException = IntersectionNotFound
  { requestedPoint :: ChainPoint
  }
  deriving newtype (Show)

instance Exception IntersectionNotFoundException

data EraNotSupportedException
  = EraNotSupportedAnymore {otherEraName :: Text}
  | EraNotSupportedYet {otherEraName :: Text}
  deriving stock (Show)

instance Exception EraNotSupportedException where
  displayException = \case
    EraNotSupportedAnymore{otherEraName} ->
      printf
        "Received blocks of not anymore supported era (%s). \
        \Please wait for your cardano-node to be fully synchronized."
        otherEraName
    EraNotSupportedYet{otherEraName} ->
      printf
        "Received blocks of not yet supported era (%s). \
        \Please upgrade your hydra-node."
        otherEraName

-- | The block type used in the node-to-client protocols.
type BlockType = BlockInMode

chainSyncClient ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  ChainSyncHandler m ->
  TinyWallet m ->
  ChainPoint ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient handler wallet startingPoint =
  ChainSyncClient $
    pure $
      SendMsgFindIntersect
        [startingPoint]
        ( clientStIntersect
            (\_ -> throwIO (IntersectionNotFound startingPoint))
        )
 where
  clientStIntersect ::
    (ChainPoint -> m (ClientStIdle BlockType ChainPoint ChainTip m ())) ->
    ClientStIntersect BlockType ChainPoint ChainTip m ()
  clientStIntersect onIntersectionNotFound =
    ClientStIntersect
      { recvMsgIntersectFound = \_ _ ->
          ChainSyncClient (pure clientStIdle)
      , recvMsgIntersectNotFound =
          ChainSyncClient . onIntersectionNotFound . chainTipToChainPoint
      }

  clientStIdle :: ClientStIdle BlockType ChainPoint ChainTip m ()
  clientStIdle = SendMsgRequestNext (pure ()) clientStNext

  clientStNext :: ClientStNext BlockType ChainPoint ChainTip m ()
  clientStNext =
    ClientStNext
      { recvMsgRollForward = \blockInMode _tip -> ChainSyncClient $ do
          case blockInMode of
            BlockInMode ConwayEra block -> do
              let header = getBlockHeader block
              let txs = getBlockTxs block
              -- Update the tiny wallet
              update wallet header txs
              -- Observe Hydra transactions
              onRollForward handler header txs
              pure clientStIdle
            BlockInMode era@BabbageEra _ -> throwIO $ EraNotSupportedAnymore{otherEraName = show era}
            BlockInMode era@AlonzoEra _ -> throwIO $ EraNotSupportedAnymore{otherEraName = show era}
            BlockInMode era@AllegraEra _ -> throwIO $ EraNotSupportedAnymore{otherEraName = show era}
            BlockInMode era@MaryEra _ -> throwIO $ EraNotSupportedAnymore{otherEraName = show era}
            BlockInMode era@ShelleyEra _ -> throwIO $ EraNotSupportedAnymore{otherEraName = show era}
            BlockInMode era@ByronEra _ -> throwIO $ EraNotSupportedAnymore{otherEraName = show era}
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- Re-initialize the tiny wallet
          reset wallet
          -- Rollback main chain sync handler
          onRollBackward handler point
          pure clientStIdle
      }

txSubmissionClient ::
  forall m.
  (MonadSTM m, MonadDelay m) =>
  Tracer m DirectChainLog ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode m ()
txSubmissionClient tracer queue =
  LocalTxSubmissionClient clientStIdle
 where
  clientStIdle :: m (LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode m ())
  clientStIdle = do
    (tx, response) <- atomically $ readTQueue queue
    let txId = getTxId $ getTxBody tx
    traceWith tracer PostingTx{txId}
    pure $
      SendMsgSubmitTx
        (TxInMode shelleyBasedEra tx)
        ( \case
            SubmitSuccess -> do
              traceWith tracer PostedTx{txId}
              atomically (putTMVar response Nothing)
              clientStIdle
            SubmitFail err -> do
              -- XXX: Very complicated / opaque show instance and no unpacking
              -- possible because of missing data constructors from cardano-api
              let postTxError = FailedToPostTx{failureReason = show err}
              traceWith tracer PostingFailed{tx, postTxError}
              -- NOTE: Delay callback in case our transaction got invalidated
              -- because of a transaction seen in a block. This gives the
              -- observing side of the chain layer time to process the
              -- transaction and business logic might even ignore this error.
              threadDelay 1
              atomically (putTMVar response (Just postTxError))
              clientStIdle
        )
