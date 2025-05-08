module Hydra.Chain.Blockfrost where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.EpochInfo.API (EpochInfo, hoistEpochInfo)
import Control.Concurrent.Class.MonadSTM (putTMVar, readTQueue)
import Control.Retry (constantDelay, retrying)
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  BlockHeader (..),
  ChainPoint (..),
  EraHistory (..),
  Hash,
  PaymentCredential (PaymentCredentialByKey),
  SerialiseAsCBOR (serialiseToCBOR),
  SlotNo (..),
  StakeAddressReference (NoStakeAddress),
  SystemStart (..),
  Tx,
  deserialiseFromCBOR,
  getTxBody,
  getTxId,
  makeShelleyAddress,
  proxyToAsType,
  runExcept,
  serialiseToRawBytes,
  toLedgerUTxO,
  verificationKeyHash,
 )
import Hydra.Chain (PostTxError (..))
import Hydra.Chain.Blockfrost.Client (
  queryEraHistory,
  queryGenesisParameters,
  queryProtocolParameters,
  queryScriptRegistry,
  queryTip,
  queryUTxO,
  runBlockfrostM,
  toCardanoNetworkId,
 )
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  DirectChainLog (..),
 )
import Hydra.Chain.Direct.State (ChainContext (..))
import Hydra.Chain.Direct.Wallet (TinyWallet (..), WalletInfoOnChain (..), newTinyWallet)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node.Util (
  readKeyPair,
 )
import Hydra.Chain.Direct.Wallet (TinyWallet (..), WalletInfoOnChain (..), newTinyWallet)
import Hydra.Logging (Tracer)
import Hydra.Options (BlockfrostChainConfig (..))
import Hydra.Tx (Party)
import Ouroboros.Consensus.HardFork.History qualified as Consensus

-- | Build the 'ChainContext' from a 'BlockfrostChainConfig and additional information.
loadChainContext ::
  BlockfrostChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  IO ChainContext
loadChainContext config party = do
  (vk, _) <- readKeyPair cardanoSigningKey
  prj <- Blockfrost.projectFromFile projectPath
  runBlockfrostM prj $ do
    scriptRegistry <- queryScriptRegistry hydraScriptsTxId
    Blockfrost.Genesis
      { _genesisNetworkMagic
      } <-
      queryGenesisParameters
    let networkId = toCardanoNetworkId _genesisNetworkMagic
    pure $
      ChainContext
        { networkId
        , ownVerificationKey = vk
        , ownParty = party
        , scriptRegistry
        }
 where
  BlockfrostChainConfig
    { projectPath
    , hydraScriptsTxId
    , cardanoSigningKey
    } = config

mkTinyWallet ::
  Tracer IO DirectChainLog ->
  BlockfrostChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet tracer config = do
  keyPair@(vk, _) <- readKeyPair cardanoSigningKey
  prj <- Blockfrost.projectFromFile projectPath
  runBlockfrostM prj $ do
    Blockfrost.Genesis{_genesisSystemStart, _genesisNetworkMagic} <- queryGenesisParameters
    let networkId = toCardanoNetworkId _genesisNetworkMagic
    let address = makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vk) NoStakeAddress
    eraHistory <- queryEraHistory
    let queryEpochInfo = pure $ toEpochInfo eraHistory
    -- NOTE: we don't need to provide address here since it is derived from the
    -- keypair but we still want to keep the same wallet api.
    let queryWalletInfo queryPoint _address = runBlockfrostM prj $ do
          point <- case queryPoint of
            QueryAt point -> pure point
            QueryTip -> queryTip
          utxo <- queryUTxO networkId [address]
          let walletUTxO = Ledger.unUTxO $ toLedgerUTxO utxo
          let systemStart = SystemStart $ posixSecondsToUTCTime _genesisSystemStart
          pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}
    let querySomePParams = runBlockfrostM prj queryProtocolParameters
    liftIO $ newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo querySomePParams
 where
  BlockfrostChainConfig{projectPath, cardanoSigningKey} = config

  toEpochInfo :: EraHistory -> EpochInfo (Either Text)
  toEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

blockfrostChain ::
  (MonadIO m, MonadCatch m, MonadAsync m) =>
  Tracer IO DirectChainLog ->
  TQueue IO (Tx, TMVar IO (Maybe (PostTxError Tx))) ->
  Blockfrost.Project ->
  ChainPoint ->
  ChainSyncHandler m ->
  TinyWallet m ->
  m ()
blockfrostChain tracer queue prj chainPoint handler wallet =
  forever $
    race_
      (blockfrostChainFollow tracer prj chainPoint handler wallet)
      (liftIO $ Blockfrost.runBlockfrost prj $ blockfrostSubmissionClient tracer queue)

blockfrostChainFollow :: (MonadIO m, MonadCatch m, MonadSTM m) => Tracer IO DirectChainLog -> Blockfrost.Project -> ChainPoint -> ChainSyncHandler m -> TinyWallet m -> m ()
blockfrostChainFollow tracer prj chainPoint handler wallet = do
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
 where
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
  block@Blockfrost.Block
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

  let receivedTxIds = getTxId . getTxBody <$> receivedTxs
  let point = toChainPoint block

  liftIO $ traceWith tracer RolledForward{point, receivedTxIds}

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
  Tracer IO DirectChainLog ->
  TQueue IO (Tx, TMVar IO (Maybe (PostTxError Tx))) ->
  Blockfrost.BlockfrostClientT IO ()
blockfrostSubmissionClient tracer queue =
  bfClient
 where
  bfClient :: Blockfrost.BlockfrostClientT IO ()
  bfClient = do
    (tx, response) <- liftIO $ atomically $ readTQueue queue
    let txId = getTxId $ getTxBody tx
    liftIO $ traceWith tracer PostingTx{txId}
    res <- Blockfrost.tryError $ Blockfrost.submitTx $ Blockfrost.CBORString $ fromStrict $ serialiseToCBOR tx
    case res of
      Left err -> do
        let postTxError = FailedToPostTx{failureReason = show err}
        liftIO $ traceWith tracer PostingFailed{tx, postTxError}
      Right _ -> do
        liftIO $ traceWith tracer PostedTx{txId}
        liftIO $ atomically (putTMVar response Nothing)
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
