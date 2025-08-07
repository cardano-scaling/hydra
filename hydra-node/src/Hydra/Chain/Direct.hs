{-# LANGUAGE DuplicateRecordFields #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions.
module Hydra.Chain.Direct (
  NetworkMagic (NetworkMagic),
  module Hydra.Chain.Direct,
) where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  putTMVar,
  readTQueue,
  takeTMVar,
  writeTQueue,
 )
import Control.Exception (IOException)
import Hydra.Cardano.Api (
  BlockInMode (..),
  CardanoEra (..),
  ChainPoint (..),
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  GenesisParameters (..),
  IsShelleyBasedEra (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId,
  SocketPath,
  Tx,
  TxInMode (..),
  TxValidationErrorInCardanoMode,
  chainTipToChainPoint,
  connectToLocalNode,
  getBlockHeader,
  getBlockTxs,
  getTxBody,
  getTxId,
 )
import Hydra.Chain (
  ChainComponent,
  ChainStateHistory,
  PostTxError (..),
  currentState,
 )
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Chain.Direct.Handlers (
  CardanoChainLog (..),
  ChainSyncHandler,
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
import Hydra.Chain.Direct.TimeHandle (queryTimeHandle)
import Hydra.Chain.Direct.Wallet (
  TinyWallet (..),
 )
import Hydra.Chain.ScriptRegistry qualified as ScriptRegistry
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), DirectOptions (..))
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

newtype DirectBackend = DirectBackend {options :: DirectOptions} deriving (Eq, Show)

instance ChainBackend DirectBackend where
  queryGenesisParameters (DirectBackend DirectOptions{networkId, nodeSocket}) =
    liftIO $ CardanoClient.queryGenesisParameters networkId nodeSocket CardanoClient.QueryTip

  queryScriptRegistry = ScriptRegistry.queryScriptRegistry

  queryNetworkId (DirectBackend DirectOptions{networkId}) = pure networkId

  queryTip (DirectBackend DirectOptions{networkId, nodeSocket}) =
    liftIO $ CardanoClient.queryTip networkId nodeSocket

  queryUTxO (DirectBackend DirectOptions{networkId, nodeSocket}) addresses =
    liftIO $ CardanoClient.queryUTxO networkId nodeSocket CardanoClient.QueryTip addresses

  queryUTxOByTxIn (DirectBackend DirectOptions{networkId, nodeSocket}) txins =
    liftIO $ CardanoClient.queryUTxOByTxIn networkId nodeSocket CardanoClient.QueryTip txins

  queryEraHistory (DirectBackend DirectOptions{networkId, nodeSocket}) queryPoint =
    liftIO $ CardanoClient.queryEraHistory networkId nodeSocket queryPoint

  querySystemStart (DirectBackend DirectOptions{networkId, nodeSocket}) queryPoint =
    liftIO $ CardanoClient.querySystemStart networkId nodeSocket queryPoint

  queryProtocolParameters (DirectBackend DirectOptions{networkId, nodeSocket}) queryPoint =
    liftIO $ CardanoClient.queryProtocolParameters networkId nodeSocket queryPoint
  queryStakePools (DirectBackend DirectOptions{networkId, nodeSocket}) queryPoint =
    liftIO $ CardanoClient.queryStakePools networkId nodeSocket queryPoint

  queryUTxOFor (DirectBackend DirectOptions{networkId, nodeSocket}) queryPoint vk =
    liftIO $ CardanoClient.queryUTxOFor networkId nodeSocket queryPoint vk

  submitTransaction (DirectBackend DirectOptions{networkId, nodeSocket}) tx =
    liftIO $ CardanoClient.submitTransaction networkId nodeSocket tx

  awaitTransaction (DirectBackend DirectOptions{networkId, nodeSocket}) tx _ =
    liftIO $ CardanoClient.awaitTransaction networkId nodeSocket tx

  getOptions (DirectBackend directOptions) = Direct directOptions

  getBlockTime (DirectBackend DirectOptions{networkId, nodeSocket}) = do
    GenesisParameters{protocolParamActiveSlotsCoefficient, protocolParamSlotLength} <-
      liftIO $ CardanoClient.queryGenesisParameters networkId nodeSocket CardanoClient.QueryTip
    pure (protocolParamSlotLength / realToFrac protocolParamActiveSlotsCoefficient)

withDirectChain ::
  DirectBackend ->
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  ChainContext ->
  TinyWallet IO ->
  -- | Chain state loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withDirectChain backend tracer config ctx wallet chainStateHistory callback action = do
  -- Last known point on chain as loaded from persistence.
  let persistedPoint = recordedAt (currentState chainStateHistory)
  queue <- newLabelledTQueueIO "direct-chain-queue"
  -- Select a chain point from which to start synchronizing
  chainPoint <- maybe (queryTip backend) pure $ do
    (max <$> startChainFrom <*> persistedPoint)
      <|> persistedPoint
      <|> startChainFrom

  let getTimeHandle = queryTimeHandle backend
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
      ( handle onIOException $ do
          labelMyThread "direct-chain-connection"
          connectToLocalNode
            (connectInfo networkId nodeSocket)
            (clientProtocols chainPoint queue handler)
      )
      (action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  DirectBackend{options = DirectOptions{networkId, nodeSocket}} = backend
  CardanoChainConfig{startChainFrom} = config

  connectInfo networkId' nodeSocket' =
    LocalNodeConnectInfo
      { -- REVIEW: This was 432000 before, but all usages in the
        -- cardano-node repository are using this value. This is only
        -- relevant for the Byron era.
        localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = networkId'
      , localNodeSocketPath = nodeSocket'
      }

  clientProtocols point queue handler =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ chainSyncClient handler wallet point
      , localTxSubmissionClient = Just $ txSubmissionClient tracer queue
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

  submitTx :: TQueue IO (Tx, TMVar IO (Maybe (PostTxError Tx))) -> Tx -> IO ()
  submitTx queue tx = do
    response <- atomically $ do
      response <- newLabelledEmptyTMVar "direct-chain-submit-tx-response"
      writeTQueue queue (tx, response)
      return response
    atomically (takeTMVar response)
      >>= maybe (pure ()) throwIO

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      DirectConnectException
        { ioException
        , nodeSocket
        , networkId
        }

data DirectConnectException = DirectConnectException
  { ioException :: IOException
  , nodeSocket :: SocketPath
  , networkId :: NetworkId
  }
  deriving stock (Show)

instance Exception DirectConnectException

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
  Tracer m CardanoChainLog ->
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
              let postTxError = FailedToPostTx{failureReason = show err, failingTx = tx}
              traceWith tracer PostingFailed{tx, postTxError}
              -- NOTE: Delay callback in case our transaction got invalidated
              -- because of a transaction seen in a block. This gives the
              -- observing side of the chain layer time to process the
              -- transaction and business logic might even ignore this error.
              threadDelay 1
              atomically (putTMVar response (Just postTxError))
              clientStIdle
        )
