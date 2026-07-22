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
import Data.Text qualified as T
import Hydra.Cardano.Api (
  BlockInMode (..),
  CardanoEra (..),
  ChainPoint (..),
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
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
  shelleyBasedEra,
 )
import Hydra.Chain (
  ChainComponent,
  ChainStateHistory (..),
  PostTxError (..),
  prefixOf,
 )
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Chain.Direct.Handlers (
  CardanoChainLog (..),
  ChainSyncHandler,
  StartingDecision (..),
  chainSyncHandler,
  mkChain,
  newLocalChainState,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.State (ChainContext (..))
import Hydra.Chain.Direct.TimeHandle (queryTimeHandle)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.Chain.ScriptRegistry qualified as ScriptRegistry
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (CardanoChainConfig (..), DirectOptions (..))
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

newtype DirectBackend a = DirectBackend (ReaderT DirectOptions IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

runDirectBackend :: DirectOptions -> DirectBackend a -> IO a
runDirectBackend opts (DirectBackend m) = runReaderT m opts

instance ChainBackend DirectBackend where
  queryGenesisParameters = withNodeConn $ \ci ->
    CardanoClient.queryGenesisParameters ci CardanoClient.QueryTip

  queryScriptRegistry = ScriptRegistry.queryScriptRegistry

  queryNetworkId = DirectBackend $ do
    DirectOptions{networkId} <- ask
    pure networkId

  queryTip = withNodeConn CardanoClient.queryTip

  queryUTxO addresses = withNodeConn $ \ci ->
    CardanoClient.queryUTxO ci CardanoClient.QueryTip addresses

  queryUTxOByTxIn txins = withNodeConn $ \ci ->
    CardanoClient.queryUTxOByTxIn ci CardanoClient.QueryTip txins

  queryEraHistory queryPoint = withNodeConn $ \ci ->
    CardanoClient.queryEraHistory ci queryPoint

  querySystemStart queryPoint = withNodeConn $ \ci ->
    CardanoClient.querySystemStart ci queryPoint

  queryProtocolParameters queryPoint = withNodeConn $ \ci ->
    CardanoClient.queryProtocolParameters ci queryPoint

  queryStakePools queryPoint = withNodeConn $ \ci ->
    CardanoClient.queryStakePools ci queryPoint

  queryUTxOFor queryPoint vk = withNodeConn $ \ci ->
    CardanoClient.queryUTxOFor ci queryPoint vk

  submitTransaction tx = withNodeConn $ \ci ->
    CardanoClient.submitTransaction ci tx

  awaitTransaction tx _ = withNodeConn $ \ci ->
    CardanoClient.awaitTransaction ci tx

  getBlockTime = withNodeConn $ \ci ->
    CardanoClient.queryBlockTime ci CardanoClient.QueryTip

  getQueryDelay = pure 0

withNodeConn :: (LocalNodeConnectInfo -> IO a) -> DirectBackend a
withNodeConn f = DirectBackend $ do
  DirectOptions{networkId, nodeSocket} <- ask
  liftIO $ f (CardanoClient.localNodeConnectInfo networkId nodeSocket)

withDirectChain ::
  DirectOptions ->
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  ChainContext ->
  TinyWallet IO ->
  -- | Chain state loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withDirectChain opts tracer config ctx wallet chainStateHistory callback action = do
  -- Known points on chain as loaded from persistence.
  let persistedPoints = prefixOf chainStateHistory

  -- Select a prefix chain from which to start synchronizing
  let (startFromPrefix, startingDecision) =
        -- Only use start chain from if its more recent than persisted points.
        case startChainFrom of
          Just sc
            | sc > head persistedPoints -> (sc :| [], FromProvided sc)
            | otherwise -> (persistedPoints, FromPersisted (head persistedPoints) True)
          _ -> (persistedPoints, FromPersisted (head persistedPoints) False)

  -- Use the tip if we would otherwise start at the genesis (it can't be a good choice).
  (prefix, startingDecision') <-
    case head startFromPrefix of
      ChainPointAtGenesis -> do
        tip <- runDirectBackend opts queryTip
        pure (tip :| [], FromTip tip)
      _ -> pure (startFromPrefix, startingDecision)

  traceWith tracer $ StartingChainDecision startingDecision'
  let getTimeHandle = runDirectBackend opts queryTimeHandle
  localChainState <- newLocalChainState chainStateHistory
  queue <- newLabelledTQueueIO "direct-chain-queue"
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
      ( "direct-chain-connection"
      , handle onIOException $ do
          connectToLocalNode
            (connectInfo networkId nodeSocket)
            (clientProtocols prefix queue handler)
      )
      ("direct-chain-chain-handle", action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  DirectOptions{networkId, nodeSocket} = opts
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

  clientProtocols prefix queue handler =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ chainSyncClient handler wallet prefix
      , localTxSubmissionClient = Just $ txSubmissionClient tracer (runDirectBackend opts getBlockTime) queue
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

-- | Thrown when the user-provided custom points of intersection are unknown to
-- the local node. This may happen if users shut down their node quickly after
-- starting them and hold on not-so-stable points of the chain. When they turn
-- the node back on, those points may no longer exist on the network if a fork
-- with deeper roots has been adopted in the meantime.
newtype IntersectionNotFoundException = IntersectionNotFound
  { requestedPoints :: NonEmpty ChainPoint
  }
  deriving newtype (Show)

instance Exception IntersectionNotFoundException where
  displayException (IntersectionNotFound points) =
    printf
      "None of the requested intersection points %s were found on the local node.\n\
      \Requested points (newest first):\n\
      \%s\n\
      \This may happen if the points are too recent and the node has not yet \
      \synchronized that far, or if they are too old and have been pruned \
      \from the local node. Please try again with a different points."
      requestedPoints
   where
    requestedPoints :: String
    requestedPoints = T.unpack $ T.unlines (fmap show (toList points))

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
  NonEmpty ChainPoint ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient handler wallet prefix =
  ChainSyncClient $
    pure $
      SendMsgFindIntersect
        (toList prefix)
        ( clientStIntersect
            (\_ -> throwIO (IntersectionNotFound prefix))
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
            BlockInMode era@DijkstraEra _ -> throwIO $ EraNotSupportedYet{otherEraName = show era}
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
  -- | Action returning the chain's average block time (seconds), used to size
  -- the delay before reporting 'PostTxError' so the observing side has a chance
  -- to process a competing transaction.
  m NominalDiffTime ->
  TQueue m (Tx, TMVar m (Maybe (PostTxError Tx))) ->
  LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode m ()
txSubmissionClient tracer queryBlockTime queue =
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
              -- NOTE: Delay callback for one block time so the observing side
              -- has a chance to process a competing transaction; business
              -- logic might then ignore this error.
              blockTime <- queryBlockTime
              threadDelay (realToFrac blockTime)
              atomically (putTMVar response (Just postTxError))
              clientStIdle
        )
