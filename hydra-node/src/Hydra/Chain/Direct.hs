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
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Control.Concurrent.Class.MonadSTM (
  newEmptyTMVar,
  newTQueueIO,
  putTMVar,
  readTQueue,
  takeTMVar,
  writeTQueue,
 )
import Control.Exception (IOException)
import Control.Monad.Trans.Except (runExcept)
import Hydra.Cardano.Api (
  BlockInMode (..),
  CardanoEra (..),
  ChainPoint (..),
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  EraHistory (EraHistory),
  IsShelleyBasedEra (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  Tx,
  TxInMode (..),
  TxValidationErrorInCardanoMode,
  chainTipToChainPoint,
  connectToLocalNode,
  getBlockHeader,
  getBlockTxs,
  getTxBody,
  getTxId,
  toLedgerUTxO,
 )
import Hydra.Chain (
  ChainComponent,
  ChainStateHistory,
  PostTxError (FailedToPostTx, failureReason),
  currentState,
 )
import Hydra.Chain.Blockfrost qualified as Blockfrost
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
              forever $ Blockfrost.blockfrostChain tracer queue prj chainPoint handler wallet
      )
      (action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  CardanoChainConfig{chainBackend, startChainFrom} = config

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
