{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions.
module Hydra.Chain.Direct (
  NetworkMagic (NetworkMagic),
  withIOManager,
  module Hydra.Chain.Direct,
) where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (UtxosFailure))
import Cardano.Ledger.Alonzo.Rules.Utxos (FailureDescription (PlutusFailure), TagMismatchDescription (FailedUnexpectedly), UtxosPredicateFailure (ValidationTagMismatch))
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (WrappedShelleyEraFailure))
import Cardano.Ledger.Alonzo.TxInfo (PlutusDebugInfo (..), debugPlutus)
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPred (FromAlonzoUtxoFail))
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPred (FromAlonzoUtxowFail))
import Cardano.Ledger.Babbage.Tx (ValidatedTx)
import Cardano.Ledger.Shelley.API (ApplyTxError (ApplyTxError))
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (UtxowFailure))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (UtxoFailure))
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Control.Exception (IOException)
import Control.Monad.Class.MonadSTM (
  newEmptyTMVar,
  newTQueueIO,
  putTMVar,
  readTQueue,
  takeTMVar,
  writeTQueue,
 )
import Control.Monad.Trans.Except (runExcept)
import Control.Tracer (nullTracer)
import Hydra.Cardano.Api (
  CardanoMode,
  ChainPoint,
  ConsensusMode (CardanoMode),
  EraHistory (EraHistory),
  LedgerEra,
  NetworkId,
  Tx,
  TxId,
  shelleyBasedEra,
  toConsensusPointInMode,
  toLedgerPParams,
  toLedgerUTxO,
 )
import qualified Hydra.Cardano.Api as Api
import Hydra.Chain (
  ChainComponent,
  ChainStateType,
  PostTxError (..),
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
  queryEraHistory,
  queryProtocolParameters,
  querySystemStart,
  queryTip,
  queryUTxO,
 )
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler,
  DirectChainLog (..),
  chainSyncHandler,
  mkChain,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.ScriptRegistry (queryScriptRegistry)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (Idle),
  ChainStateAt (..),
 )
import Hydra.Chain.Direct.TimeHandle (queryTimeHandle)
import Hydra.Chain.Direct.Util (
  Block,
  defaultCodecs,
  nullConnectTracers,
  readKeyPair,
  readVerificationKey,
  versions,
 )
import Hydra.Chain.Direct.Wallet (
  TinyWallet (..),
  WalletInfoOnChain (..),
  getTxId,
  newTinyWallet,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (ChainConfig (..))
import Hydra.Party (Party)
import Ouroboros.Consensus.Cardano.Block (
  GenTx (..),
  HardForkApplyTxErr (ApplyTxErrBabbage),
 )
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import Ouroboros.Network.Block (Point (..), Tip, getTipPoint)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (
  MuxMode (..),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (..),
 )
import Ouroboros.Network.NodeToClient (
  LocalAddress,
  NodeToClientProtocols (..),
  NodeToClientVersion,
  connectTo,
  localSnocket,
  localStateQueryPeerNull,
  localTxMonitorPeerNull,
  nodeToClientProtocols,
  withIOManager,
 )
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
  chainSyncClientPeer,
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (
  LocalTxClientStIdle (..),
  LocalTxSubmissionClient (..),
  SubmitResult (..),
  localTxSubmissionClientPeer,
 )
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

-- | Defines the starting state of the direct chain layer.
initialChainState :: ChainStateType Tx
initialChainState =
  ChainStateAt
    { chainState = Idle
    , recordedAt = Nothing
    }

-- | Build the 'ChainContext' from a 'ChainConfig' and additional information.
loadChainContext ::
  ChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | Transaction id at which to look for Hydra scripts.
  TxId ->
  IO ChainContext
loadChainContext config party hydraScriptsTxId = do
  (vk, _) <- readKeyPair cardanoSigningKey
  otherCardanoKeys <- mapM readVerificationKey cardanoVerificationKeys
  scriptRegistry <- queryScriptRegistry networkId nodeSocket hydraScriptsTxId
  pure $
    ChainContext
      { networkId
      , peerVerificationKeys = otherCardanoKeys
      , ownVerificationKey = vk
      , ownParty = party
      , scriptRegistry
      , contestationPeriod
      }
 where
  DirectChainConfig
    { networkId
    , nodeSocket
    , cardanoSigningKey
    , cardanoVerificationKeys
    , contestationPeriod
    } = config

mkTinyWallet ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet tracer config = do
  keyPair <- readKeyPair cardanoSigningKey
  newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo
 where
  DirectChainConfig{networkId, nodeSocket, cardanoSigningKey} = config

  queryEpochInfo = toEpochInfo <$> queryEraHistory networkId nodeSocket QueryTip

  queryWalletInfo queryPoint address = do
    point <- case queryPoint of
      QueryAt point -> pure point
      QueryTip -> queryTip networkId nodeSocket
    walletUTxO <- Ledger.unUTxO . toLedgerUTxO <$> queryUTxO networkId nodeSocket (QueryAt point) [address]
    pparams <- toLedgerPParams (shelleyBasedEra @Api.Era) <$> queryProtocolParameters networkId nodeSocket (QueryAt point)
    systemStart <- querySystemStart networkId nodeSocket (QueryAt point)
    epochInfo <- queryEpochInfo
    pure $ WalletInfoOnChain{walletUTxO, pparams, systemStart, epochInfo, tip = point}

  toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
  toEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

withDirectChain ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  ChainContext ->
  -- | Last known point on chain as loaded from persistence.
  Maybe ChainPoint ->
  TinyWallet IO ->
  [Party] ->
  ChainComponent Tx IO a
withDirectChain tracer config ctx persistedPoint wallet otherParties callback action = do
  queue <- newTQueueIO
  -- Select a chain point from which to start synchronizing
  chainPoint <- maybe (queryTip networkId nodeSocket) pure $ do
    (min <$> startChainFrom <*> persistedPoint)
      <|> persistedPoint
      <|> startChainFrom
  let getTimeHandle = queryTimeHandle networkId nodeSocket
  let chainHandle =
        mkChain
          tracer
          getTimeHandle
          wallet
          ctx
          (submitTx queue)
  res <-
    race
      ( handle onIOException $ do
          let handler = chainSyncHandler tracer callback getTimeHandle ctx otherParties
          let intersection = toConsensusPointInMode CardanoMode chainPoint
          let client = ouroborosApplication tracer intersection queue handler wallet
          withIOManager $ \iocp ->
            connectTo
              (localSnocket iocp)
              nullConnectTracers
              (versions networkId client)
              nodeSocket
      )
      (action chainHandle)
  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a
 where
  DirectChainConfig{networkId, nodeSocket, startChainFrom} = config

  submitTx queue vtx = do
    response <- atomically $ do
      response <- newEmptyTMVar
      writeTQueue queue (vtx, response)
      return response
    atomically (takeTMVar response)
      >>= maybe (pure ()) throwIO

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      ConnectException
        { ioException
        , nodeSocket
        , networkId
        }

data ConnectException = ConnectException
  { ioException :: IOException
  , nodeSocket :: FilePath
  , networkId :: NetworkId
  }
  deriving (Show)

instance Exception ConnectException

-- | Thrown when the user-provided custom point of intersection is unknown to
-- the local node. This may happen if users shut down their node quickly after
-- starting them and hold on a not-so-stable point of the chain. When they turn
-- the node back on, that point may no longer exist on the network if a fork
-- with deeper roots has been adopted in the meantime.
newtype IntersectionNotFoundException = IntersectionNotFound
  { requestedPoint :: Point Block
  }
  deriving (Show)

instance Exception IntersectionNotFoundException

ouroborosApplication ::
  (MonadST m, MonadTimer m, MonadThrow m) =>
  Tracer m DirectChainLog ->
  Point Block ->
  TQueue m (ValidatedTx LedgerEra, TMVar m (Maybe (PostTxError Tx))) ->
  ChainSyncHandler m ->
  TinyWallet m ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
ouroborosApplication tracer point queue handler wallet nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClient handler wallet point
                   in MuxPeer nullTracer cChainSyncCodec (chainSyncClientPeer peer)
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  let peer = txSubmissionClient tracer queue
                   in MuxPeer nullTracer cTxSubmissionCodec (localTxSubmissionClientPeer peer)
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  let peer = localStateQueryPeerNull
                   in MuxPeer nullTracer cStateQueryCodec peer
            , localTxMonitorProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxMonitorPeerNull
                   in MuxPeer nullTracer cTxMonitorCodec peer
            }
    )
    nodeToClientV
 where
  Codecs
    { cChainSyncCodec
    , cTxSubmissionCodec
    , cStateQueryCodec
    , cTxMonitorCodec
    } = defaultCodecs nodeToClientV

chainSyncClient ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  ChainSyncHandler m ->
  TinyWallet m ->
  Point Block ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
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
    (Point Block -> m (ClientStIdle Block (Point Block) (Tip Block) m ())) ->
    ClientStIntersect Block (Point Block) (Tip Block) m ()
  clientStIntersect onIntersectionNotFound =
    ClientStIntersect
      { recvMsgIntersectFound = \_ _ ->
          ChainSyncClient (pure clientStIdle)
      , recvMsgIntersectNotFound = \(getTipPoint -> tip) ->
          ChainSyncClient $ onIntersectionNotFound tip
      }

  clientStIdle :: ClientStIdle Block (Point Block) (Tip Block) m ()
  clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

  clientStNext :: ClientStNext Block (Point Block) (Tip Block) m ()
  clientStNext =
    ClientStNext
      { recvMsgRollForward = \block _tip -> ChainSyncClient $ do
          -- Update the tiny wallet
          update wallet block
          -- Observe Hydra transactions
          onRollForward handler block
          pure clientStIdle
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- Re-initialize the tiny wallet
          reset wallet
          -- Rollback main chain sync handler
          onRollBackward handler point
          pure clientStIdle
      }

txSubmissionClient ::
  forall m.
  (MonadSTM m) =>
  Tracer m DirectChainLog ->
  TQueue m (ValidatedTx LedgerEra, TMVar m (Maybe (PostTxError Tx))) ->
  LocalTxSubmissionClient (GenTx Block) (ApplyTxErr Block) m ()
txSubmissionClient tracer queue =
  LocalTxSubmissionClient clientStIdle
 where
  clientStIdle :: m (LocalTxClientStIdle (GenTx Block) (ApplyTxErr Block) m ())
  clientStIdle = do
    (tx, response) <- atomically $ readTQueue queue
    traceWith tracer (PostingTx (getTxId tx))
    pure $
      SendMsgSubmitTx
        (GenTxBabbage . mkShelleyTx $ tx)
        ( \case
            SubmitSuccess -> do
              traceWith tracer (PostedTx (getTxId tx))
              atomically (putTMVar response Nothing)
              clientStIdle
            SubmitFail err -> do
              let postTxError = onFail err
              traceWith tracer PostingFailed{tx, postTxError}
              atomically (putTMVar response (Just postTxError))
              clientStIdle
        )

  -- XXX(SN): patch-work error pretty printing on single plutus script failures
  onFail err =
    case err of
      ApplyTxErrBabbage (ApplyTxError [failure]) ->
        fromMaybe failedToPostTx (unwrapPlutus failure)
      _ ->
        failedToPostTx
   where
    failedToPostTx = FailedToPostTx{failureReason = show err}

  unwrapPlutus :: LedgerPredicateFailure LedgerEra -> Maybe (PostTxError Tx)
  unwrapPlutus = \case
    UtxowFailure (FromAlonzoUtxowFail (WrappedShelleyEraFailure (UtxoFailure (FromAlonzoUtxoFail (UtxosFailure (ValidationTagMismatch _ (FailedUnexpectedly (PlutusFailure plutusFailure debug :| _)))))))) ->
      let plutusDebugInfo =
            case debugPlutus (decodeUtf8 debug) of
              DebugSuccess budget -> "DebugSuccess: " <> show budget
              DebugCannotDecode err -> "DebugCannotDecode: " <> fromString err
              DebugInfo logs err _debug ->
                unlines
                  [ "DebugInfo:"
                  , "  Error: " <> show err
                  , "  Logs:"
                  ]
                  <> unlines (fmap ("    " <>) logs)
              DebugBadHex err -> "DebugBadHex: " <> fromString err
       in Just $ PlutusValidationFailed{plutusFailure, plutusDebugInfo}
    _ ->
      Nothing
