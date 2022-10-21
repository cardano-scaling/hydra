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
  EraHistory (EraHistory),
  LedgerEra,
  NetworkId,
  Tx,
  TxId,
  fromConsensusPointHF,
  shelleyBasedEra,
  toConsensusPointHF,
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
  QueryPoint (QueryAt),
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
  IdleState (..),
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

-- | Create the initial state of the direct chain layer. This will query for the
-- hydra scripts and initialize a 'ChainContext'.
-- XXX: It's a bit weird that the 'ChainContext' is in the 'ChainState'
initialChainState ::
  ChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | Transaction id at which to look for Hydra scripts.
  TxId ->
  IO (ChainStateType Tx)
initialChainState config party hydraScriptsTxId = do
  (vk, _) <- readKeyPair cardanoSigningKey
  otherCardanoKeys <- mapM readVerificationKey cardanoVerificationKeys
  scriptRegistry <- queryScriptRegistry networkId nodeSocket hydraScriptsTxId
  let ctx =
        ChainContext
          { networkId
          , peerVerificationKeys = otherCardanoKeys
          , ownVerificationKey = vk
          , ownParty = party
          , scriptRegistry
          }
  pure $
    Idle IdleState{ctx}
 where
  DirectChainConfig{networkId, nodeSocket, cardanoSigningKey, cardanoVerificationKeys} = config

withDirectChain ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  ChainComponent Tx IO a
withDirectChain tracer config callback action = do
  keyPair <- readKeyPair cardanoSigningKey
  queue <- newTQueueIO
  -- Select a chain point from which to start synchronizing
  chainPoint <- case startChainFrom of
    Nothing -> queryTip networkId nodeSocket
    Just point -> pure point
  wallet <- newTinyWallet (contramap Wallet tracer) networkId keyPair chainPoint queryUTxOEtc
  let chainHandle =
        mkChain
          tracer
          (queryTimeHandle networkId nodeSocket)
          wallet
          (submitTx queue)
  let getTimeHandle = queryTimeHandle networkId nodeSocket
  res <-
    race
      ( handle onIOException $ do
          let handler = chainSyncHandler tracer callback getTimeHandle
          let intersection = toConsensusPointHF <$> startChainFrom
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
  DirectChainConfig{networkId, nodeSocket, cardanoSigningKey, startChainFrom} = config

  queryUTxOEtc queryPoint address = do
    utxo <- Ledger.unUTxO . toLedgerUTxO <$> queryUTxO networkId nodeSocket queryPoint [address]
    pparams <- toLedgerPParams (shelleyBasedEra @Api.Era) <$> queryProtocolParameters networkId nodeSocket queryPoint
    systemStart <- querySystemStart networkId nodeSocket queryPoint
    epochInfo <- toEpochInfo <$> queryEraHistory networkId nodeSocket queryPoint
    pure (utxo, pparams, systemStart, epochInfo)

  toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
  toEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

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
  Maybe (Point Block) ->
  TQueue m (ValidatedTx LedgerEra, TMVar m (Maybe (PostTxError Tx))) ->
  ChainSyncHandler m ->
  TinyWallet m ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
ouroborosApplication tracer mpoint queue handler wallet nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClient handler wallet mpoint
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
  Maybe (Point Block) ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient handler wallet = \case
  Nothing ->
    ChainSyncClient (pure initStIdle)
  Just startingPoint ->
    ChainSyncClient $
      pure $ do
        SendMsgFindIntersect
          [startingPoint]
          ( clientStIntersect
              (\_ -> throwIO (IntersectionNotFound startingPoint))
          )
 where
  -- NOTE:
  -- We fast-forward the chain client to the current node's tip on start, and
  -- from there, follow the chain block by block as they arrive. This is why the
  -- chain client here has no state (and needs no persistence of previously seen
  -- headers). It fits with the narrative of heads being online all-the-time;
  -- history prior to when the client is created is thus not needed.
  --
  -- To acquire the chain tip, we leverage the fact that in any responses, the
  -- server will send its current tip, which can then find an intersection with.
  -- Hence the first `SendMsgRequestNext` which sole purpose is to get the
  -- server's tip. Note that the findIntersect can fail after that if the server
  -- switches to a different chain fork in between the two calls, in which case
  -- we'll start over the last step with the new tip.
  initStIdle :: ClientStIdle Block (Point Block) (Tip Block) m ()
  initStIdle = SendMsgRequestNext initStNext (pure initStNext)
   where
    initStNext :: ClientStNext Block (Point Block) (Tip Block) m ()
    initStNext =
      ClientStNext
        { recvMsgRollForward = \_ (getTipPoint -> tip) ->
            ChainSyncClient $ findIntersect tip
        , recvMsgRollBackward = \_ (getTipPoint -> tip) ->
            ChainSyncClient $ findIntersect tip
        }

    findIntersect :: Point Block -> m (ClientStIdle Block (Point Block) (Tip Block) m ())
    findIntersect tip =
      pure $ SendMsgFindIntersect [tip] (clientStIntersect findIntersect)

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
          reset wallet $ QueryAt (fromConsensusPointHF point)
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
