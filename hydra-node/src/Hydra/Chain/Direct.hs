{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions.
module Hydra.Chain.Direct (
  NetworkMagic (NetworkMagic),
  withIOManager,
  module Hydra.Chain.Direct,
) where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (UtxosFailure))
import Cardano.Ledger.Alonzo.Rules.Utxos (TagMismatchDescription (FailedUnexpectedly), UtxosPredicateFailure (ValidationTagMismatch))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (WrappedShelleyEraFailure))
import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxInfo (FailureDescription (PlutusFailure), debugPlutus, slotToPOSIXTime)
import Cardano.Ledger.Shelley.API (ApplyTxError (ApplyTxError))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (UtxowFailure))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (UtxoFailure))
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Control.Exception (IOException)
import Control.Monad.Class.MonadSTM (
  newEmptyTMVar,
  newTQueueIO,
  newTVarIO,
  putTMVar,
  readTQueue,
  takeTMVar,
  writeTQueue,
 )
import Control.Monad.Trans.Except (runExcept)
import Control.Tracer (nullTracer)
import Data.List ((\\))
import Hydra.Cardano.Api (
  CardanoMode,
  ChainPoint (..),
  EraHistory (EraHistory),
  LedgerEra,
  NetworkId,
  PaymentKey,
  ShelleyBasedEra (ShelleyBasedEraAlonzo),
  SigningKey,
  Tx,
  VerificationKey,
  toConsensusPointHF,
  toLedgerPParams,
 )
import Hydra.Chain (
  ChainComponent,
  PostTxError (..),
 )
import Hydra.Chain.CardanoClient (
  queryEraHistory,
  queryProtocolParameters,
  querySystemStart,
  queryTipSlotNo,
 )
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler,
  DirectChainLog (..),
  RecordedAt (..),
  SomeOnChainHeadStateAt (..),
  TimeHandle (..),
  chainSyncHandler,
  mkChain,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.State (
  SomeOnChainHeadState (..),
  idleOnChainHeadState,
 )
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  defaultCodecs,
  nullConnectTracers,
  versions,
 )
import Hydra.Chain.Direct.Wallet (
  TinyWallet (..),
  getTxId,
  newTinyWallet,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Party (Party)
import Ouroboros.Consensus.Cardano.Block (GenTx (..), HardForkApplyTxErr (ApplyTxErrAlonzo))
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import Ouroboros.Network.Block (Point (..), Tip (..), getTipPoint)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (
  MuxMode (..),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (..),
 )
import Ouroboros.Network.NodeToClient (
  IOManager,
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

withDirectChain ::
  -- | Tracer for logging
  Tracer IO DirectChainLog ->
  -- | Network identifer to which we expect to connect.
  NetworkId ->
  -- | A cross-platform abstraction for managing I/O operations on local sockets
  IOManager ->
  -- | Path to a domain socket used to connect to the server.
  FilePath ->
  -- | Key pair for the wallet.
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | Cardano keys of all Head participants (including our key pair).
  [VerificationKey PaymentKey] ->
  -- | Point at which to start following the chain.
  Maybe ChainPoint ->
  ChainComponent Tx IO a
withDirectChain tracer networkId iocp socketPath keyPair party cardanoKeys point callback action = do
  queue <- newTQueueIO
  wallet <- newTinyWallet (contramap Wallet tracer) networkId keyPair socketPath
  let (vk, _) = keyPair
  headState <-
    newTVarIO $
      SomeOnChainHeadStateAt
        { currentOnChainHeadState =
            SomeOnChainHeadState $
              idleOnChainHeadState networkId (cardanoKeys \\ [vk]) vk party
        , recordedAt = AtStart
        }
  res <-
    race
      ( do
          -- FIXME: There's currently a race-condition with the actual client
          -- which will only see transactions after it has established
          -- connection with the server's tip. So any transaction submitted
          -- before that tip will be missed.
          threadDelay 2
          action $
            mkChain
              tracer
              (queryTimeHandle networkId socketPath)
              cardanoKeys
              wallet
              headState
              (newTxPoster queue)
      )
      ( handle onIOException $ do
          let intersection = toConsensusPointHF <$> point
          let client = ouroborosApplication tracer intersection queue (chainSyncHandler tracer callback headState) wallet
          connectTo
            (localSnocket iocp)
            nullConnectTracers
            (versions networkId client)
            socketPath
      )
  case res of
    Left a -> pure a
    Right () -> error "'connectTo' cannot terminate but did?"
 where
  newTxPoster queue stm = do
    response <- atomically $ do
      response <- newEmptyTMVar
      vtx <- stm
      writeTQueue queue (vtx, response)
      return response
    atomically (takeTMVar response)
      >>= maybe (pure ()) throwIO

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      ConnectException
        { ioException
        , socketPath
        , networkId
        }

-- | Query ad-hoc epoch, system start and protocol parameters to determine
-- current point in time.
queryTimeHandle :: MonadThrow m => NetworkId -> FilePath -> IO (TimeHandle m)
queryTimeHandle networkId socketPath = do
  systemStart <- querySystemStart networkId socketPath
  eraHistory <- queryEraHistory networkId socketPath
  let epochInfo = toEpochInfo eraHistory
  pparams <- queryProtocolParameters networkId socketPath
  slotNo <- queryTipSlotNo networkId socketPath
  let toTime =
        slotToPOSIXTime
          (toLedgerPParams ShelleyBasedEraAlonzo pparams :: PParams LedgerEra)
          epochInfo
          systemStart
  pure $
    TimeHandle
      { currentPointInTime = (slotNo,) <$> toTime slotNo
      , adjustPointInTime = \n (slot, _) -> do
          let adjusted = slot + n
          time <- toTime adjusted
          pure (adjusted, time)
      }
 where
  toEpochInfo :: MonadThrow m => EraHistory CardanoMode -> EpochInfo m
  toEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (either throwIO pure . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

data ConnectException = ConnectException
  { ioException :: IOException
  , socketPath :: FilePath
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
  TQueue m (ValidatedTx Era, TMVar m (Maybe (PostTxError Tx))) ->
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
          reset wallet $ Just point
          -- Rollback main chain sync handler
          onRollBackward handler point
          pure clientStIdle
      }

txSubmissionClient ::
  forall m.
  (MonadSTM m) =>
  Tracer m DirectChainLog ->
  TQueue m (ValidatedTx Era, TMVar m (Maybe (PostTxError Tx))) ->
  LocalTxSubmissionClient (GenTx Block) (ApplyTxErr Block) m ()
txSubmissionClient tracer queue =
  LocalTxSubmissionClient clientStIdle
 where
  clientStIdle :: m (LocalTxClientStIdle (GenTx Block) (ApplyTxErr Block) m ())
  clientStIdle = do
    (tx, response) <- atomically $ readTQueue queue
    traceWith tracer (PostingTx (getTxId tx, tx))
    pure $
      SendMsgSubmitTx
        (GenTxAlonzo . mkShelleyTx $ tx)
        ( \case
            SubmitSuccess -> do
              traceWith tracer (PostedTx (getTxId tx))
              atomically (putTMVar response Nothing)
              clientStIdle
            SubmitFail reason -> do
              atomically (putTMVar response (Just $ onFail reason))
              clientStIdle
        )

  -- XXX(SN): patch-work error pretty printing on single plutus script failures
  onFail err =
    case err of
      ApplyTxErrAlonzo (ApplyTxError [failure]) ->
        fromMaybe failedToPostTx (unwrapPlutus failure)
      _ ->
        failedToPostTx
   where
    failedToPostTx = FailedToPostTx{failureReason = show err}

  unwrapPlutus :: LedgerPredicateFailure Era -> Maybe (PostTxError Tx)
  unwrapPlutus = \case
    UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch _ (FailedUnexpectedly [PlutusFailure plutusFailure debug]))))) ->
      Just $ PlutusValidationFailed{plutusFailure, plutusDebugInfo = show (debugPlutus PlutusV1 (decodeUtf8 debug))}
    _ ->
      Nothing
