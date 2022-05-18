{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
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
import Cardano.Ledger.Alonzo.TxSeq (txSeqTxns)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (ApplyTxError (ApplyTxError), TxId)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (UtxowFailure))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (UtxoFailure))
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Exception (IOException)
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (
  newEmptyTMVar,
  newTQueueIO,
  newTVarIO,
  putTMVar,
  readTQueue,
  readTVarIO,
  retry,
  takeTMVar,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadTimer (timeout)
import Control.Monad.Trans.Except (runExcept)
import Control.Tracer (nullTracer)
import Data.Aeson (Value (String), object, (.=))
import Data.List ((\\))
import Data.Sequence.Strict (StrictSeq)
import Hydra.Cardano.Api (
  CardanoMode,
  ChainPoint (..),
  ChainTip (..),
  ConsensusModeIsMultiEra (CardanoModeIsMultiEra),
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
  EraHistory (EraHistory),
  EraInMode (AlonzoEraInCardanoMode),
  LedgerEra,
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  NetworkId,
  PaymentKey,
  ProtocolParameters,
  QueryInEra (QueryInShelleyBasedEra),
  QueryInMode (QueryEraHistory, QueryInEra, QuerySystemStart),
  QueryInShelleyBasedEra (QueryProtocolParameters),
  ShelleyBasedEra (ShelleyBasedEraAlonzo),
  SigningKey,
  SlotNo,
  Tx,
  VerificationKey,
  fromConsensusPointHF,
  fromLedgerTx,
  fromLedgerTxIn,
  fromLedgerUTxO,
  getLocalChainTip,
  queryNodeLocalState,
  toConsensusPointHF,
  toLedgerPParams,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.Chain (
  Chain (..),
  ChainComponent,
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct.State (
  SomeOnChainHeadState (..),
  TokHeadState (..),
  abort,
  close,
  collect,
  commit,
  contest,
  fanout,
  getKnownUTxO,
  idleOnChainHeadState,
  initialize,
  observeSomeTx,
  reifyState,
 )
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  SomePoint (..),
  defaultCodecs,
  nullConnectTracers,
  versions,
 )
import Hydra.Chain.Direct.Wallet (
  ErrCoverFee (..),
  TinyWallet (..),
  TinyWalletLog,
  getFuelUTxO,
  getTxId,
  withTinyWallet,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Party (Party)
import Ouroboros.Consensus.Cardano.Block (EraMismatch, GenTx (..), HardForkApplyTxErr (ApplyTxErrAlonzo), HardForkBlock (BlockAlonzo))
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import Ouroboros.Network.Block (Point (..), Tip (..), blockPoint, getTipPoint)
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
import Plutus.V1.Ledger.Api (POSIXTime)
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
  withTinyWallet (contramap Wallet tracer) networkId keyPair iocp socketPath $ \wallet -> do
    headState <-
      newTVarIO $
        SomeOnChainHeadStateAt
          { currentOnChainHeadState =
              SomeOnChainHeadState $
                idleOnChainHeadState
                  networkId
                  (cardanoKeys \\ [verificationKey wallet])
                  (verificationKey wallet)
                  party
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
              Chain
                { postTx = \tx -> do
                    traceWith tracer $ ToPost tx
                    timeHandle <- queryTimeHandle networkId socketPath
                    -- XXX(SN): 'finalizeTx' retries until a payment utxo is
                    -- found. See haddock for details
                    response <-
                      timeoutThrowAfter (NoPaymentInput @Tx) 10 $
                        -- FIXME (MB): 'cardanoKeys' should really not be here. They
                        -- are only required for the init transaction and ought to
                        -- come from the _client_ and be part of the init request
                        -- altogether. This goes in the direction of 'dynamic
                        -- heads' where participants aren't known upfront but
                        -- provided via the API. Ultimately, an init request from
                        -- a client would contain all the details needed to
                        -- establish connection to the other peers and to
                        -- bootstrap the init transaction.
                        -- For now, we bear with it and keep the static keys in
                        -- context.
                        fromPostChainTx timeHandle cardanoKeys wallet headState tx
                          >>= finalizeTx wallet headState . toLedgerTx
                          >>= \vtx -> do
                            response <- newEmptyTMVar
                            writeTQueue queue (vtx, response)
                            return response
                    atomically (takeTMVar response) >>= \case
                      Nothing -> pure ()
                      Just err -> throwIO err
                }
        )
        ( handle onIOException $ do
            let intersection = toConsensusPointHF <$> point
            let client = ouroborosApplication tracer intersection queue (chainSyncHandler tracer callback headState)
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
  timeoutThrowAfter ex s stm = do
    timeout s (atomically stm) >>= \case
      Nothing -> throwIO ex
      Just a -> pure a

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      ConnectException
        { ioException
        , socketPath
        , networkId
        }

data TimeHandle m = TimeHandle
  { currentSlot :: m SlotNo
  , -- | Convert some slot into an absolute point in time.
    -- It throws an exception when requesting a `SlotNo` that is
    -- more than the current epoch length (5 days at time of writing this comment).
    convertSlot :: MonadThrow m => SlotNo -> m POSIXTime
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
  pure $
    TimeHandle
      { currentSlot = pure slotNo
      , convertSlot =
          slotToPOSIXTime
            (toLedgerPParams ShelleyBasedEraAlonzo pparams :: PParams LedgerEra)
            epochInfo
            systemStart
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
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
ouroborosApplication tracer point queue handler nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClient handler point
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

-- | The on-chain head state is maintained in a mutually-recursive data-structure,
-- pointing to the previous chain state; This allows to easily rewind the state
-- to a point in the past when rolling backward due to a change of chain fork by
-- our local cardano-node.
data SomeOnChainHeadStateAt = SomeOnChainHeadStateAt
  { currentOnChainHeadState :: SomeOnChainHeadState
  , recordedAt :: RecordedAt
  }
  deriving (Eq, Show)

-- | Records when a state was seen on-chain. 'AtStart' is used for states that
-- simply exist out of any chain events (e.g. the 'Idle' state).
data RecordedAt
  = AtStart
  | AtPoint ChainPoint SomeOnChainHeadStateAt
  deriving (Eq, Show)

data ChainSyncHandler m = ChainSyncHandler
  { onRollForward :: Block -> m ()
  , onRollBackward :: Point Block -> m ()
  }

chainSyncHandler ::
  forall m.
  (MonadSTM m) =>
  -- | Tracer for logging
  Tracer m DirectChainLog ->
  -- | Chain callback
  (ChainEvent Tx -> m ()) ->
  -- | On-chain head-state.
  TVar m SomeOnChainHeadStateAt ->
  -- | A chain-sync handler to use in a local-chain-sync client.
  ChainSyncHandler m
chainSyncHandler tracer callback headState =
  ChainSyncHandler
    { onRollBackward
    , onRollForward
    }
 where
  onRollBackward :: Point Block -> m ()
  onRollBackward point = do
    traceWith tracer $ RolledBackward $ SomePoint point
    (st, depth) <- rollback (fromConsensusPointHF point) <$> readTVarIO headState
    atomically $ writeTVar headState st
    callback (Rollback depth)

  onRollForward :: Block -> m ()
  onRollForward blk = do
    let receivedTxs = toList $ getAlonzoTxs blk
    onChainTxs <- reverse <$> atomically (foldM (withNextTx (blockPoint blk)) [] receivedTxs)
    unless (null receivedTxs) $
      traceWith tracer $
        ReceivedTxs
          { onChainTxs
          , receivedTxs = map (\tx -> (getTxId tx, tx)) receivedTxs
          }
    mapM_ (callback . Observation) onChainTxs

  withNextTx :: Point Block -> [OnChainTx Tx] -> ValidatedTx Era -> STM m [OnChainTx Tx]
  withNextTx point observed (fromLedgerTx -> tx) = do
    st <- readTVar headState
    case observeSomeTx tx (currentOnChainHeadState st) of
      Just (onChainTx, st') -> do
        writeTVar headState $
          SomeOnChainHeadStateAt
            { currentOnChainHeadState = st'
            , recordedAt = AtPoint (fromConsensusPointHF point) st
            }
        pure $ onChainTx : observed
      Nothing ->
        pure observed

-- | Rewind some head state back to the first known state that is strictly
-- before the provided 'ChainPoint'.
--
-- This function also computes the /rollback depth/, e.g the number of observed states
-- that needs to be discarded given some rollback 'chainPoint'. This depth is used
-- in 'Hydra.HeadLogic.rollback' to discard corresponding off-chain state. Consequently
-- the two states /must/ be kept in sync in order for rollbacks to be handled properly.
rollback :: ChainPoint -> SomeOnChainHeadStateAt -> (SomeOnChainHeadStateAt, Word)
rollback chainPoint = backward 0
 where
  backward n st =
    case recordedAt st of
      AtStart ->
        (st, n)
      AtPoint at previous ->
        if at <= chainPoint
          then (st, n)
          else backward (succ n) previous

chainSyncClient ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  ChainSyncHandler m ->
  Maybe (Point Block) ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient handler = \case
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
          clientStIdle <$ onRollForward handler block
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          clientStIdle <$ onRollBackward handler point
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

-- | Balance and sign the given partial transaction.
--
-- XXX(SN): This function does 'retry' when no payment UTXO was found and thus
-- might block. This is necessary in some situations when the wallet has not yet
-- "seen" the change output although the direct chain client observed a
-- transaction already. This is a smell and we should try to avoid having this
-- race condition in the first place.
finalizeTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TinyWallet m ->
  TVar m SomeOnChainHeadStateAt ->
  ValidatedTx Era ->
  STM m (ValidatedTx Era)
finalizeTx TinyWallet{sign, getUTxO, coverFee} headState partialTx = do
  someSt <- currentOnChainHeadState <$> readTVar headState
  let headUTxO = (\(SomeOnChainHeadState st) -> getKnownUTxO st) someSt
  walletUTxO <- fromLedgerUTxO . Ledger.UTxO <$> getUTxO
  coverFee (Ledger.unUTxO $ toLedgerUTxO headUTxO) partialTx >>= \case
    Left ErrNoPaymentUTxOFound ->
      retry
    Left ErrUnknownInput{input} -> do
      throwIO
        ( CannotSpendInput
            { input = show input
            , walletUTxO
            , headUTxO
            } ::
            PostTxError Tx
        )
    Left e ->
      throwIO
        ( CannotCoverFees
            { walletUTxO
            , headUTxO
            , reason = show e
            , tx = fromLedgerTx partialTx
            } ::
            PostTxError Tx
        )
    Right validatedTx -> do
      pure $ sign validatedTx

-- | Hardcoded grace time for close transaction to be valid.
-- TODO: replace/remove with deadline contestation
-- TODO: make it a node configuration parameter
closeGraceTime :: SlotNo
closeGraceTime = 100

fromPostChainTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TimeHandle (STM m) ->
  [VerificationKey PaymentKey] ->
  TinyWallet m ->
  TVar m SomeOnChainHeadStateAt ->
  PostChainTx Tx ->
  STM m Tx
fromPostChainTx TimeHandle{currentSlot, convertSlot} cardanoKeys wallet someHeadState tx = do
  SomeOnChainHeadState st <- currentOnChainHeadState <$> readTVar someHeadState
  case (tx, reifyState st) of
    (InitTx params, TkIdle) -> do
      getFuelUTxO wallet >>= \case
        Just (fromLedgerTxIn -> seedInput, _) -> do
          pure $ initialize params cardanoKeys seedInput st
        Nothing ->
          throwIO (NoSeedInput @Tx)
    (AbortTx{}, TkInitialized) -> do
      pure (abort st)
    -- NOTE / TODO: 'CommitTx' also contains a 'Party' which seems redundant
    -- here. The 'Party' is already part of the state and it is the only party
    -- which can commit from this Hydra node.
    (CommitTx{committed}, TkInitialized) -> do
      either throwIO pure (commit committed st)
    -- TODO: We do not rely on the utxo from the collect com tx here because the
    -- chain head-state is already tracking UTXO entries locked by commit scripts,
    -- and thus, can re-construct the committed UTXO for the collectComTx from
    -- the commits' datums.
    --
    -- Perhaps we do want however to perform some kind of sanity check to ensure
    -- that both states are consistent.
    (CollectComTx{}, TkInitialized) -> do
      pure (collect st)
    (CloseTx{confirmedSnapshot}, TkOpen) -> do
      slot <- (+ closeGraceTime) <$> currentSlot
      posixTime <- convertSlot slot
      pure (close confirmedSnapshot (slot, posixTime) st)
    (ContestTx{confirmedSnapshot}, TkClosed) -> do
      slot <- (+ closeGraceTime) <$> currentSlot
      posixTime <- convertSlot slot
      pure (contest confirmedSnapshot (slot, posixTime) st)
    (FanoutTx{utxo}, TkClosed) ->
      pure (fanout utxo st)
    (_, _) ->
      throwIO $ InvalidStateToPost tx

--
-- Helpers
--

-- | This extract __Alonzo__ transactions from a block. If the block wasn't
-- produced in the Alonzo era, it returns a empty sequence.
getAlonzoTxs :: Block -> StrictSeq (ValidatedTx Era)
getAlonzoTxs = \case
  BlockAlonzo (ShelleyBlock (Ledger.Block _ txsSeq) _) ->
    txSeqTxns txsSeq
  _ ->
    mempty

--
-- Tracing
--

-- TODO add  ToJSON, FromJSON instances
data DirectChainLog
  = ToPost {toPost :: PostChainTx Tx}
  | PostingTx {postedTx :: (TxId StandardCrypto, ValidatedTx Era)}
  | PostedTx {postedTxId :: TxId StandardCrypto}
  | ReceivedTxs {onChainTxs :: [OnChainTx Tx], receivedTxs :: [(TxId StandardCrypto, ValidatedTx Era)]}
  | RolledBackward {point :: SomePoint}
  | Wallet TinyWalletLog
  deriving (Eq, Show, Generic)

instance Arbitrary DirectChainLog where
  arbitrary = genericArbitrary

instance ToJSON DirectChainLog where
  toJSON = \case
    ToPost{toPost} ->
      object
        [ "tag" .= String "ToPost"
        , "toPost" .= toPost
        ]
    PostingTx{postedTx} ->
      object
        [ "tag" .= String "PostingTx"
        , "postedTx" .= postedTx
        ]
    PostedTx{postedTxId} ->
      object
        [ "tag" .= String "PostedTx"
        , "postedTxId" .= postedTxId
        ]
    ReceivedTxs{onChainTxs, receivedTxs} ->
      object
        [ "tag" .= String "ReceivedTxs"
        , "onChainTxs" .= onChainTxs
        , "receivedTxs" .= receivedTxs
        ]
    RolledBackward{point} ->
      object
        [ "tag" .= String "RolledBackward"
        , "point" .= show @Text point
        ]
    Wallet log ->
      object
        [ "tag" .= String "Wallet"
        , "contents" .= log
        ]

-- * Querying the cardano node

newtype QueryException
  = QueryException Text
  deriving (Eq, Show)

instance Exception QueryException

queryTipSlotNo :: NetworkId -> FilePath -> IO SlotNo
queryTipSlotNo networkId socket =
  getLocalChainTip (localNodeConnectInfo networkId socket) >>= \case
    ChainTipAtGenesis -> pure 0
    ChainTip slotNo _ _ -> pure slotNo

querySystemStart :: NetworkId -> FilePath -> IO SystemStart
querySystemStart networkId socket =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing QuerySystemStart >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right result -> pure result

queryEraHistory :: NetworkId -> FilePath -> IO (EraHistory CardanoMode)
queryEraHistory networkId socket =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing (QueryEraHistory CardanoModeIsMultiEra) >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right result -> pure result

-- | Query current protocol parameters.
--
-- Throws 'CardanoClientException' if query fails.
queryProtocolParameters :: NetworkId -> FilePath -> IO ProtocolParameters
queryProtocolParameters networkId socket =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              QueryProtocolParameters
          )
   in runQuery networkId socket query

runQuery :: NetworkId -> FilePath -> QueryInMode CardanoMode (Either EraMismatch a) -> IO a
runQuery networkId socket query =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing query >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right (Left eraMismatch) -> throwIO $ QueryException (show eraMismatch)
    Right (Right result) -> pure result

localNodeConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64
