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

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (UtxosFailure))
import Cardano.Ledger.Alonzo.Rules.Utxos (TagMismatchDescription (FailedUnexpectedly), UtxosPredicateFailure (ValidationTagMismatch))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (WrappedShelleyEraFailure))
import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxInfo (FailureDescription (PlutusFailure), debugPlutus)
import Cardano.Ledger.Alonzo.TxSeq (txSeqTxns)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (ApplyTxError (ApplyTxError), TxId)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (UtxowFailure))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (UtxoFailure))
import Control.Exception (IOException)
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (newTQueueIO, newTVarIO, readTQueue, retry, writeTQueue, writeTVar)
import Control.Monad.Class.MonadTimer (timeout)
import Control.Tracer (nullTracer)
import Data.Aeson (Value (String), object, (.=))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Hydra.Cardano.Api (
  NetworkId (Testnet),
  PaymentKey,
  SigningKey,
  UTxO' (..),
  VerificationKey,
  fromLedgerTx,
  fromLedgerTxIn,
  fromLedgerUTxO,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.Chain (
  Chain (..),
  ChainCallback,
  ChainComponent,
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct.Tx (
  OnChainHeadState (..),
  abortTx,
  closeTx,
  collectComTx,
  commitTx,
  fanoutTx,
  getKnownUTxO,
  initTx,
  observeAbortTx,
  observeCloseTx,
  observeCollectComTx,
  observeCommit,
  observeFanoutTx,
  observeInitTx,
  ownInitial,
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
  getTxId,
  withTinyWallet,
 )
import Hydra.Ledger.Cardano (CardanoTx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..))
import Ouroboros.Consensus.Cardano.Block (GenTx (..), HardForkApplyTxErr (ApplyTxErrAlonzo), HardForkBlock (BlockAlonzo))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..))
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
  NetworkMagic ->
  -- | A cross-platform abstraction for managing I/O operations on local sockets
  IOManager ->
  -- | Path to a domain socket used to connect to the server.
  FilePath ->
  -- | Key pair for the wallet.
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | Cardano keys of all Head participants.
  [VerificationKey PaymentKey] ->
  ChainComponent CardanoTx IO ()
withDirectChain tracer networkMagic iocp socketPath keyPair party cardanoKeys callback action = do
  queue <- newTQueueIO
  headState <- newTVarIO None
  handle onIOException $
    withTinyWallet (contramap Wallet tracer) networkMagic keyPair iocp socketPath $ \wallet ->
      race_
        ( do
            -- FIXME: There's currently a race-condition with the actual client
            -- which will only see transactions after it has established
            -- connection with the server's tip. So any transaction submitted
            -- before that tip will be missed.
            --
            -- The way we handle rollbacks is also wrong because it'll
            -- fast-forward to the tip, and not allow recovering intermediate
            -- history.
            threadDelay 2
            action $
              Chain
                { postTx = \tx -> do
                    traceWith tracer $ ToPost tx
                    -- XXX(SN): 'finalizeTx' retries until a payment utxo is
                    -- found. See haddock for details
                    timeoutThrowAfter (NoPaymentInput @CardanoTx) 10 $
                      fromPostChainTx wallet (Testnet networkMagic) headState cardanoKeys tx
                        >>= finalizeTx wallet headState . toLedgerTx
                        >>= writeTQueue queue
                }
        )
        ( connectTo
            (localSnocket iocp)
            nullConnectTracers
            (versions networkMagic (client tracer networkMagic queue party headState callback))
            socketPath
        )
 where
  timeoutThrowAfter ex s stm = do
    timeout s (atomically stm) >>= \case
      Nothing -> throwIO ex
      Just _ -> pure ()

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      ConnectException
        { ioException
        , socketPath
        , networkMagic
        }

data ConnectException = ConnectException
  { ioException :: IOException
  , socketPath :: FilePath
  , networkMagic :: NetworkMagic
  }
  deriving (Show)

instance Exception ConnectException

client ::
  (MonadST m, MonadTimer m) =>
  Tracer m DirectChainLog ->
  NetworkMagic ->
  TQueue m (ValidatedTx Era) ->
  Party ->
  TVar m OnChainHeadState ->
  ChainCallback CardanoTx m ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
client tracer networkMagic queue party headState callback nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClientPeer $ chainSyncClient tracer networkMagic callback party headState
                   in MuxPeer nullTracer cChainSyncCodec peer
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxSubmissionClientPeer $ txSubmissionClient tracer queue
                   in MuxPeer nullTracer cTxSubmissionCodec peer
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  let peer = localStateQueryPeerNull
                   in MuxPeer nullTracer cStateQueryCodec peer
            }
    )
    nodeToClientV
 where
  Codecs
    { cChainSyncCodec
    , cTxSubmissionCodec
    , cStateQueryCodec
    } = defaultCodecs nodeToClientV

chainSyncClient ::
  forall m.
  MonadSTM m =>
  Tracer m DirectChainLog ->
  NetworkMagic ->
  ChainCallback CardanoTx m ->
  Party ->
  TVar m OnChainHeadState ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient tracer networkMagic callback party headState =
  ChainSyncClient (pure initStIdle)
 where
  networkId = Testnet networkMagic

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
            ChainSyncClient $ pure $ SendMsgFindIntersect [tip] initStIntersect
        , recvMsgRollBackward = \_ (getTipPoint -> tip) ->
            ChainSyncClient $ pure $ SendMsgFindIntersect [tip] initStIntersect
        }

    initStIntersect :: ClientStIntersect Block (Point Block) (Tip Block) m ()
    initStIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \_ _ ->
            ChainSyncClient (pure clientStIdle)
        , recvMsgIntersectNotFound = \(getTipPoint -> tip) ->
            ChainSyncClient $ pure $ SendMsgFindIntersect [tip] initStIntersect
        }

  clientStIdle :: ClientStIdle Block (Point Block) (Tip Block) m ()
  clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

  -- FIXME: rolling forward with a transaction does not necessarily mean that we
  -- can't roll backward. Or said differently, the block / transactions yielded
  -- by the server are not necessarily settled. Settlement only happens after a
  -- while and we will have to carefully consider how we want to handle
  -- rollbacks. What happen if an 'init' transaction is rolled back?
  --
  -- At the moment, we trigger the callback directly, though we may want to
  -- perhaps only yield transactions through the callback once they have
  -- 'settled' and keep a short buffer of pending transactions in the network
  -- layer directly? To be discussed.
  clientStNext :: ClientStNext Block (Point Block) (Tip Block) m ()
  clientStNext =
    ClientStNext
      { recvMsgRollForward = \blk _tip -> do
          ChainSyncClient $ do
            let receivedTxs = toList $ getAlonzoTxs blk
            onChainTxs <- runOnChainTxs receivedTxs
            unless (null receivedTxs) $
              traceWith tracer $ ReceivedTxs{onChainTxs, receivedTxs = map (\tx -> (getTxId tx, tx)) receivedTxs}
            mapM_ callback onChainTxs
            pure clientStIdle
      , recvMsgRollBackward = \point _tip ->
          ChainSyncClient $ do
            traceWith tracer $ RolledBackward $ SomePoint point
            pure clientStIdle
      }

  runOnChainTxs :: [ValidatedTx Era] -> m [OnChainTx CardanoTx]
  runOnChainTxs = fmap reverse . atomically . foldM runOnChainTx []

  runOnChainTx :: [OnChainTx CardanoTx] -> ValidatedTx Era -> STM m [OnChainTx CardanoTx]
  runOnChainTx observed (fromLedgerTx -> tx) = do
    onChainHeadState <- readTVar headState
    let utxo = UTxO (getKnownUTxO onChainHeadState)
    -- TODO(SN): We should be only looking for abort,commit etc. when we have a headId/policyId
    let res =
          observeInitTx networkId party tx
            <|> observeCommit networkId tx onChainHeadState
            <|> observeCollectComTx utxo tx
            <|> observeCloseTx utxo tx
            <|> observeAbortTx utxo tx
            <|> observeFanoutTx utxo tx
    case res of
      Just (onChainTx, newOnChainHeadState) -> do
        writeTVar headState newOnChainHeadState
        pure $ onChainTx : observed
      Nothing -> pure observed

txSubmissionClient ::
  forall m.
  MonadSTM m =>
  Tracer m DirectChainLog ->
  TQueue m (ValidatedTx Era) ->
  LocalTxSubmissionClient (GenTx Block) (ApplyTxErr Block) m ()
txSubmissionClient tracer queue =
  LocalTxSubmissionClient clientStIdle
 where
  clientStIdle :: m (LocalTxClientStIdle (GenTx Block) (ApplyTxErr Block) m ())
  clientStIdle = do
    tx <- atomically $ readTQueue queue
    traceWith tracer (PostingTx (getTxId tx, tx))
    pure $
      SendMsgSubmitTx
        (GenTxAlonzo . mkShelleyTx $ tx)
        ( \case
            SubmitFail reason -> onFail reason
            SubmitSuccess -> traceWith tracer (PostedTx (getTxId tx)) >> clientStIdle
        )

  -- XXX(SN): patch-work error pretty printing on single plutus script failures
  onFail = \case
    ApplyTxErrAlonzo (ApplyTxError [failure]) -> unwrapPlutus failure
    err -> error $ "failed to submit tx: " <> show err

  unwrapPlutus :: LedgerPredicateFailure Era -> p
  unwrapPlutus = \case
    UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch _ (FailedUnexpectedly [PlutusFailure plutusFailure debug]))))) ->
      error $ "Plutus validation failed: " <> plutusFailure <> "\nDebugInfo: " <> show (debugPlutus PlutusV1 (decodeUtf8 debug))
    err -> error $ "Some other ledger failure: " <> show err

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
  TVar m OnChainHeadState ->
  ValidatedTx Era ->
  STM m (ValidatedTx Era)
finalizeTx TinyWallet{sign, getUTxO, coverFee} headState partialTx = do
  headUTxO <- UTxO . getKnownUTxO <$> readTVar headState
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
            PostTxError CardanoTx
        )
    Left e ->
      throwIO
        ( CannotCoverFees
            { walletUTxO
            , headUTxO
            , reason = show e
            , tx = fromLedgerTx partialTx
            } ::
            PostTxError CardanoTx
        )
    Right validatedTx -> do
      pure $ sign validatedTx

fromPostChainTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TinyWallet m ->
  NetworkId ->
  TVar m OnChainHeadState ->
  [VerificationKey PaymentKey] ->
  PostChainTx CardanoTx ->
  STM m CardanoTx
fromPostChainTx TinyWallet{getUTxO, verificationKey} networkId headState cardanoKeys tx =
  case tx of
    InitTx params -> do
      u <- getUTxO
      -- NOTE: 'lookupMax' to favor change outputs!
      case Map.lookupMax u of
        Just (fromLedgerTxIn -> seedInput, _) -> pure $ initTx networkId cardanoKeys params seedInput
        Nothing -> throwIO (NoSeedInput @CardanoTx)
    AbortTx _utxo ->
      readTVar headState >>= \case
        Initial{threadOutput, initials, commits} ->
          let (i, _, dat, _) = threadOutput
           in case abortTx networkId (i, dat) (Map.fromList $ map convertTuple initials) (Map.fromList $ map tripleToPair commits) of
                Left err -> error $ show err
                Right tx' -> pure tx'
        _st -> throwIO $ InvalidStateToPost tx
    CommitTx party utxo ->
      readTVar headState >>= \case
        st@Initial{initials} -> case ownInitial verificationKey initials of
          Nothing ->
            throwIO (CannotFindOwnInitial{knownUTxO = UTxO $ getKnownUTxO st} :: PostTxError CardanoTx)
          Just initial ->
            case UTxO.pairs utxo of
              [aUTxO] -> do
                pure $ commitTx networkId party (Just aUTxO) initial
              [] -> do
                pure $ commitTx networkId party Nothing initial
              _ ->
                throwIO (MoreThanOneUTxOCommitted @CardanoTx)
        _st -> throwIO $ InvalidStateToPost tx
    -- TODO: We do not rely on the utxo from the collect com tx here because the
    -- chain head-state is already tracking UTXO entries locked by commit scripts,
    -- and thus, can re-construct the committed UTXO for the collectComTx from
    -- the commits' datums.
    --
    -- Perhaps we do want however to perform some kind of sanity check to ensure
    -- that both states are consistent.
    CollectComTx{} ->
      readTVar headState >>= \case
        Initial{threadOutput, commits} -> do
          let (i, _, dat, parties) = threadOutput
          pure $ collectComTx networkId (i, dat, parties) (Map.fromList $ fmap tripleToPair commits)
        _st -> throwIO $ InvalidStateToPost tx
    CloseTx confirmedSnapshot ->
      readTVar headState >>= \case
        OpenOrClosed{threadOutput} -> do
          let (sn, sigs) =
                case confirmedSnapshot of
                  ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
                  InitialSnapshot{snapshot} -> (snapshot, mempty)
          let (i, o, dat, _) = threadOutput
          pure $ closeTx sn sigs (i, o, dat)
        _st -> throwIO $ InvalidStateToPost tx
    FanoutTx{utxo} ->
      readTVar headState >>= \case
        OpenOrClosed{threadOutput} ->
          let (i, _, dat, _) = threadOutput
           in pure $ fanoutTx utxo (i, dat)
        _st -> throwIO $ InvalidStateToPost tx
    _ -> error "not implemented"
 where
  convertTuple (i, _, dat) = (i, dat)

  tripleToPair (a, b, c) = (a, (b, c))

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
  = ToPost {toPost :: PostChainTx CardanoTx}
  | PostingTx {postedTx :: (TxId StandardCrypto, ValidatedTx Era)}
  | PostedTx {postedTxId :: TxId StandardCrypto}
  | ReceivedTxs {onChainTxs :: [OnChainTx CardanoTx], receivedTxs :: [(TxId StandardCrypto, ValidatedTx Era)]}
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
