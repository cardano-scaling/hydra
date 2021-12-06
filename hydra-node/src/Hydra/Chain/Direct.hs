{-# LANGUAGE BangPatterns #-}
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
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (UtxosFailure))
import Cardano.Ledger.Alonzo.Rules.Utxos (TagMismatchDescription (FailedUnexpectedly), UtxosPredicateFailure (ValidationTagMismatch))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (WrappedShelleyEraFailure))
import Cardano.Ledger.Alonzo.Tx (TxBody (inputs), ValidatedTx (body))
import Cardano.Ledger.Alonzo.TxInfo (FailureDescription (PlutusFailure), debugPlutus)
import Cardano.Ledger.Alonzo.TxSeq (txSeqTxns)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (ApplyTxError (ApplyTxError), TxId, TxIn (TxIn))
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (UtxowFailure))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (UtxoFailure))
import Control.Exception (IOException)
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (MonadSTMTx (writeTVar), newTQueueIO, newTVarIO, readTQueue, throwSTM, writeTQueue)
import Control.Tracer (nullTracer)
import Data.Aeson (Value (String), object, (.=))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set
import Hydra.Chain (
  Chain (..),
  ChainCallback,
  ChainComponent,
  InvalidTxError (..),
  OnChainTx (PostTxFailed),
  PostChainTx (..),
 )
import Hydra.Chain.Direct.Tx (
  OnChainHeadState (..),
  abortTx,
  closeTx,
  collectComTx,
  commitTx,
  fanoutTx,
  initTx,
  knownUtxo,
  observeAbortTx,
  observeCloseTx,
  observeCollectComTx,
  observeCommitTx,
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
import qualified Hydra.Chain.Direct.Util as Cardano
import Hydra.Chain.Direct.Wallet (
  ErrCoverFee (ErrUnknownInput, input),
  TinyWallet (..),
  TinyWalletLog,
  getTxId,
  withTinyWallet,
 )
import Hydra.Ledger.Cardano (CardanoTx, fromLedgerTxId, fromLedgerUtxo, utxoPairs)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Party (Party)
import Hydra.Snapshot (Snapshot (..))
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
  (Cardano.VerificationKey, Cardano.SigningKey) ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | Cardano keys of all Head participants.
  [Cardano.VerificationKey] ->
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

                    res <- atomically $ do
                      fromPostChainTx wallet headState cardanoKeys tx >>= \case
                        Nothing ->
                          pure Nothing
                        Just partialTx ->
                          Just <$> finalizeTx wallet headState partialTx

                    maybe
                      (callback PostTxFailed)
                      (atomically . writeTQueue queue)
                      res
                }
        )
        ( connectTo
            (localSnocket iocp)
            nullConnectTracers
            (versions networkMagic (client tracer queue party headState callback))
            socketPath
        )
 where
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
  TQueue m (ValidatedTx Era) ->
  Party ->
  TVar m OnChainHeadState ->
  ChainCallback CardanoTx m ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
client tracer queue party headState callback nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClientPeer $ chainSyncClient tracer callback party headState
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
  ChainCallback CardanoTx m ->
  Party ->
  TVar m OnChainHeadState ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient tracer callback party headState =
  ChainSyncClient (pure initStIdle)
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
              traceWith tracer $ ReceivedTxs{onChainTxs, receivedTxs = map getTxId receivedTxs}
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
  runOnChainTx observed tx = do
    onChainHeadState <- readTVar headState
    let utxo = knownUtxo onChainHeadState
    -- TODO(SN): We should be only looking for abort,commit etc. when we have a headId/policyId
    let res =
          observeInitTx party tx
            <|> observeCommit onChainHeadState tx
            <|> observeCollectComTx utxo tx
            <|> observeCloseTx utxo tx
            <|> observeAbortTx utxo tx
            <|> observeFanoutTx utxo tx
    case res of
      Just (onChainTx, newOnChainHeadState) -> do
        writeTVar headState newOnChainHeadState
        pure $ onChainTx : observed
      Nothing -> pure observed

  -- XXX(SN): this is not covered by tests
  observeCommit s tx = case s of
    Initial{threadOutput, initials, commits} -> do
      (onChainTx, commitTriple) <- observeCommitTx tx
      -- NOTE(SN): A commit tx has been observed and thus we can remove all it's
      -- inputs from our tracked initials
      let commitIns = inputs $ body tx
      let initials' = filter (\(i, _, _) -> i `Set.member` commitIns) initials
      let commits' = commitTriple : commits
      pure
        ( onChainTx
        , Initial
            { threadOutput
            , initials = initials'
            , commits = commits'
            }
        )
    _ -> Nothing

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
    traceWith tracer (PostedTx $ getTxId tx)
    pure $
      SendMsgSubmitTx
        (GenTxAlonzo . mkShelleyTx $ tx)
        ( \case
            SubmitFail reason -> onFail reason
            SubmitSuccess -> clientStIdle
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

finalizeTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TinyWallet m ->
  TVar m OnChainHeadState ->
  ValidatedTx Era ->
  STM m (ValidatedTx Era)
finalizeTx TinyWallet{sign, getUtxo, coverFee} headState partialTx = do
  headUtxo <- knownUtxo <$> readTVar headState
  walletUtxo <- fromLedgerUtxo . Ledger.UTxO <$> getUtxo
  coverFee headUtxo partialTx >>= \case
    Left ErrUnknownInput{input = TxIn txId txIx} -> do
      throwSTM
        ( CannotSpendInput
            { input = (fromLedgerTxId txId, txIx)
            , walletUtxo
            , headUtxo = fromLedgerUtxo $ Ledger.UTxO headUtxo
            } ::
            InvalidTxError CardanoTx
        )
    Left e ->
      error
        ( "failed to cover fee for transaction: "
            <> show e
            <> ", "
            <> show partialTx
            <> ", using head utxo: "
            <> show headUtxo
            <> ", and wallet utxo: "
            <> show walletUtxo
        )
    Right validatedTx -> do
      pure $ sign validatedTx

fromPostChainTx ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TinyWallet m ->
  TVar m OnChainHeadState ->
  [Cardano.VerificationKey] ->
  PostChainTx CardanoTx ->
  STM m (Maybe (ValidatedTx Era))
fromPostChainTx TinyWallet{getUtxo, verificationKey} headState cardanoKeys = \case
  InitTx params -> do
    u <- getUtxo
    -- NOTE: 'lookupMax' to favor change outputs!
    case Map.lookupMax u of
      Just (seedInput, _) -> pure . Just $ initTx cardanoKeys params seedInput
      Nothing -> error "cannot find a seed input to pass to Init transaction"
  AbortTx _utxo ->
    readTVar headState >>= \case
      Initial{threadOutput, initials} ->
        case abortTx (convertTuple threadOutput) (Map.fromList $ map convertTuple initials) of
          Left err -> error $ show err
          Right tx -> pure $ Just tx
      _st -> pure Nothing
  CommitTx party utxo ->
    readTVar headState >>= \case
      Initial{initials} -> case ownInitial verificationKey initials of
        Nothing -> error $ "no ownInitial: " <> show initials
        Just initial ->
          case utxoPairs utxo of
            [aUtxo] -> do
              pure . Just $ commitTx party (Just aUtxo) initial
            [] -> do
              pure . Just $ commitTx party Nothing initial
            _ ->
              throwIO (MoreThanOneUtxoCommitted @CardanoTx)
      st -> error $ "cannot post CommitTx, invalid state: " <> show st
  CollectComTx utxo ->
    readTVar headState >>= \case
      Initial{threadOutput, commits} -> do
        pure . Just $ collectComTx utxo (convertTuple threadOutput) (Map.fromList $ fmap (\(a, b, c) -> (a, (b, c))) commits)
      st -> error $ "cannot post CollectComTx, invalid state: " <> show st
  CloseTx Snapshot{number, utxo} ->
    readTVar headState >>= \case
      OpenOrClosed{threadOutput} ->
        pure . Just $ closeTx number utxo threadOutput
      st -> error $ "cannot post CloseTx, invalid state: " <> show st
  FanoutTx{utxo} ->
    readTVar headState >>= \case
      OpenOrClosed{threadOutput} ->
        pure . Just $ fanoutTx utxo (convertTuple threadOutput)
      st -> error $ "cannot post FanOutTx, invalid state: " <> show st
  _ -> error "not implemented"
 where
  convertTuple (i, _, dat) = (i, dat)

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
  | PostedTx {postedTx :: TxId StandardCrypto}
  | ReceivedTxs {onChainTxs :: [OnChainTx CardanoTx], receivedTxs :: [TxId StandardCrypto]}
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
    PostedTx{postedTx} ->
      object
        [ "tag" .= String "PostedTx"
        , "postedTx" .= postedTx
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
