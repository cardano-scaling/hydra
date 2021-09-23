{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
module Hydra.Chain.Direct (
  NetworkMagic (NetworkMagic),
  withIOManager,
  module Hydra.Chain.Direct,
) where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxSeq (txSeqTxns)
import Control.Monad.Class.MonadSTM (
  newTQueueIO,
  newTVarIO,
  readTQueue,
  readTVarIO,
  writeTQueue,
 )
import Control.Tracer (nullTracer)
import Data.Sequence.Strict (StrictSeq)
import Hydra.Chain (
  Chain (..),
  ChainCallback,
  ChainComponent,
  PostChainTx (..),
 )
import Hydra.Chain.Direct.Tx (OnChainHeadState (..), abortTx, initTx, runOnChainTxs)
import Hydra.Chain.Direct.Util (Block, Era, defaultCodecs, nullConnectTracers, versions)
import Hydra.Chain.Direct.Wallet (SigningKey, TinyWallet (..), VerificationKey, withTinyWallet)
import Hydra.Logging (Tracer, traceWith)
import Ouroboros.Consensus.Cardano.Block (GenTx (..), HardForkBlock (BlockAlonzo))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import Ouroboros.Network.Block (Point (..), Tip (..))
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
  ClientStNext (..),
  chainSyncClientPeer,
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (
  LocalTxClientStIdle (..),
  LocalTxSubmissionClient (..),
  localTxSubmissionClientPeer,
 )
import qualified Shelley.Spec.Ledger.API as Ledger
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
  -- | Key pair for the wallet
  (VerificationKey, SigningKey) ->
  ChainComponent tx IO ()
withDirectChain tracer magic iocp addr keyPair callback action = do
  queue <- newTQueueIO
  headState <- newTVarIO Closed
  withTinyWallet magic keyPair iocp addr $ \wallet ->
    race_
      (action $ Chain{postTx = atomically . writeTQueue queue})
      ( connectTo
          (localSnocket iocp addr)
          nullConnectTracers
          (versions magic (client tracer queue headState wallet callback))
          addr
      )

client ::
  (MonadST m, MonadTimer m) =>
  Tracer m DirectChainLog ->
  TQueue m (PostChainTx tx) ->
  TVar m OnChainHeadState ->
  TinyWallet m ->
  ChainCallback tx m ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
client tracer queue headState wallet callback nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClientPeer $ chainSyncClient tracer callback headState
                   in MuxPeer nullTracer cChainSyncCodec peer
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxSubmissionClientPeer $ txSubmissionClient tracer queue headState wallet
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
  forall m tx.
  (MonadSTM m) =>
  Tracer m DirectChainLog ->
  ChainCallback tx m ->
  TVar m OnChainHeadState ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient tracer callback headState =
  ChainSyncClient (pure clientStIdle)
 where
  -- FIXME: This won't work well with real client. Without acquiring any point
  -- (i.e. agreeing on a common state / intersection with the server), the
  -- server will start streaming blocks from the origin.
  --
  -- Since Hydra heads are supposedly always online, it may be sufficient to
  -- simply negotiate the intersection at the current tip, and then, continue
  -- following the chain from that tip. The head, or more exactly, this client,
  -- would not be able to yield on chain events happening in the past, but only
  -- events which occur after the hydra-node is started. For now, since our test
  -- code is unable to illustrate that problem, I'll leave it as it is.
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
            -- REVIEW(SN): There seems to be no 'toList' for StrictSeq? That's
            -- why I resorted to foldMap using the list monoid ('pure')
            let txs = toList $ getAlonzoTxs blk
            traceWith tracer $ ReceiveTxs txs
            onChainTxs <- runOnChainTxs headState txs
            mapM_ callback onChainTxs
            pure clientStIdle
      , recvMsgRollBackward =
          error "Rolled backward!"
      }

txSubmissionClient ::
  forall m tx.
  MonadSTM m =>
  Tracer m DirectChainLog ->
  TQueue m (PostChainTx tx) ->
  TVar m OnChainHeadState ->
  TinyWallet m ->
  LocalTxSubmissionClient (GenTx Block) (ApplyTxErr Block) m ()
txSubmissionClient tracer queue headState TinyWallet{getUtxo} =
  LocalTxSubmissionClient clientStIdle
 where
  clientStIdle :: m (LocalTxClientStIdle (GenTx Block) (ApplyTxErr Block) m ())
  clientStIdle = do
    tx <- atomically $ readTQueue queue
    validatedTx <- fromPostChainTx tx
    traceWith tracer (PostTx validatedTx)
    pure $ SendMsgSubmitTx (GenTxAlonzo . mkShelleyTx $ validatedTx) (const clientStIdle)

  -- FIXME
  -- This is where we need signatures and client credentials. Ideally, we would
  -- rather have this transaction constructed by clients, albeit with some help.
  -- The hydra node could provide a pre-filled transaction body, and let the
  -- client submit a signed transaction.
  --
  -- For now, it simply does not sign..
  --
  fromPostChainTx :: PostChainTx tx -> m (ValidatedTx Era)
  fromPostChainTx = \case
    InitTx p -> do
      txIns <- keys <$> atomically getUtxo
      case txIns of
        (seedInput : _) -> pure $ initTx p seedInput
        [] -> error "cannot find a seed input to pass to Init transaction"
    AbortTx _utxo -> do
      readTVarIO headState >>= \case
        Initial{threadOutput, initials} -> pure $ abortTx threadOutput initials
        st -> error $ "cannot post Abort transaction in state " <> show st
    _ -> error "not implemented"

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
  = PostTx {postedTx :: ValidatedTx Era}
  | ReceiveTxs {receivedTxs :: [ValidatedTx Era]}
  deriving (Eq, Show, Generic)
