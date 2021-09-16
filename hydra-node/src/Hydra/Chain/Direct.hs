{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
module Hydra.Chain.Direct where

import Hydra.Prelude

import Cardano.Binary (serialize)
import Hydra.Chain (Chain (..), ChainComponent, OnChainTx, toOnChainTx)
import Hydra.Ledger (Tx)
import Hydra.Logging (Tracer)

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (toTxSeq)
import Cardano.Slotting.Slot (WithOrigin (Origin))
import Control.Monad.Class.MonadSTM (readTQueue, writeTQueue)
import Control.Tracer (nullTracer)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Hydra.Ledger.Cardano (generateWith)
import Network.TypedProtocol.Codec
import Ouroboros.Consensus.Byron.Ledger.Config (CodecConfig (..))
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (AlonzoEra, CodecConfig (..), GenTx (..), HardForkBlock (BlockAlonzo))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (Apps (aTxSubmissionServer), ClientCodecs, Codecs' (..), clientCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (SupportedNetworkProtocolVersion (..))
import Ouroboros.Consensus.Shelley.Ledger (ApplyTxError, ShelleyBlock, mkShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (..))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx (..))
import Ouroboros.Network.Block
import Ouroboros.Network.Channel
import Ouroboros.Network.Codec
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient
import Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient, chainSyncClientPeer)
import Ouroboros.Network.Protocol.ChainSync.Server (
  ChainSyncServer (..),
  ServerStIdle (..),
  ServerStIntersect (..),
  ServerStNext (..),
  chainSyncServerPeer,
 )
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (
  LocalTxSubmissionClient (..),
  localTxSubmissionClientPeer,
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Server (
  LocalTxSubmissionServer (..),
  localTxSubmissionServerPeer,
 )
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSubmission
import qualified Shelley.Spec.Ledger.API as Ledger

withDirectChain ::
  IO (Channel IO LByteString) ->
  Tracer IO DirectChainLog ->
  ChainComponent tx IO ()
withDirectChain connect _tracer callback action = do
  chan <- connect
  action $ Chain{postTx = postTx chan}
 where
  postTx Channel{send} tx = do
    -- TODO(SN): convert 'postChainTx' to a Cardano tx and send it to the node
    send $ serialize ()
    now <- getCurrentTime
    callback (toOnChainTx now tx)

data DirectChainLog

chainSyncClient ::
  ChainSyncClient header point tip m a
chainSyncClient = undefined

txSubmissionClient ::
  LocalTxSubmissionClient tx reject m a
txSubmissionClient = undefined

--
-- Mock Server
--

type Block = CardanoBlock StandardCrypto

type Era = AlonzoEra StandardCrypto

runMockChainSyncServer ::
  (MonadThrow m, MonadST m, MonadSTM m) =>
  TQueue m (ValidatedTx Era) ->
  Channel m LByteString ->
  m ()
runMockChainSyncServer queue channel =
  void . runPeer nullTracer chainSyncCodec channel $
    chainSyncServerPeer $ mockChainSyncServer queue

mockChainSyncServer ::
  forall m.
  MonadSTM m =>
  TQueue m (ValidatedTx Era) ->
  ChainSyncServer Block (Point Block) (Tip Block) m ()
mockChainSyncServer queue =
  ChainSyncServer (pure serverStIdle)
 where
  tip :: Tip Block
  tip = TipGenesis

  origin :: Point Block
  origin = genesisPoint

  nextBlock :: ValidatedTx Era -> Block
  nextBlock tx =
    -- We will ignore the header so we generate an arbitrary one
    let header = generateWith arbitrary 100
        body = toTxSeq $ StrictSeq.singleton tx
     in BlockAlonzo $ mkShelleyBlock $ Ledger.Block header body

  serverStIdle :: ServerStIdle Block (Point Block) (Tip Block) m ()
  serverStIdle =
    ServerStIdle
      { -- recvMsgRequestNext   :: m (Either (ServerStNext header point tip m a) (m (ServerStNext header point tip m a))),
        recvMsgRequestNext = do
          tx <- atomically $ readTQueue queue
          pure $ Left $ SendMsgRollForward (nextBlock tx) tip (mockChainSyncServer queue)
      , recvMsgFindIntersect = \case
          [] -> pure $ SendMsgIntersectFound origin tip (ChainSyncServer $ pure serverStIdle)
          h : _ -> pure $ SendMsgIntersectFound h tip (ChainSyncServer $ pure serverStIdle)
      , recvMsgDoneClient = pure ()
      }

runMockTxSubmissionServer ::
  (MonadThrow m, MonadST m, MonadSTM m) =>
  TQueue m (ValidatedTx Era) ->
  Channel m LByteString ->
  m ()
runMockTxSubmissionServer queue channel =
  void . runPeer nullTracer txSubmissionCodec channel $
    localTxSubmissionServerPeer $ pure $ mockTxSubmissionServer queue

mockTxSubmissionServer ::
  MonadSTM m =>
  TQueue m (ValidatedTx Era) ->
  LocalTxSubmissionServer (GenTx Block) reject m ()
mockTxSubmissionServer queue =
  LocalTxSubmissionServer
    { recvMsgSubmitTx = \tx -> do
        case tx of
          GenTxAlonzo genTx ->
            atomically $ writeTQueue queue (toValidatedTx genTx)
          _ ->
            -- FIXME: This should really fail? (i.e. SubmitFail)
            pure ()
        pure (LocalTxSubmission.SubmitSuccess, mockTxSubmissionServer queue)
    , recvMsgDone = ()
    }
 where
  toValidatedTx :: GenTx (ShelleyBlock Era) -> ValidatedTx Era
  toValidatedTx (ShelleyTx _id tx) = tx

--
-- Codecs
--

nodeToClientVLatest :: NodeToClientVersion
nodeToClientVLatest =
  fst $ Map.findMax $ supportedNodeToClientVersions proxy
 where
  proxy = Proxy @(CardanoBlock StandardCrypto)

codecs ::
  forall m block.
  (MonadST m, block ~ CardanoBlock StandardCrypto) =>
  EpochSlots ->
  NodeToClientVersion ->
  ClientCodecs block m
codecs epochSlots nodeToClientV =
  clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
 where
  supportedVersions = supportedNodeToClientVersions (Proxy @block)
  cfg = CardanoCodecConfig byron shelley allegra mary alonzo
   where
    byron = ByronCodecConfig epochSlots
    shelley = ShelleyCodecConfig
    allegra = ShelleyCodecConfig
    mary = ShelleyCodecConfig
    alonzo = ShelleyCodecConfig

chainSyncCodec ::
  MonadST m =>
  Codec (ChainSync.ChainSync Block (Point Block) (Tip Block)) DeserialiseFailure m LByteString
chainSyncCodec = codecs (EpochSlots 432000) nodeToClientVLatest & cChainSyncCodec

txSubmissionCodec ::
  MonadST m =>
  Codec (LocalTxSubmission.LocalTxSubmission (GenTx Block) (ApplyTxErr Block)) DeserialiseFailure m LByteString
txSubmissionCodec = codecs (EpochSlots 432000) nodeToClientVLatest & cTxSubmissionCodec
