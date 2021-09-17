-- | Companion tiny-wallet for the direct chain component. This module provide
-- some useful utilities to tracking the wallet's UTXO, and accessing it
module Hydra.Chain.Direct.Wallet (
  withTinyWallet,
  TinyWallet (..),
  NetworkMagic (..),
  applyBlock,
) where

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash.Class (Hash (..))
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (DSIGN, StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import Control.Monad.Class.MonadSTM (
  check,
  newEmptyTMVarIO,
  newTVarIO,
  putTMVar,
  readTMVar,
  swapTMVar,
  tryTakeTMVar,
  writeTVar,
 )
import Control.Tracer (nullTracer)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  defaultCodecs,
  nullConnectTracers,
  versions,
 )
import Hydra.Ledger.Cardano (mkVkAddress, signWith)
import Hydra.Prelude
import Ouroboros.Consensus.Cardano.Block (BlockQuery (..), CardanoEras)
import Ouroboros.Consensus.HardFork.Combinator (MismatchEraInfo)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock, ShelleyHash (..))
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (..))
import Ouroboros.Network.Block (Point (..), Tip (..), castPoint, genesisPoint)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (
  MuxMode (..),
  MuxPeer (..),
  OuroborosApplication (..),
  RunMiniProtocol (..),
 )
import Ouroboros.Network.NodeToClient (
  LocalAddress (..),
  NodeToClientProtocols (..),
  NodeToClientVersion,
  connectTo,
  localSnocket,
  localTxSubmissionPeerNull,
  nodeToClientProtocols,
  withIOManager,
 )
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  chainSyncClientPeer,
 )
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import Ouroboros.Network.Protocol.LocalStateQuery.Client (
  LocalStateQueryClient (..),
  localStateQueryClientPeer,
 )
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ
import Shelley.Spec.Ledger.BlockChain (HashHeader (..))
import qualified Shelley.Spec.Ledger.TxBody as Ledger hiding (TxBody, TxOut)
import qualified Shelley.Spec.Ledger.UTxO as Ledger

type Address = Ledger.Addr StandardCrypto
type TxBody = Ledger.TxBody Era
type VerificationKey = Crypto.VerKeyDSIGN (DSIGN StandardCrypto)
type SigningKey = Crypto.SignKeyDSIGN (DSIGN StandardCrypto)
type TxIn = Ledger.TxIn StandardCrypto
type TxOut = Ledger.TxOut Era
type VkWitness = Ledger.WitVKey 'Ledger.Witness StandardCrypto
type QueryResult result = Either (MismatchEraInfo (CardanoEras StandardCrypto)) result
type UtxoSet = Ledger.UTxO Era
type AlonzoPoint = Point (ShelleyBlock Era)

-- | A 'TinyWallet' is a small abstraction of a wallet with basic UTXO
-- management. The wallet is assumed to have only one address, and only one
-- UTXO at that address.
--
-- It can sign transactions and keeps track of its UTXO behind the scene.
data TinyWallet m = TinyWallet
  { getUtxo :: m (Maybe (TxIn, TxOut))
  , getAddress :: Address
  , sign :: TxBody -> VkWitness
  }

withTinyWallet ::
  NetworkMagic ->
  (VerificationKey, SigningKey) ->
  FilePath ->
  (TinyWallet IO -> IO ()) ->
  IO ()
withTinyWallet magic (vk, sk) addr action = do
  utxoVar <- newEmptyTMVarIO
  tipVar <- newTVarIO genesisPoint
  withIOManager $ \iocp -> do
    race_
      (action $ newTinyWallet utxoVar)
      ( connectTo
          (localSnocket iocp addr)
          nullConnectTracers
          (versions magic $ client tipVar utxoVar address)
          addr
      )
 where
  address =
    mkVkAddress (Ledger.VKey vk)

  newTinyWallet utxoVar =
    TinyWallet
      { getUtxo =
          Map.lookupMax <$> atomically (readTMVar utxoVar)
      , getAddress =
          address
      , sign = \body ->
          let txid = Ledger.TxId (SafeHash.hashAnnotated body)
           in txid `signWith` Ledger.KeyPair (Ledger.VKey vk) sk
      }

-- | Apply a block to our wallet. Does nothing if the transaction does not
-- modify the UTXO set, or else, remove consumed utxos and add produced ones.
--
-- To determine whether a produced output is ours, we compare it to our unique
-- address.
applyBlock :: Block -> Address -> Map TxIn TxOut -> Map TxIn TxOut
applyBlock = error "TODO: applyBlock"

-- | The idea for this wallet client is rather simple:
--
-- 1. We bootstrap the client using the Local State Query (abbrev. LSQ) protocol,
-- and in particular the 'GetUTxO' query to get the most recent UTxO at the tip.
--
-- This request is however quite costly (O(n) in the size of the ledger's UTXO).
-- So we don't want to poll it too often and burn needless CPU resources. Plus,
-- whatever poll value we choose, we'll always end up report outdated
-- information. Thus,
--
-- 2. Once the most recent tip and UTXO acquired, we give the baton from the LSQ
-- to the chain-sync protocol, which can continue watching the chain from that
-- point on such that, if the UTXO is ever spent, it'll replace it with its new
-- UTXO (reminder: the wallet has only one address).
--
-- Then, there's a bit of error-handling because, it's possible that between the
-- moment we acquire the tip with the LSQ and pass the relay to the chain-sync,
-- the chain has rolled back and the tip is no longer available. This can be
-- gracefully done via waiting on MVars. See below.
--
-- The MVar also provides a convenient interface for the `TinyWallet m`
-- abstraction which is used by consumers downstreams.
client ::
  (MonadST m, MonadTimer m) =>
  TVar m (Point Block) ->
  TMVar m (Map TxIn TxOut) ->
  Address ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
client tipVar utxoVar address nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClientPeer $ chainSyncClient tipVar utxoVar address
                   in MuxPeer nullTracer cChainSyncCodec peer
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxSubmissionPeerNull
                   in MuxPeer nullTracer cTxSubmissionCodec peer
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  let peer = localStateQueryClientPeer $ stateQueryClient tipVar utxoVar address
                   in MuxPeer nullTracer cStateQueryCodec peer
            }
    )
    nodeToClientV
 where
  Codecs{cChainSyncCodec, cTxSubmissionCodec, cStateQueryCodec} =
    defaultCodecs nodeToClientV

chainSyncClient ::
  forall m.
  (MonadSTM m) =>
  TVar m (Point Block) ->
  TMVar m (Map TxIn TxOut) ->
  Address ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient tipVar utxoVar address =
  reset
 where
  reset :: ChainSyncClient Block (Point Block) (Tip Block) m ()
  reset = ChainSyncClient $ do
    atomically $ do
      writeTVar tipVar genesisPoint
      void $ tryTakeTMVar utxoVar
    tip <- atomically $ do
      tip <- readTVar tipVar
      tip <$ check (tip /= genesisPoint)
    pure $ ChainSync.SendMsgFindIntersect [tip] clientStIntersect

  clientStIntersect :: ChainSync.ClientStIntersect Block (Point Block) (Tip Block) m ()
  clientStIntersect =
    ChainSync.ClientStIntersect
      { ChainSync.recvMsgIntersectNotFound = \_tip -> do
          reset
      , ChainSync.recvMsgIntersectFound = \_point _tip ->
          ChainSyncClient (pure clientStIdle)
      }

  clientStIdle :: ChainSync.ClientStIdle Block (Point Block) (Tip Block) m ()
  clientStIdle =
    ChainSync.SendMsgRequestNext clientStNext (pure clientStNext)

  clientStNext :: ChainSync.ClientStNext Block (Point Block) (Tip Block) m ()
  clientStNext =
    ChainSync.ClientStNext
      { ChainSync.recvMsgRollBackward = \_point _tip ->
          reset
      , ChainSync.recvMsgRollForward = \block _tip ->
          ChainSyncClient $ do
            utxo <- atomically (readTMVar utxoVar)
            let utxo' = applyBlock block address utxo
            when (utxo' /= utxo) (void $ atomically $ swapTMVar utxoVar utxo')
            pure clientStIdle
      }

stateQueryClient ::
  forall m.
  (MonadSTM m, MonadTimer m) =>
  TVar m (Point Block) ->
  TMVar m (Map TxIn TxOut) ->
  Address ->
  LocalStateQueryClient Block (Point Block) (Query Block) m ()
stateQueryClient tipVar utxoVar address =
  LocalStateQueryClient (pure clientStIdle)
 where
  clientStIdle :: LSQ.ClientStIdle Block (Point Block) (Query Block) m ()
  clientStIdle = LSQ.SendMsgAcquire Nothing clientStAcquiring

  clientStAcquiring :: LSQ.ClientStAcquiring Block (Point Block) (Query Block) m ()
  clientStAcquiring =
    LSQ.ClientStAcquiring
      { LSQ.recvMsgAcquired = pure clientStAcquired
      , -- NOTE: This really can't fail, because we acquire 'Nothing'.
        LSQ.recvMsgFailure = const (pure clientStIdle)
      }

  clientStAcquired :: LSQ.ClientStAcquired Block (Point Block) (Query Block) m ()
  clientStAcquired =
    let query = QueryIfCurrentAlonzo GetLedgerTip
     in LSQ.SendMsgQuery (BlockQuery query) clientStQueryingTip

  clientStQueryingTip :: LSQ.ClientStQuerying Block (Point Block) (Query Block) m () (QueryResult AlonzoPoint)
  clientStQueryingTip =
    LSQ.ClientStQuerying
      { LSQ.recvMsgResult = \case
          -- Era mismatch, this can happen if the node is still syncing. In which
          -- case, we can't do much but logging and retrying later.
          Left{} ->
            handleEraMismatch
          Right (castPoint -> tip) -> do
            let query = QueryIfCurrentAlonzo $ GetUTxOByAddress (Set.singleton address)
            pure $ LSQ.SendMsgQuery (BlockQuery query) (clientStQueryingUtxo tip)
      }

  clientStQueryingUtxo :: Point Block -> LSQ.ClientStQuerying Block (Point Block) (Query Block) m () (QueryResult UtxoSet)
  clientStQueryingUtxo tip =
    LSQ.ClientStQuerying
      { LSQ.recvMsgResult = \case
          Left{} ->
            handleEraMismatch
          Right (Ledger.unUTxO -> utxo) -> do
            atomically $ do
              writeTVar tipVar tip
              putTMVar utxoVar utxo
            reset -- NOTE: This will block until the chain-sync client clear
            -- resets the tip TVar to the genesisPoint, which may happen if it
            -- fails to find an intersection with the provided tip due to a race
            -- condition between the node and the state-query protocol. In which
            -- case, we retry the whole process again. Otherwise, it'll block
            -- indefinitely.
      }

  reset :: m (LSQ.ClientStAcquired Block (Point Block) (Query Block) m ())
  reset = do
    atomically $ do
      tip <- readTVar tipVar
      check (tip == genesisPoint)
    pure $ LSQ.SendMsgReAcquire Nothing clientStAcquiring

  handleEraMismatch :: m (LSQ.ClientStAcquired Block (Point Block) (Query Block) m ())
  handleEraMismatch = do
    -- FIXME: log something before looping back?
    threadDelay 30
    reset
