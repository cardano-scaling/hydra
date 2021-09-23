{-# LANGUAGE PatternSynonyms #-}

-- | Companion tiny-wallet for the direct chain component. This module provide
-- some useful utilities to tracking the wallet's UTXO, and accessing it
module Hydra.Chain.Direct.Wallet where

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash.Class (Hash (..))
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (collateral, inputs, outputs, txfee, pattern TxOut)
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (DSIGN, StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Val (Val (..), invert)
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
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  defaultCodecs,
  nullConnectTracers,
  versions,
 )
import Hydra.Ledger.Cardano (genKeyPair, mkVkAddress, signWith)
import Hydra.Prelude
import Ouroboros.Consensus.Cardano.Block (BlockQuery (..), CardanoEras, pattern BlockAlonzo)
import Ouroboros.Consensus.HardFork.Combinator (MismatchEraInfo)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..), ShelleyHash (..))
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
  IOManager,
  LocalAddress (..),
  NodeToClientProtocols (..),
  NodeToClientVersion,
  connectTo,
  localSnocket,
  localTxSubmissionPeerNull,
  nodeToClientProtocols,
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
import qualified Shelley.Spec.Ledger.API as Ledger hiding (TxBody, TxOut)
import Shelley.Spec.Ledger.BlockChain (HashHeader (..))
import Shelley.Spec.Ledger.TxBody (TxId (..), pattern TxIn)
import Test.QuickCheck (generate)

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
  { getUtxo :: STM m (Map TxIn TxOut)
  , getAddress :: Address
  , sign :: TxBody -> VkWitness
  , coverFee :: TxBody -> STM m (Either ErrCoverFee TxBody)
  }

withTinyWallet ::
  -- | Network identifier to which we expect to connect.
  NetworkMagic ->
  -- | Credentials of the wallet.
  (VerificationKey, SigningKey) ->
  -- | A cross-platform abstraction for managing I/O operations on local sockets
  IOManager ->
  -- | Path to a domain socket used to connect to the server.
  FilePath ->
  (TinyWallet IO -> IO ()) ->
  IO ()
withTinyWallet magic (vk, sk) iocp addr action = do
  utxoVar <- newEmptyTMVarIO
  tipVar <- newTVarIO genesisPoint
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
          readTMVar utxoVar
      , getAddress =
          address
      , sign = \body ->
          let txid = Ledger.TxId (SafeHash.hashAnnotated body)
           in txid `signWith` Ledger.KeyPair (Ledger.VKey vk) sk
      , coverFee = \body ->
          coverFee_ <$> readTMVar utxoVar <*> pure body
      }

-- | Apply a block to our wallet. Does nothing if the transaction does not
-- modify the UTXO set, or else, remove consumed utxos and add produced ones.
--
-- To determine whether a produced output is ours, we compare it to our unique
-- address.
applyBlock :: Block -> (Address -> Bool) -> Map TxIn TxOut -> Map TxIn TxOut
applyBlock blk isOurs utxo = case blk of
  BlockAlonzo (ShelleyBlock (Ledger.Block _ bbody) _) ->
    flip execState utxo $ do
      forM_ (txSeqTxns bbody) $ \tx -> do
        let txId = TxId $ SafeHash.hashAnnotated (body tx)
        modify (`Map.withoutKeys` inputs (body tx))
        let indexedOutputs =
              let outs = outputs (body tx)
               in StrictSeq.zip (StrictSeq.fromList [0 .. length outs]) outs
        forM_ indexedOutputs $ \(fromIntegral -> ix, out@(TxOut addr _ _)) ->
          when (isOurs addr) $ modify (Map.insert (TxIn txId ix) out)
  _ ->
    utxo

data ErrCoverFee
  = ErrNoAvailableUtxo
  | ErrNotEnoughFunds {missingDelta :: Coin}
  deriving (Show)

-- | Cover fee for a transaction body using the given UTXO set. This calculate
-- necessary fees and augments inputs / outputs / collateral accordingly to
-- cover for the transaction cost and get the change back.
--
-- TODO: The fee calculation is currently very dumb and static.
coverFee_ ::
  Map TxIn TxOut ->
  TxBody ->
  Either ErrCoverFee TxBody
coverFee_ utxo body = do
  (input, output) <- case Map.lookupMax utxo of
    Nothing ->
      Left ErrNoAvailableUtxo
    Just (i, o) ->
      Right (i, o)
  change <- first ErrNotEnoughFunds $ mkChange output needlesslyHighFee

  let inputs' = inputs body <> Set.singleton input
  let outputs' = outputs body <> StrictSeq.singleton change

  pure $
    body
      { inputs = inputs'
      , outputs = outputs'
      , collateral = Set.singleton input
      , txfee = needlesslyHighFee
      }
 where
  -- TODO: Do a better fee estimation based on the transaction's content.
  needlesslyHighFee :: Coin
  needlesslyHighFee = Coin 2_000_000

  mkChange :: TxOut -> Coin -> Either Coin TxOut
  mkChange (TxOut addr value datum) fee
    | coin value > fee =
      Right $ TxOut addr (value <> invert (inject fee)) datum
    | otherwise =
      Left (fee <> invert (coin value))

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
            let utxo' = applyBlock block (== address) utxo
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

--
-- Keys
--

generateKeyPair :: IO (VerificationKey, SigningKey)
generateKeyPair = do
  Ledger.KeyPair (Ledger.VKey vk) sk <- generate genKeyPair
  pure (vk, sk)
