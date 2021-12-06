{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Companion tiny-wallet for the direct chain component. This module provide
-- some useful utilities to tracking the wallet's UTXO, and accessing it
module Hydra.Chain.Direct.Wallet where

import Cardano.Api (AddressInEra (..), AddressTypeInEra (..), shelleyBasedEra)
import qualified Cardano.Api.Shelley as Cardano.Api
import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (
  Language (PlutusV1),
 )
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (language)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), txscriptfee)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..), hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxBody (
  TxBody,
  collateral,
  inputs,
  outputs,
  scriptIntegrityHash,
  txfee,
  pattern TxOut,
 )
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (..))
import Cardano.Ledger.Alonzo.TxWitness (
  RdmrPtr (RdmrPtr),
  Redeemers (..),
  TxWitness (..),
  unRedeemers,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (PParams)
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (DSIGN, HASH, StandardCrypto)
import Cardano.Ledger.Era (ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Ledger hiding (TxBody, TxOut)
import Cardano.Ledger.Shelley.BlockChain (HashHeader)
import Cardano.Ledger.Val (Val (..), invert)
import Control.Monad.Class.MonadSTM (
  check,
  newEmptyTMVarIO,
  newTVarIO,
  putTMVar,
  readTMVar,
  retry,
  swapTMVar,
  tryTakeTMVar,
  writeTVar,
 )
import Control.Tracer (nullTracer)
import Data.Aeson (Value (String), object, (.=))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain.Direct.Tx (redeemersFromList)
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  SigningKey,
  SomePoint (..),
  VerificationKey,
  defaultCodecs,
  nullConnectTracers,
  versions,
 )
import Hydra.Ledger.Cardano (
  NetworkId (Testnet),
  fromLedgerTxId,
  genKeyPair,
  mkVkAddress,
  signWith,
  toLedgerAddr,
  toLedgerKeyWitness,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Prelude
import Ouroboros.Consensus.Cardano.Block (BlockQuery (..), CardanoEras, pattern BlockAlonzo)
import Ouroboros.Consensus.HardFork.Combinator (MismatchEraInfo)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..), ShelleyHash (..))
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (..))
import Ouroboros.Network.Block (
  Point (..),
  Tip (..),
  blockPoint,
  castPoint,
  genesisPoint,
  pattern BlockPoint,
  pattern GenesisPoint,
 )
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
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (generate)

type Address = Ledger.Addr StandardCrypto
type TxBody = Ledger.TxBody Era
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
  , sign :: ValidatedTx Era -> ValidatedTx Era
  , coverFee :: Map TxIn TxOut -> ValidatedTx Era -> STM m (Either ErrCoverFee (ValidatedTx Era))
  , verificationKey :: VerificationKey
  }

watchUtxoUntil :: (Map TxIn TxOut -> Bool) -> TinyWallet IO -> IO (Map TxIn TxOut)
watchUtxoUntil predicate TinyWallet{getUtxo} = atomically $ do
  u <- getUtxo
  u <$ check (predicate u)

withTinyWallet ::
  -- | A tracer for logging
  Tracer IO TinyWalletLog ->
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
withTinyWallet tracer magic (vk, sk) iocp addr action = do
  utxoVar <- newEmptyTMVarIO
  tipVar <- newTVarIO genesisPoint
  race_
    (action $ newTinyWallet utxoVar)
    ( connectTo
        (localSnocket iocp)
        nullConnectTracers
        (versions magic $ client tracer tipVar utxoVar address)
        addr
    )
 where
  address =
    toLedgerAddr $
      mkVkAddress (Testnet magic) $ Cardano.Api.PaymentVerificationKey $ Ledger.VKey vk

  newTinyWallet utxoVar =
    TinyWallet
      { getUtxo =
          fst <$> readTMVar utxoVar
      , getAddress =
          address
      , sign = \validatedTx@ValidatedTx{body, wits} ->
          let txid = Ledger.TxId (SafeHash.hashAnnotated body)
              wit =
                fromLedgerTxId txid
                  `signWith` ( Cardano.Api.PaymentVerificationKey (Ledger.VKey vk)
                             , Cardano.Api.PaymentSigningKey sk
                             )
           in validatedTx
                { wits =
                    wits
                      { txwitsVKey = toLedgerKeyWitness @Cardano.Api.AlonzoEra [wit]
                      }
                }
      , coverFee = \lookupUtxo partialTx -> do
          (walletUtxo, pparams) <- readTMVar utxoVar
          case coverFee_ pparams lookupUtxo walletUtxo partialTx of
            Left ErrNoAvailableUtxo ->
              retry
            Left e ->
              pure (Left e)
            Right (walletUtxo', balancedTx) ->
              swapTMVar utxoVar (walletUtxo', pparams) $> Right balancedTx
      , verificationKey = vk
      }

-- | Apply a block to our wallet. Does nothing if the transaction does not
-- modify the UTXO set, or else, remove consumed utxos and add produced ones.
--
-- To determine whether a produced output is ours, we apply the given function
-- checking the output's address.
applyBlock :: Block -> (Address -> Bool) -> Map TxIn TxOut -> Map TxIn TxOut
applyBlock blk isOurs utxo = case blk of
  BlockAlonzo (ShelleyBlock (Ledger.Block _ bbody) _) ->
    flip execState utxo $ do
      forM_ (txSeqTxns bbody) $ \tx -> do
        let txId = getTxId tx
        modify (`Map.withoutKeys` inputs (body tx))
        let indexedOutputs =
              let outs = outputs (body tx)
               in StrictSeq.zip (StrictSeq.fromList [0 .. length outs]) outs
        forM_ indexedOutputs $ \(fromIntegral -> ix, out@(TxOut addr _ _)) ->
          when (isOurs addr) $ modify (Map.insert (Ledger.TxIn txId ix) out)
  _ ->
    utxo

getTxId ::
  ( HashAlgorithm (HASH crypto)
  , SafeHash.HashAnnotated
      (Ledger.TxBody (era crypto))
      EraIndependentTxBody
      crypto
  ) =>
  ValidatedTx (era crypto) ->
  Ledger.TxId crypto
getTxId tx = Ledger.TxId $ SafeHash.hashAnnotated (body tx)

data ErrCoverFee
  = ErrNoAvailableUtxo
  | ErrNotEnoughFunds ChangeError
  | ErrUnknownInput {input :: TxIn}
  deriving (Show)

data ChangeError = ChangeError {inputBalance :: Coin, outputBalance :: Coin}
  deriving (Show)

-- | Cover fee for a transaction body using the given UTXO set. This calculate
-- necessary fees and augments inputs / outputs / collateral accordingly to
-- cover for the transaction cost and get the change back.
--
-- TODO: The fee calculation is currently very dumb and static.
coverFee_ ::
  PParams Era ->
  Map TxIn TxOut ->
  Map TxIn TxOut ->
  ValidatedTx Era ->
  Either ErrCoverFee (Map TxIn TxOut, ValidatedTx Era)
coverFee_ pparams lookupUtxo walletUtxo partialTx@ValidatedTx{body, wits} = do
  (input, output) <- maybeToRight ErrNoAvailableUtxo selectUtxo

  let inputs' = inputs body <> Set.singleton input
  resolvedInputs <- traverse resolveInput (toList inputs')

  let adjustedRedeemers = adjustRedeemers (inputs body) inputs' (txrdmrs wits)
      needlesslyHighFee = calculateNeedlesslyHighFee adjustedRedeemers

  change <- first ErrNotEnoughFunds $ mkChange output resolvedInputs (toList $ outputs body) needlesslyHighFee

  let outputs' = outputs body <> StrictSeq.singleton change
      langs =
        [ l
        | (_hash, script) <- Map.toList (txscripts wits)
        , (not . isNativeScript @Era) script
        , Just l <- [language script]
        ]
      finalBody =
        body
          { inputs = inputs'
          , outputs = outputs'
          , collateral = Set.singleton input
          , txfee = needlesslyHighFee
          , scriptIntegrityHash =
              hashScriptIntegrity
                pparams
                (Set.fromList langs)
                adjustedRedeemers
                (txdats wits)
          }
  pure
    ( Map.withoutKeys walletUtxo inputs'
    , partialTx
        { body = finalBody
        , wits = wits{txrdmrs = adjustedRedeemers}
        }
    )
 where
  selectUtxo =
    fmap head
      . nonEmpty
      . filter notInInputs
      $ Map.toList (Map.filter hasEnoughValue walletUtxo)

  notInInputs (i, _o) = i `Set.notMember` inputs body

  -- FIXME: 10 ADAs is arbitrary, just a way to increase the likelihood to cover fees
  hasEnoughValue :: TxOut -> Bool
  hasEnoughValue = (> Coin 10_000_000) . getAdaValue

  -- TODO: Do a better fee estimation based on the transaction's content.
  calculateNeedlesslyHighFee (Redeemers redeemers) =
    let executionCost = txscriptfee (_prices pparams) $ foldMap snd redeemers
     in Coin 2_000_000 <> executionCost

  getAdaValue :: TxOut -> Coin
  getAdaValue (TxOut _ value _) =
    coin value

  resolveInput :: TxIn -> Either ErrCoverFee TxOut
  resolveInput i = do
    case Map.lookup i (lookupUtxo <> walletUtxo) of
      Nothing -> Left $ ErrUnknownInput i
      Just o -> Right o

  mkChange ::
    TxOut ->
    [TxOut] ->
    [TxOut] ->
    Coin ->
    Either ChangeError TxOut
  mkChange (TxOut addr _ datum) resolvedInputs otherOutputs fee
    -- FIXME: The delta between in and out must be greater than the min utxo value!
    | totalIn <= totalOut =
      Left $ ChangeError totalIn totalOut
    | otherwise =
      Right $ TxOut addr (inject changeOut) datum
   where
    totalOut = foldMap getAdaValue otherOutputs <> fee
    totalIn = foldMap getAdaValue resolvedInputs
    changeOut = totalIn <> invert totalOut

  adjustRedeemers :: Set TxIn -> Set TxIn -> Redeemers Era -> Redeemers Era
  adjustRedeemers initialInputs finalInputs (Redeemers initialRedeemers) =
    Redeemers $ Map.fromList $ map adjustOne $ Map.toList initialRedeemers
   where
    sortedInputs = sort $ toList initialInputs
    sortedFinalInputs = sort $ toList finalInputs
    differences = List.findIndices (not . uncurry (==)) $ zip sortedInputs sortedFinalInputs
    adjustOne (ptr@(RdmrPtr t idx), (d, _exUnits))
      | fromIntegral idx `elem` differences =
        (RdmrPtr t (idx + 1), (d, maxExecutionUnits))
      | otherwise =
        (ptr, (d, maxExecutionUnits))

    maxExecutionUnits :: ExUnits
    maxExecutionUnits =
      let ExUnits mem steps = _maxTxExUnits pparams
          nRedeemers = fromIntegral (Map.size initialRedeemers)
       in ExUnits (mem `div` nRedeemers) (steps `div` nRedeemers)

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
  Tracer m TinyWalletLog ->
  TVar m (Point Block) ->
  TMVar m (Map TxIn TxOut, PParams Era) ->
  Address ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
client tracer tipVar utxoVar address nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClientPeer $ chainSyncClient tracer tipVar utxoVar address
                   in MuxPeer nullTracer cChainSyncCodec peer
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxSubmissionPeerNull
                   in MuxPeer nullTracer cTxSubmissionCodec peer
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  let peer = localStateQueryClientPeer $ stateQueryClient tracer tipVar utxoVar address
                   in MuxPeer nullTracer cStateQueryCodec peer
            }
    )
    nodeToClientV
 where
  Codecs{cChainSyncCodec, cTxSubmissionCodec, cStateQueryCodec} =
    defaultCodecs nodeToClientV

type OnRollback m = ChainSyncClient Block (Point Block) (Tip Block) m ()

-- NOTE: We are fetching PParams only once, when the client first starts. Which
-- means that, if the params change later, we may start producing invalid
-- transactions. In principle, we also expect the Hydra node to monitor the
-- chain for parameter updates and to close heads when this happens. Thus, from
-- the perspective of the client it's okay-ish to fetch it only once.
--
chainSyncClient ::
  forall m.
  (MonadSTM m) =>
  Tracer m TinyWalletLog ->
  TVar m (Point Block) ->
  TMVar m (Map TxIn TxOut, PParams Era) ->
  Address ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient tracer tipVar utxoVar address =
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

  onFirstRollback :: ChainSyncClient Block (Point Block) (Tip Block) m ()
  onFirstRollback = ChainSyncClient (pure $ clientStIdle reset)

  clientStIntersect :: ChainSync.ClientStIntersect Block (Point Block) (Tip Block) m ()
  clientStIntersect =
    ChainSync.ClientStIntersect
      { ChainSync.recvMsgIntersectNotFound = \_tip -> do
          reset
      , ChainSync.recvMsgIntersectFound = \_point _tip ->
          ChainSyncClient (pure $ clientStIdle onFirstRollback)
      }

  clientStIdle ::
    OnRollback m ->
    ChainSync.ClientStIdle Block (Point Block) (Tip Block) m ()
  clientStIdle onRollback =
    ChainSync.SendMsgRequestNext
      (clientStNext onRollback)
      (pure $ clientStNext onRollback)

  clientStNext ::
    OnRollback m ->
    ChainSync.ClientStNext Block (Point Block) (Tip Block) m ()
  clientStNext onRollback =
    ChainSync.ClientStNext
      { ChainSync.recvMsgRollBackward = \_point _tip ->
          onRollback
      , ChainSync.recvMsgRollForward = \block _tip ->
          ChainSyncClient $ do
            msg <- atomically $ do
              (utxo, pparams) <- readTMVar utxoVar
              let utxo' = applyBlock block (== address) utxo
              if utxo' /= utxo
                then do
                  void $ swapTMVar utxoVar (utxo', pparams)
                  pure $ Just $ ApplyBlock utxo utxo'
                else do
                  pure Nothing
            mapM_ (traceWith tracer) msg
            pure (clientStIdle reset)
      }

stateQueryClient ::
  forall m.
  (MonadSTM m, MonadTimer m) =>
  Tracer m TinyWalletLog ->
  TVar m (Point Block) ->
  TMVar m (Map TxIn TxOut, PParams Era) ->
  Address ->
  LocalStateQueryClient Block (Point Block) (Query Block) m ()
stateQueryClient tracer tipVar utxoVar address =
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
          Right tip -> do
            let query = QueryIfCurrentAlonzo GetCurrentPParams
            pure $ LSQ.SendMsgQuery (BlockQuery query) (clientStQueryingPParams $ fromPoint tip)
      }

  fromPoint = \case
    GenesisPoint -> GenesisPoint
    (BlockPoint slot h) -> BlockPoint slot (fromShelleyHash h)
   where
    fromShelleyHash (Ledger.unHashHeader . unShelleyHash -> UnsafeHash h) = coerce h

  clientStQueryingPParams :: Point Block -> LSQ.ClientStQuerying Block (Point Block) (Query Block) m () (QueryResult (PParams Era))
  clientStQueryingPParams tip =
    let query = QueryIfCurrentAlonzo $ GetUTxOByAddress (Set.singleton address)
     in LSQ.ClientStQuerying
          { LSQ.recvMsgResult = \case
              Left{} ->
                handleEraMismatch
              Right pparams ->
                pure $ LSQ.SendMsgQuery (BlockQuery query) (clientStQueryingUtxo tip pparams)
          }
  clientStQueryingUtxo :: Point Block -> PParams Era -> LSQ.ClientStQuerying Block (Point Block) (Query Block) m () (QueryResult UtxoSet)
  clientStQueryingUtxo tip pparams =
    LSQ.ClientStQuerying
      { LSQ.recvMsgResult = \case
          Left{} ->
            handleEraMismatch
          Right (Ledger.unUTxO -> utxo) -> do
            traceWith tracer $ InitializingWallet (SomePoint tip) utxo
            atomically $ do
              writeTVar tipVar tip
              putTMVar utxoVar (utxo, pparams)
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
  ( Cardano.Api.PaymentVerificationKey (Ledger.VKey vk)
    , Cardano.Api.PaymentSigningKey sk
    ) <-
    generate genKeyPair
  pure (vk, sk)

--
-- Logs
--

data TinyWalletLog
  = InitializingWallet SomePoint (Map TxIn TxOut)
  | ApplyBlock (Map TxIn TxOut) (Map TxIn TxOut)
  deriving (Eq, Generic, Show)

instance ToJSON TinyWalletLog where
  toJSON =
    \case
      (InitializingWallet point initialUtxo) ->
        object
          [ "tag" .= String "InitializingWallet"
          , "point" .= show @Text point
          , "initialUtxo" .= initialUtxo
          ]
      (ApplyBlock utxo utxo') ->
        object
          [ "tag" .= String "ApplyBlock"
          , "before" .= utxo
          , "after" .= utxo'
          ]

instance Arbitrary TinyWalletLog where
  arbitrary = genericArbitrary
