{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.ExternalPAB where

import Hydra.Prelude

import Control.Monad.Class.MonadAsync (Async, async, cancel)
import Control.Monad.Class.MonadSTM (modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Class.MonadSay (say)
import Data.Aeson (Result (Error, Success), eitherDecodeStrict)
import Data.Aeson.Types (fromJSON)
import qualified Data.Map as Map
import Hydra.Chain (
  Chain (Chain, postTx),
  ChainComponent,
  ContestationPeriod,
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
 )
import Hydra.Ledger (Tx, Utxo)
import Hydra.Logging (Tracer)
import Hydra.PAB (PabContract (..), pabPort)
import Hydra.Party (Party)
import Ledger (AssetClass, PubKeyHash, TxOut (txOutValue), TxOutRef, pubKeyHash)
import Ledger.Value (flattenValue)
import Network.HTTP.Req (
  HttpException (VanillaHttpException),
  POST (..),
  ReqBodyJson (..),
  defaultHttpConfig,
  http,
  jsonResponse,
  port,
  req,
  responseBody,
  responseStatusCode,
  runReq,
  (/:),
 )
import Network.WebSockets (receiveData)
import Network.WebSockets.Client (runClient)
import Plutus.PAB.Webserver.Types (InstanceStatusToClient (NewObservableState))
import Wallet.Emulator.Types (Wallet (..), knownWallet, walletPubKey)
import Wallet.Types (ContractInstanceId (..))

data ExternalPabLog = ExternalPabLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ExternalPabLog where
  arbitrary = genericArbitrary

type WalletId = Integer

withExternalPab ::
  forall tx.
  Tx tx =>
  WalletId ->
  Tracer IO ExternalPabLog ->
  ChainComponent tx IO ()
withExternalPab walletId _tracer callback action = do
  bracket (newTVarIO mempty) stopObservers $ \headObservers ->
    withAsync (initTxSubscriber wallet headObservers callback) $ \_ ->
      withAsync (utxoSubscriber wallet) $ \_ ->
        action $ Chain{postTx = postTx headObservers}
 where
  stopObservers obs =
    readTVarIO obs >>= mapM_ (cancel . snd)

  postTx headObservers = \case
    InitTx HeadParameters{contestationPeriod, parties} -> do
      postInitTx wallet $
        PostInitParams
          { contestationPeriod
          , cardanoPubKeys = pubKeyHash <$> pubKeys
          , hydraParties = parties
          }
    AbortTx utxo -> do
      readTVarIO headObservers >>= \case
        -- XXX(SN): use MonadThrow and proper exceptions or even Either
        [] -> error "cannot post AbortTx , unknown head / no thread token yet!"
        -- FIXME(SN): AbortTx should contain info which head to abort, now we
        -- need to guess (first is the latest)
        ((tt, _) : _) -> postAbortTx @tx wallet tt utxo
    tx -> error $ "should post " <> show tx

  wallet = knownWallet walletId

  -- TODO(SN): Parameterize this
  allWallets = map knownWallet [1, 2, 3]
  pubKeys = map walletPubKey allWallets

activateContract :: PabContract -> Wallet -> IO ContractInstanceId
activateContract contract wallet =
  retryOnAnyHttpException $ do
    res <-
      runReq defaultHttpConfig $
        req
          POST
          (http "127.0.0.1" /: "api" /: "contract" /: "activate")
          (ReqBodyJson reqBody)
          jsonResponse
          (port pabPort)
    when (responseStatusCode res /= 200) $
      error "failed to activateContract"
    pure $ responseBody res
 where
  reqBody = ActivateContractRequest (show contract) wallet

-- NOTE(SN): Not using the same type on both ends as having a too complicated
-- 'Party' type to be able to use it properly in plutus ('Lift' and 'IsData'
-- instances), and this would also be annoying in the dependency management.
data PostInitParams = PostInitParams
  { contestationPeriod :: ContestationPeriod
  , cardanoPubKeys :: [PubKeyHash]
  , hydraParties :: [Party]
  }
  deriving (Show, Generic, ToJSON)

instance Arbitrary PostInitParams where
  shrink = genericShrink
  arbitrary = genericArbitrary

-- TODO(SN): use MonadHttp, but clashes with MonadThrow
postInitTx :: Wallet -> PostInitParams -> IO ()
postInitTx wallet params = do
  -- XXX(SN): stop contract instances?
  (ContractInstanceId cid) <- activateContract Init wallet
  retryOnAnyHttpException $
    runReq defaultHttpConfig $ do
      res <-
        req
          POST
          (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: show cid /: "endpoint" /: "init")
          (ReqBodyJson params)
          jsonResponse
          (port pabPort)
      when (responseStatusCode res /= 200) $
        error "failed to postInitTx"
      pure $ responseBody res

-- TODO(SN): use MonadHttp, but clashes with MonadThrow
postAbortTx :: Wallet -> AssetClass -> Utxo tx -> IO ()
postAbortTx wallet headObservers _utxo = do
  -- XXX(SN): stop contract instances?
  (ContractInstanceId cid) <- activateContract Abort wallet
  retryOnAnyHttpException $ do
    runReq defaultHttpConfig $ do
      res <-
        req
          POST
          (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: show cid /: "endpoint" /: "abort")
          (ReqBodyJson headObservers)
          jsonResponse
          (port pabPort)
      when (responseStatusCode res /= 200) $
        error "failed to postAbortTx"
      pure $ responseBody res

data ActivateContractRequest = ActivateContractRequest {caID :: Text, caWallet :: Wallet}
  deriving (Generic, ToJSON)

-- TODO(SN): DRY subscribers and proper error handling

data ErrDecoding
  = ErrDecodingJson String
  | ErrDecodingMsg String
  deriving (Show)

instance Exception ErrDecoding

initTxSubscriber :: Tx tx => Wallet -> TVar IO [(AssetClass, Async IO ())] -> (OnChainTx tx -> IO ()) -> IO ()
initTxSubscriber wallet headObservers callback = do
  (ContractInstanceId cid) <- activateContract WatchInit wallet
  runClient "127.0.0.1" pabPort ("/ws/" <> show cid) $ \con -> forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right (NewObservableState val) -> do
        case fromJSON val of
          Error err -> throwIO $ ErrDecodingJson $ show err
          Success res -> case getLast res of
            Nothing -> pure ()
            Just (tt :: AssetClass, cp :: ContestationPeriod, ps :: [Party]) -> do
              say $ "Observed initTx " <> show tt <> ", " <> show cp <> ", " <> show ps
              -- XXX(SN): posting txs always uses first item in list
              sub <- async $ headSubscriber wallet tt callback
              atomically $ modifyTVar headObservers ((tt, sub) :)
              callback $ OnInitTx cp ps
      Right _ -> pure ()
      Left err ->
        throwIO $ ErrDecodingMsg $ show err

headSubscriber :: Tx tx => Wallet -> AssetClass -> (OnChainTx tx -> IO ()) -> IO ()
headSubscriber wallet tt callback = do
  say $ "Head subscriber: starting watch contract with thread token " <> show tt
  (ContractInstanceId cid) <- activateContract WatchHead wallet
  runReq defaultHttpConfig $ do
    res <-
      req
        POST
        (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: show cid /: "endpoint" /: "watchHead")
        (ReqBodyJson tt)
        (jsonResponse @())
        (port pabPort)
    when (responseStatusCode res /= 200) $
      error "failed to send threadToken to watchHead contract"
  say "Head subscriber: listening..."
  runClient "127.0.0.1" pabPort ("/ws/" <> show cid) $ \con -> forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right (NewObservableState val) -> do
        case fromJSON val of
          Error err -> throwIO $ ErrDecodingJson $ show err
          Success res -> case getLast res of
            Nothing -> pure ()
            Just (tx :: OnChainTx tx) -> do
              say $ "Observed head transaction " <> show tx
              callback tx
      Right _ -> pure ()
      Left err ->
        throwIO $ ErrDecodingMsg $ show err

utxoSubscriber :: Wallet -> IO ()
utxoSubscriber wallet = do
  cid <- unContractInstanceId <$> activateContract GetUtxos wallet
  runClient "127.0.0.1" pabPort ("/ws/" <> show cid) $ \con -> forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right (NewObservableState val) ->
        case fromJSON val of
          Error err -> error $ "decoding error json: " <> show err
          Success res -> case getLast res of
            Nothing -> pure ()
            Just (utxos :: Map TxOutRef TxOut) -> do
              let v = mconcat $ Map.elems $ txOutValue <$> utxos
              say $ "Own funds changed: " ++ show (flattenValue v)
      Right _ -> pure ()
      Left err -> error $ "error decoding msg: " <> show err

retryOnAnyHttpException :: (MonadCatch m, MonadDelay m, MonadIO m) => m b -> m b
retryOnAnyHttpException action = action `catch` onAnyHttpException
 where
  onAnyHttpException = \case
    (VanillaHttpException _) -> threadDelay 1 >> retryOnAnyHttpException action
    e -> throwIO e
