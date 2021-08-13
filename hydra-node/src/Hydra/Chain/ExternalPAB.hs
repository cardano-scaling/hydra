{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.ExternalPAB where

import Hydra.Prelude

import Control.Monad.Class.MonadSay (say)
import Data.Aeson (Result (Error, Success), eitherDecodeStrict)
import Data.Aeson.Types (fromJSON)
import qualified Data.Map as Map
import Hydra.Chain (
  Chain (Chain, postTx),
  ChainComponent,
  ContestationPeriod,
  HeadParameters (..),
  PostChainTx (..),
 )
import Hydra.Contract.PAB (PabContract (..), pabPort)
import Hydra.Ledger (Party, Tx, Utxo)
import Hydra.Logging (Tracer)
import Ledger (PubKeyHash, TxOut (txOutValue), pubKeyHash, txOutTxOut)
import Ledger.AddressMap (UtxoMap)
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
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Wallet.Types (ContractInstanceId (..))

data ExternalPabLog = ExternalPabLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type WalletId = Integer

withExternalPab ::
  forall tx.
  Tx tx =>
  WalletId ->
  Tracer IO ExternalPabLog ->
  ChainComponent tx IO ()
withExternalPab walletId _tracer callback action = do
  cid <- activateContract Watch wallet
  withAsync (initTxSubscriber cid callback) $ \_ ->
    withAsync (utxoSubscriber wallet) $ \_ -> do
      action $ Chain{postTx = postTx cid}
 where
  postTx cid = \case
    InitTx HeadParameters{contestationPeriod, parties} -> do
      -- XXX(SN): stop contract instances
      oneTimeCid <- activateContract Init wallet
      postInitTx oneTimeCid $
        PostInitParams
          { contestationPeriod
          , cardanoPubKeys = pubKeyHash <$> pubKeys
          , hydraParties = parties
          }
    AbortTx utxo -> do
      postAbortTx @tx cid utxo
    tx -> error $ "should post " <> show tx

  wallet = Wallet walletId

  -- TODO(SN): Parameterize this
  allWallets = [Wallet 1, Wallet 2, Wallet 3]
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
postInitTx :: ContractInstanceId -> PostInitParams -> IO ()
postInitTx cid params =
  retryOnAnyHttpException $
    runReq defaultHttpConfig $ do
      res <-
        req
          POST
          (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: cidText /: "endpoint" /: "init")
          (ReqBodyJson params)
          jsonResponse
          (port pabPort)
      when (responseStatusCode res /= 200) $
        error "failed to postInitTx"
      pure $ responseBody res
 where
  cidText = show $ unContractInstanceId cid

-- TODO(SN): use MonadHttp, but clashes with MonadThrow
postAbortTx :: ContractInstanceId -> Utxo tx -> IO ()
postAbortTx cid _utxo =
  retryOnAnyHttpException $ do
    say "send abort http request"
    runReq defaultHttpConfig $ do
      res <-
        req
          POST
          (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: cidText /: "endpoint" /: "abort")
          (ReqBodyJson ())
          jsonResponse
          (port pabPort)
      when (responseStatusCode res /= 200) $
        error "failed to postAbortTx"
      pure $ responseBody res
 where
  cidText = show $ unContractInstanceId cid

data ActivateContractRequest = ActivateContractRequest {caID :: Text, caWallet :: Wallet}
  deriving (Generic, ToJSON)

-- TODO(SN): DRY subscribers and proper error handling

initTxSubscriber :: ContractInstanceId -> (OnChainTx tx -> IO ()) -> IO ()
initTxSubscriber (ContractInstanceId cid) callback = do
  runClient "127.0.0.1" pabPort ("/ws/" <> show cid) $ \con -> forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right (NewObservableState val) -> do
        case fromJSON val of
          Error err -> say $ "decoding error json: " <> show err
          Success res -> case getLast res of
            Nothing -> pure ()
            Just (parameters :: HeadParameters) -> do
              say $ "Observed Init tx with parameters:" ++ show parameters
              callback $ InitTx parameters
      Right _ -> pure ()
      Left err -> say $ "error decoding msg: " <> show err

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
            Just (utxos :: UtxoMap) -> do
              let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
              say $ "Own funds changed: " ++ show (flattenValue v)
      Right _ -> pure ()
      Left err -> error $ "error decoding msg: " <> show err

retryOnAnyHttpException :: (MonadCatch m, MonadDelay m, MonadIO m) => m b -> m b
retryOnAnyHttpException action = action `catch` onAnyHttpException
 where
  onAnyHttpException = \case
    (VanillaHttpException _) -> threadDelay 1 >> retryOnAnyHttpException action
    e -> throwIO e
