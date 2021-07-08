{-# LANGUAGE EmptyDataDeriving #-}

module Hydra.Chain.ExternalPAB where

import Hydra.Prelude

import Control.Monad.Class.MonadSay (say)
import Data.Aeson (Result (Error, Success), eitherDecodeStrict)
import Data.Aeson.Types (fromJSON)
import qualified Data.Map as Map
import Hydra.Chain (Chain (Chain, postTx))
import Hydra.Contract.PAB (PABContract (..))
import Hydra.HeadLogic (OnChainTx (InitTx))
import Hydra.Ledger (Party, Tx)
import Hydra.Logging (Tracer)
import Ledger (PubKeyHash, TxOut (txOutValue), pubKeyHash, txOutTxOut)
import Ledger.AddressMap (UtxoMap)
import Ledger.Value as Value
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
import Wallet.Types (ContractInstanceId, unContractInstanceId)

data ExternalPABLog = ExternalPABLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type WalletId = Integer

withExternalPAB ::
  Tx tx =>
  WalletId ->
  Tracer IO ExternalPABLog ->
  (OnChainTx tx -> IO ()) ->
  (Chain tx IO -> IO a) ->
  IO a
withExternalPAB walletId _tracer callback action = do
  withAsync (initTxSubscriber wallet callback) $ \_ ->
    withAsync (utxoSubscriber wallet) $ \_ -> do
      action $ Chain{postTx = postTx}
 where
  postTx = \case
    InitTx _parties -> do
      cid <- activateContract Setup wallet
      postInitTx cid (pubKeyHash <$> pubKeys)
    tx -> error $ "should post " <> show tx

  wallet = Wallet walletId

  -- TODO(SN): Parameterize this
  allWallets = [Wallet 1, Wallet 2, Wallet 3]
  pubKeys = map walletPubKey allWallets

activateContract :: PABContract -> Wallet -> IO ContractInstanceId
activateContract contract wallet =
  retryOnAnyHttpException $ do
    res <-
      runReq defaultHttpConfig $
        req
          POST
          (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "activate")
          (ReqBodyJson reqBody)
          jsonResponse
          (port 8080)
    when (responseStatusCode res /= 200) $
      error "failed to activateContract"
    pure $ responseBody res
 where
  reqBody = ActivateContractRequest (show contract) wallet

-- TODO(SN): use MonadHttp, but clashes with MonadThrow
postInitTx :: ContractInstanceId -> [PubKeyHash] -> IO ()
postInitTx cid pubkeys =
  retryOnAnyHttpException $
    runReq defaultHttpConfig $ do
      res <-
        req
          POST
          (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "instance" /: cidText /: "endpoint" /: "init")
          (ReqBodyJson pubkeys)
          jsonResponse
          (port 8080)
      when (responseStatusCode res /= 200) $
        error "failed to postInitTx"
      pure $ responseBody res
 where
  cidText = show $ unContractInstanceId cid

data ActivateContractRequest = ActivateContractRequest {caID :: Text, caWallet :: Wallet}
  deriving (Generic, ToJSON)

-- TODO(SN): DRY subscribers
initTxSubscriber :: Wallet -> (OnChainTx tx -> IO ()) -> IO ()
initTxSubscriber wallet callback = do
  cid <- unContractInstanceId <$> activateContract WatchInit wallet
  runClient "127.0.0.1" 8080 ("/ws/" <> show cid) $ \con -> forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right (NewObservableState val) -> do
        case fromJSON val of
          Error err -> say $ "decoding error json: " <> show err
          Success res -> case getLast res of
            Nothing -> pure ()
            Just (parties :: [Party]) -> do
              say $ "Observed Init tx with datums (parties): " ++ show parties
              -- TODO(SN): pack hydra verification keys into metadata and callback with these
              callback $ InitTx parties
      Right _ -> pure ()
      Left err -> say $ "error decoding msg: " <> show err

utxoSubscriber :: Wallet -> IO ()
utxoSubscriber wallet = do
  cid <- unContractInstanceId <$> activateContract GetUtxos wallet
  runClient "127.0.0.1" 8080 ("/ws/" <> show cid) $ \con -> forever $ do
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
