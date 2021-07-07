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
import Ledger (PubKeyHash, TxOut (txOutValue), txOutTxOut)
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
import Wallet.Emulator.Types (Wallet (..))
import Wallet.Types (ContractInstanceId, unContractInstanceId)

data ExternalPABLog = ExternalPABLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withExternalPAB ::
  Tx tx =>
  Tracer IO ExternalPABLog ->
  (OnChainTx tx -> IO ()) ->
  (Chain tx IO -> IO a) ->
  IO a
withExternalPAB _tracer callback action = do
  withAsync (utxoSubscriber wallet) $ \_ -> do
    action $ Chain{postTx = postTx hydraCid}
 where
  postTx cid = \case
    InitTx parties -> postInitTx cid parties
    tx -> error $ "should post " <> show tx

  -- TODO(SN): Parameterize with nodeId
  wallet = Wallet 1

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
postInitTx :: ContractInstanceId -> [Party] -> IO ()
postInitTx cid parties =
  retryOnAnyHttpException $
    runReq defaultHttpConfig $ do
      res <-
        req
          POST
          (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "instance" /: cidText /: "endpoint" /: "init")
          (ReqBodyJson parties)
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
initTxSubscriber :: Wallet -> AssetClass -> (OnChainTx tx -> IO ()) -> IO ()
initTxSubscriber wallet token callback = do
  cid <- unContractInstanceId <$> activateContract (WatchInit token) wallet
  runClient "127.0.0.1" 8080 ("/ws/" <> show cid) $ \con -> forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right (NewObservableState val) -> do
        case fromJSON val of
          Error err -> say $ "decoding error json: " <> show err
          Success res -> case getLast res of
            Nothing -> pure ()
            Just (pubKeyHashes :: [PubKeyHash]) -> do
              say $ "Observed Init tx with datums (pubkeyhashes): " ++ show pubKeyHashes
              -- TODO(SN): pack hydra verification keys into metadata and callback with these
              callback $ InitTx mempty
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
