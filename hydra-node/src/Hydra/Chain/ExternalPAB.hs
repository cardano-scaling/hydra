{-# LANGUAGE EmptyDataDeriving #-}
module Hydra.Chain.ExternalPAB where

import Hydra.Prelude

import Hydra.Chain (Chain (Chain, postTx))
import Hydra.HeadLogic (OnChainTx (InitTx))
import Hydra.Ledger (Tx)
import Hydra.Logging (Tracer)
import Network.HTTP.Req (POST (..), ReqBodyJson (..), defaultHttpConfig, http, jsonResponse, port, req, responseBody, responseStatusCode, runReq, (/:))
import Wallet.Emulator.Types (Wallet (..))
import Ledger.Value     as Value
import Data.Aeson (ToJSON, eitherDecodeStrict, Result (Error, Success))
import Network.WebSockets (receiveData)
import qualified Data.Map as Map
import Ledger (txOutTxOut, TxOut (txOutValue))
import Control.Monad.Class.MonadSay (say)
import Wallet.Types (unContractInstanceId)
import Network.WebSockets.Client (runClient)
import Ledger.AddressMap (UtxoMap)
import Plutus.PAB.Webserver.Types (InstanceStatusToClient(NewObservableState))
import Data.Aeson.Types (fromJSON)

data ExternalPABLog
  deriving (Eq, Show)

-- TODO(SN): Parameterize with nodeId
withExternalPAB ::
  Tx tx =>
  Tracer IO ExternalPABLog ->
  (OnChainTx tx -> IO ()) ->
  (Chain tx IO -> IO a) ->
  IO a
withExternalPAB _tracer _callback action = do
  withAsync (utxoSubscriber $ Wallet 1) $ \_ ->
    action $ Chain{postTx}
 where
  postTx = \case
    InitTx _ -> loadCid >>= postInitTx
    tx -> error $ "should post " <> show tx

  -- TODO(SN): don't use /tmp
  loadCid = readFileText "/tmp/W1.cid"

-- TODO(SN): use MonadHttp, but clashes with MonadThrow
postInitTx :: Text -> IO ()
postInitTx cid = do
  runReq defaultHttpConfig $ do
    res <-
      req
        POST
        (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "instance" /: cid /: "endpoint" /: "init")
        (ReqBodyJson ())
        jsonResponse
        (port 8080)
    when (responseStatusCode res /= 200) $
      error "failed to postInitTx"
    pure $ responseBody res

data ActivateContractRequest = ActivateContractRequest { caID :: Text , caWallet :: Wallet }
  deriving (Generic, ToJSON)

utxoSubscriber :: Wallet -> IO ()
utxoSubscriber wallet = do
  res <- runReq defaultHttpConfig $ req
      POST
      (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "activate")
      (ReqBodyJson reqBody)
      jsonResponse
      (port 8080)
  when (responseStatusCode res /= 200) $
    error "failed to activateContract"
  let cid = unContractInstanceId $ responseBody res
  say $ "activated: " <> show cid
  runClient "127.0.0.1" 8080 ("/ws/" <> show cid) $ \con -> forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right (NewObservableState val) ->
        case fromJSON val of
          Error err -> error $ "decoding error json: " <> show err
          Success (utxos :: UtxoMap) -> do
            let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
            say $ "own funds: " ++ show (flattenValue v)
      Right _ -> pure ()
      Left err -> error $ "error decoding msg: " <> show err
 where
  reqBody = ActivateContractRequest "GetUtxos" wallet
