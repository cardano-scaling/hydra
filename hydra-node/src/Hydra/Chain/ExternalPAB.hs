{-# LANGUAGE EmptyDataDeriving #-}
module Hydra.Chain.ExternalPAB where

import Hydra.Prelude

import Hydra.Chain (Chain (Chain, postTx))
import Hydra.HeadLogic (OnChainTx (InitTx))
import Hydra.Ledger (Tx)
import Hydra.Logging (Tracer)
import Network.HTTP.Req (POST (..), ReqBodyJson (..), defaultHttpConfig, http, jsonResponse, port, req, responseBody, responseStatusCode, runReq, (/:))
import Wallet.Emulator.Types (Wallet (..))
import Plutus.Contract (ContractInstanceId)
import Data.Aeson (ToJSON)

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
  getUtxoWebsocket (Wallet 1)
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

getUtxoWebsocket :: Wallet -> IO ()
getUtxoWebsocket wallet = do
  res <- runReq defaultHttpConfig $ req
      POST
      (http "127.0.0.1" /: "api" /: "new" /: "contract" /: "activate")
      (ReqBodyJson reqBody)
      jsonResponse
      (port 8080)
  when (responseStatusCode res /= 200) $
    error "failed to activateContract"
  let cid = responseBody res :: ContractInstanceId
  putText $ "activated: " <> show cid
 where
  reqBody = ActivateContractRequest "GetUtxos" wallet
