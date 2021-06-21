module Hydra.Chain.ExternalPAB where

import Hydra.Prelude

import Hydra.Chain (Chain (Chain, postTx))
import Hydra.HeadLogic (OnChainTx (InitTx))
import Hydra.Ledger (Tx)
import Hydra.Logging (Tracer)
import Network.HTTP.Req (POST (..), ReqBodyJson (..), defaultHttpConfig, http, jsonResponse, port, req, responseBody, responseStatusCode, runReq, (/:))

data ExternalPABLog

withExternalPAB ::
  Tx tx =>
  Tracer IO ExternalPABLog ->
  (OnChainTx tx -> IO ()) ->
  (Chain tx IO -> IO a) ->
  IO a
withExternalPAB _tracer _callback action = do
  action $ Chain{postTx}
 where
  postTx = \case
    InitTx _ -> loadCid >>= postInitTx
    tx -> error $ "should post " <> show tx

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
