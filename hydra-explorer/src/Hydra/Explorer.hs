module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

-- XXX: Depending on hydra-node will be problematic to support versions
import Hydra.HeadId (HeadId)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Network (PortNumber)

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO)
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Cardano.Api (ChainPoint, TxId)
import Hydra.Chain.Direct.Tx (HeadObservation)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai (
  Application,
  Response,
  pathInfo,
  rawPathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (withArgs)

type ExplorerState = Map ChainPoint [(TxId, HeadObservation)]

observerHandler :: TVar IO ExplorerState -> ChainPoint -> [(TxId, HeadObservation)] -> IO ()
observerHandler explorerState point observations =
  atomically $
    modifyTVar' explorerState (Map.insert point observations)

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    explorerState <- newTVarIO (mempty :: ExplorerState)
    getHeadIds <- getHeadIdsReadModel explorerState
    args <- getArgs
    race
      -- FIXME: this is going to be problematic on mainnet.
      (withArgs (args <> ["--start-chain-from", "0"]) $ Hydra.ChainObserver.main (observerHandler explorerState))
      ( traceWith tracer (APIServerStarted (fromIntegral port :: PortNumber))
          *> Warp.runSettings (settings tracer) (httpApp tracer getHeadIds)
      )
      >>= \case
        Left{} -> error "Something went wrong"
        Right a -> pure a
 where
  port = 9090

  settings tracer =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "0.0.0.0"
      & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

  getHeadIdsReadModel :: TVar IO ExplorerState -> IO GetHeadIds
  getHeadIdsReadModel tv = atomically $ do
    currentState <- readTVar tv
    pure $ pure []

type GetHeadIds = IO [HeadId]

httpApp :: Tracer IO APIServerLog -> GetHeadIds -> Application
httpApp tracer getHeadIds req send = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod req
      , path = PathInfo $ rawPathInfo req
      }
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> send $ responseLBS status200 corsHeaders ""
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["heads"]) -> handleGetHeads getHeadIds req send
    -- FIXME: do proper file serving, this is dangerous
    ("GET", path) -> send $ handleFile $ toString $ mconcat $ List.intersperse "/" ("." : path)
    (_, _) -> send handleNotFound

handleGetHeads ::
  -- | Read model of all known head ids
  GetHeadIds ->
  Application
handleGetHeads getHeadIds _req send = do
  headIds <- getHeadIds
  send . responseLBS status200 corsHeaders $ Aeson.encode headIds

handleError :: Response
handleError =
  responseLBS status500 corsHeaders "INVALID REQUEST"

handleNotFound :: Response
handleNotFound =
  responseLBS status404 corsHeaders "NOT FOUND"

handleFile :: FilePath -> Response
handleFile filepath = responseFile status200 corsHeaders filepath Nothing

corsHeaders :: [(HeaderName, ByteString)]
corsHeaders =
  [ ("Access-Control-Allow-Origin", "*")
  , ("Access-Control-Allow-Methods", "*")
  , ("Access-Control-Allow-Headers", "*")
  ]
