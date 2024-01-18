module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

-- XXX: Depending on hydra-node will be problematic to support versions
import Hydra.HeadId (HeadId)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Network (PortNumber)

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Cardano.Api (TxIn, TxOut)
import Hydra.Cardano.Api.Prelude (CtxUTxO)
import Hydra.Chain.Direct.Tx (HeadObservation (..))
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party)
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

data PartyCommit = PartyCommit
  { txIn :: TxIn
  , txOut :: TxOut CtxUTxO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HeadMember = HeadMember
  { party :: Party
  , onChainId :: OnChainId
  , commits :: [PartyCommit]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HeadStatus
  = Initializing
  | Aborted
  | Open
  | Closed
  | FanoutPossible
  | Finalized
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HeadState = HeadState
  { headId :: HeadId
  , seedTxIn :: TxIn
  , status :: HeadStatus
  , contestationPeriod :: ContestationPeriod
  , members :: [HeadMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type ExplorerState = [HeadState]

aggregateHeadObservations :: [HeadObservation] -> ExplorerState -> ExplorerState
aggregateHeadObservations = undefined

observerHandler :: TVar IO ExplorerState -> [HeadObservation] -> IO ()
observerHandler explorerState observations = do
  atomically $
    modifyTVar' explorerState $
      aggregateHeadObservations observations

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    explorerState <- newTVarIO (mempty :: ExplorerState)
    let getHeads = readModelGetHeadIds explorerState
    args <- getArgs
    race
      -- FIXME: this is going to be problematic on mainnet.
      (withArgs (args <> ["--start-chain-from", "0"]) $ Hydra.ChainObserver.main (observerHandler explorerState))
      ( traceWith tracer (APIServerStarted (fromIntegral port :: PortNumber))
          *> Warp.runSettings (settings tracer) (httpApp tracer getHeads)
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

  readModelGetHeadIds :: TVar IO ExplorerState -> GetHeads
  readModelGetHeadIds = readTVarIO

type GetHeads = IO [HeadState]

httpApp :: Tracer IO APIServerLog -> GetHeads -> Application
httpApp tracer getHeads req send = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod req
      , path = PathInfo $ rawPathInfo req
      }
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> send $ responseLBS status200 corsHeaders ""
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["heads"]) -> handleGetHeads getHeads req send
    -- FIXME: do proper file serving, this is dangerous
    ("GET", path) -> send $ handleFile $ toString $ mconcat $ List.intersperse "/" ("." : path)
    (_, _) -> send handleNotFound

handleGetHeads ::
  -- | Read model of all known head ids
  GetHeads ->
  Application
handleGetHeads getHeads _req send = do
  headIds <- getHeads
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
