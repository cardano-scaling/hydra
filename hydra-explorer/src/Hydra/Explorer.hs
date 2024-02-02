module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), Tx, unFile)
import Hydra.Chain (OnChainTx)
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.Chain.Direct.Tx (
  HeadObservation (..),
 )
import Hydra.Explorer.ExplorerState (ExplorerState, HeadState, aggregateOnChainTx)
import Hydra.Explorer.Options (Options (..), hydraExplorerOptions)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Network (Host (..))
import Hydra.Node (HydraNodeLog (..))
import Hydra.Options qualified as Options
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental, loadAll)
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative (execParser)
import Servant (Server, throwError)
import Servant.API (Get, Header, JSON, addHeader, (:>))
import Servant.API.ResponseHeaders (Headers)
import Servant.Server (Application, Handler, err500, serve)
import System.Environment (withArgs)

type API =
  "heads"
    :> Get
        '[JSON]
        ( Headers
            '[ Header "Accept" String
             , Header "Access-Control-Allow-Origin" String
             , Header "Access-Control-Allow-Methods" String
             , Header "Access-Control-Allow-Headers" String
             ]
            [HeadState]
        )

type GetHeads = IO [HeadState]

explorerAPI :: Proxy API
explorerAPI = Proxy

server :: GetHeads -> Server API
server = handleGetHeads

handleGetHeads ::
  GetHeads ->
  Handler
    ( Headers
        '[ Header "Accept" String
         , Header "Access-Control-Allow-Origin" String
         , Header "Access-Control-Allow-Methods" String
         , Header "Access-Control-Allow-Headers" String
         ]
        [HeadState]
    )
handleGetHeads getHeads = do
  result <- liftIO $ try getHeads
  case result of
    Right heads -> do
      return $ addHeader "application/json" $ addCorsHeaders heads
    Left (_ :: SomeException) -> throwError err500

logMiddleware :: Tracer IO APIServerLog -> Middleware
logMiddleware tracer app' req sendResponse = do
  liftIO $
    traceWith tracer $
      APIHTTPRequestReceived
        { method = Method $ requestMethod req
        , path = PathInfo $ rawPathInfo req
        }
  app' req sendResponse

httpApp :: Tracer IO APIServerLog -> GetHeads -> Application
httpApp tracer getHeads =
  logMiddleware tracer $ serve explorerAPI $ server getHeads

observerHandler ::
  TVar IO ExplorerState ->
  PersistenceIncremental (OnChainTx Tx) IO ->
  [HeadObservation] ->
  IO ()
observerHandler explorerState PersistenceIncremental{append} observations = do
  let onChainTxs = mapMaybe convertObservation observations
  forM_ onChainTxs append
  atomically $
    modifyTVar' explorerState $ \currentState ->
      foldl' aggregateOnChainTx currentState onChainTxs

readModelGetHeadIds :: TVar IO ExplorerState -> GetHeads
readModelGetHeadIds = readTVarIO

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \(tracer :: Tracer IO (HydraLog Tx ())) -> do
    opts <- execParser hydraExplorerOptions
    let Options
          { networkId
          , host = Host{hostname, port}
          , nodeSocket
          , startChainFrom
          , persistenceDir
          } = opts
    persistence <- createPersistenceIncremental $ persistenceDir <> "/explorer-state"
    explorerState <- do
      let nodeTracer = contramap Node tracer
      events <- loadAll persistence
      traceWith nodeTracer LoadedState{numberOfEvents = fromIntegral $ length events}
      let initialState = mempty
          recoveredSt = foldl' aggregateOnChainTx initialState events
      newTVarIO recoveredSt

    let getHeads = readModelGetHeadIds explorerState

        apiTracer = contramap APIServer tracer

        chainObserverArgs =
          ["--node-socket", unFile nodeSocket]
            <> case networkId of
              Mainnet -> ["--mainnet"]
              Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]
            <> Options.toArgStartChainFrom startChainFrom
    race
      ( withArgs chainObserverArgs $
          Hydra.ChainObserver.main (observerHandler explorerState persistence)
      )
      ( traceWith apiTracer (APIServerStarted port)
          *> Warp.runSettings (settings apiTracer port hostname) (httpApp apiTracer getHeads)
      )
      >>= \case
        Left{} -> error "Something went wrong"
        Right a -> pure a
 where
  settings tracer port hostname =
    Warp.defaultSettings
      & Warp.setPort (fromIntegral port)
      & Warp.setHost (fromString . toString $ hostname)
      & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})

addCorsHeaders ::
  a ->
  Headers
    [ Header "Access-Control-Allow-Origin" String
    , Header "Access-Control-Allow-Methods" String
    , Header "Access-Control-Allow-Headers" String
    ]
    a
addCorsHeaders = addHeader "*" . addHeader "*" . addHeader "*"
