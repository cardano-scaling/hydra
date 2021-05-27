{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Prelude hiding (Option, option)

import Control.Concurrent.STM (newBroadcastTChanIO, writeTChan)
import Data.IP (IP)
import Hydra.API.Server (APIServerLog, runAPIServer)
import Hydra.Ledger.Mock (MockTx)
import qualified Hydra.Ledger.Mock as Ledger
import Hydra.Logging
import Hydra.Logic (
  Environment (..),
  Event (..),
  HeadParameters (..),
  SnapshotStrategy (..),
  createHeadState,
 )
import Hydra.MockZMQChain (MockChainLog)
import Hydra.Network.ZeroMQ (
  NetworkLog,
  withZeroMQHydraNetwork,
 )
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  HydraNodeLog,
  createEventQueue,
  createHydraHead,
  createMockChainClient,
  runHydraNode,
 )
import Options.Applicative (Parser, ParserInfo, auto, execParser, flag, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, value)

data Option = Option
  { verbosity :: Verbosity
  , nodeId :: Natural
  , host :: IP
  }
  deriving (Show)

hydraNodeParser :: Parser Option
hydraNodeParser =
  Option
    <$> verbosityParser
    <*> nodeIdParser
    <*> hostParser

nodeIdParser :: Parser Natural
nodeIdParser =
  option
    auto
    ( long "node-id"
        <> short 'n'
        <> metavar "INTEGER"
        <> help "Sets this node's id"
    )

verbosityParser :: Parser Verbosity
verbosityParser =
  flag
    (Verbose "HydraNode")
    Quiet
    ( long "quiet"
        <> short 'q'
        <> help "Turns off any logging"
    )

hostParser :: Parser IP
hostParser =
  option
    auto
    ( long "host"
        <> short 'h'
        <> value "127.0.0.1"
        <> metavar "IP"
        <> help "The address this node listens on (default: 127.0.0.1)"
    )

hydraNodeOptions :: ParserInfo Option
hydraNodeOptions =
  info
    (hydraNodeParser <**> helper)
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - A prototype of Hydra Head protocol"
    )

data HydraLog
  = MockChain MockChainLog
  | APIServer APIServerLog
  | Network NetworkLog
  | Node (HydraNodeLog MockTx)
  deriving (Show)

main :: IO ()
main = do
  Option{nodeId, verbosity, host} <- identifyNode <$> execParser hydraNodeOptions
  withTracer verbosity show $ \tracer -> do
    let env = Environment nodeId
    eq <- createEventQueue
    let headState = createHeadState [] HeadParameters SnapshotStrategy
    hh <- createHydraHead headState Ledger.mockLedger
    oc <- createMockChainClient eq (contramap MockChain tracer)
    withZeroMQHydraNetwork (me nodeId host) (them nodeId host) (contramap Network tracer) (putEvent eq . NetworkEvent) $ \hn -> do
      responseChannel <- newBroadcastTChanIO
      let sendResponse = atomically . writeTChan responseChannel
      let node = HydraNode{eq, hn, hh, oc, sendResponse, env}
      race_
        (runAPIServer responseChannel node (contramap APIServer tracer))
        (runHydraNode (contramap Node tracer) node)
 where
  me nodeId host = (show host, show $ 5000 + nodeId)
  them nodeId host = [(show host, show $ 5000 + id) | id <- [1 .. 3], id /= nodeId]

identifyNode :: Option -> Option
identifyNode opt@Option{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
