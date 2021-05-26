{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Prelude hiding (Option, option)

import Control.Concurrent.STM (newBroadcastTChanIO, writeTChan)
import Hydra.API.Server (APIServerLog, runAPIServer)
import Hydra.Ledger.Mock (MockTx)
import qualified Hydra.Ledger.Mock as Ledger
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
import Hydra.Logging
import Options.Applicative (Parser, ParserInfo, auto, execParser, flag, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short)

data Option = Option {verbosity :: Verbosity, nodeId :: Natural}
  deriving (Show)

hydraNodeParser :: Parser Option
hydraNodeParser = Option <$> verbosityParser <*> nodeIdParser

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
  Option{nodeId, verbosity} <- identifyNode <$> execParser hydraNodeOptions
  withTracer verbosity show $ \tracer -> do
    let env = Environment nodeId
    eq <- createEventQueue
    let headState = createHeadState [] HeadParameters SnapshotStrategy
    hh <- createHydraHead headState Ledger.mockLedger
    oc <- createMockChainClient eq (contramap MockChain tracer)
    withZeroMQHydraNetwork (me nodeId) (them nodeId) (contramap Network tracer) (putEvent eq . NetworkEvent) $ \hn -> do
      responseChannel <- newBroadcastTChanIO
      let sendResponse = atomically . writeTChan responseChannel
      let node = HydraNode{eq, hn, hh, oc, sendResponse, env}
      race_
        (runAPIServer responseChannel node (contramap APIServer tracer))
        (runHydraNode node (contramap Node tracer))
 where
  -- HACK(SN): Obviously we should configure the node instead
  me nodeId = ("127.0.0.1", show $ 5000 + nodeId)
  them nodeId = [("127.0.0.1", show $ 5000 + id) | id <- [1 .. 3], id /= nodeId]

identifyNode :: Option -> Option
identifyNode Option{verbosity = Verbose "HydraNode", nodeId} = Option (Verbose $ "HydraNode-" <> show nodeId) nodeId
identifyNode opt = opt
