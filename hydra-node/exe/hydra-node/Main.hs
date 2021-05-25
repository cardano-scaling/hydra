{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Prelude hiding (Option, option)

import Control.Concurrent.STM (newBroadcastTChanIO, writeTChan)
import Hydra.API.Server (runAPIServer)
import qualified Hydra.Ledger.Mock as Ledger
import Hydra.Logic (
  Environment (..),
  Event (..),
  HeadParameters (..),
  SnapshotStrategy (..),
  createHeadState,
 )
import Hydra.Network.ZeroMQ (
  withZeroMQHydraNetwork,
 )
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  createEventQueue,
  createHydraHead,
  createMockChainClient,
  runHydraNode,
 )
import Logging
import Options.Applicative (Parser, ParserInfo, auto, execParser, flag, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short)

data Option = Option {verbosity :: Verbosity, nodeId :: Natural}
  deriving (Show)

hydraNodeParser :: Parser Option
hydraNodeParser =
  Option
    <$> flag
      (Verbose "HydraNode")
      Quiet
      ( long "quiet"
          <> short 'q'
          <> help "Turns off any logging"
      )
      <*> option
        auto
        ( long "node-id"
            <> short 'n'
            <> metavar "INTEGER"
            <> help "Sets this node's id"
        )

hydraNodeOptions :: ParserInfo Option
hydraNodeOptions =
  info
    (hydraNodeParser <**> helper)
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - A prototype of Hydra Head protocol"
    )

main :: IO ()
main = do
  Option{nodeId, verbosity} <- execParser hydraNodeOptions
  withTracer verbosity show $ \tracer -> do
    let env = Environment nodeId
    eq <- createEventQueue
    let headState = createHeadState [] HeadParameters SnapshotStrategy
    hh <- createHydraHead headState Ledger.mockLedger
    oc <- createMockChainClient env eq tracer
    withZeroMQHydraNetwork (me nodeId) (them nodeId) (putEvent eq . NetworkEvent) $ \hn -> do
      responseChannel <- newBroadcastTChanIO
      let sendResponse = atomically . writeTChan responseChannel
      let node = HydraNode{eq, hn, hh, oc, sendResponse, env}
      race_
        (runAPIServer responseChannel node)
        (runHydraNode node)
 where
  -- HACK(SN): Obviously we should configure the node instead
  me nodeId = ("127.0.0.1", show $ 5000 + nodeId)
  them nodeId = [("127.0.0.1", show $ 5000 + id) | id <- [1 .. 3], id /= nodeId]
