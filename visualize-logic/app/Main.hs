-- | hydra-vis: interactive HeadLogic visualizer.
--
-- Modes:
--
-- * @--smoke@ steps a tiny scripted SimpleTx scenario and prints each
--   @(input, outcome)@ pair as JSON. Use this to sanity-check the library
--   wiring before reaching for the UI.
-- * @--gen-db PATH@ writes a sample SimpleTx SQLite event log to PATH.
-- * @--db PATH@ loads a SQLite event log and serves the interactive UI on
--   @http://localhost:PORT@ (default port 8080). The Tx instance is chosen
--   with @--cardano@ (default: real Hydra node databases) or @--simple@
--   (sample databases produced by @--gen-db@).
module Main where

import Hydra.Prelude

import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Chain.Direct.State (ChainStateAt (..))
import Hydra.HeadLogic.StateEvent qualified
import Hydra.Ledger.Simple (SimpleChainState (..))
import Hydra.Ledger.Simple qualified
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (initNodeState)
import Hydra.Node.UnsyncedPeriod (defaultUnsyncedPeriodFor)
import HydraVis.History (loadEventsAfter, loadHistoryFor)
import HydraVis.Multi (PartySpec (..), mkMultiModel)
import HydraVis.Sample (sampleEnvironment, sampleLedger, sampleOnInitTx, sampleTick)
import HydraVis.SampleDb (writeSampleDb)
import HydraVis.Server (runMultiServer, runServer)
import HydraVis.Smoke (runSmoke)
import HydraVis.UI (AuthoringCtx (..))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  flag,
  flag',
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  showDefault,
  strOption,
  switch,
  value,
 )
import Test.Hydra.Tx.Fixture (
  alice,
  aliceSk,
  bob,
  bobSk,
  carol,
  carolSk,
  cperiod,
  deriveOnChainId,
 )

data Mode
  = Smoke
  | GenDb {dbPath :: FilePath}
  | FollowCheck {dbPath :: FilePath}
  | Simulate {port :: Int}
  | Serve
      { port :: Int
      , source :: Source
      , txKind :: TxKind
      , startAt :: Maybe Int
      , followFlag :: Bool
      }
  deriving stock (Show)

data Source
  = NoSource
  | FromDb {dbPath :: FilePath}
  deriving stock (Show)

data TxKind = Cardano | Simple
  deriving stock (Show)

modeParser :: Parser Mode
modeParser =
  flag' Smoke (long "smoke" <> help "Print a scripted SimpleTx scenario to stdout")
    <|> ( GenDb
            <$> strOption
              ( long "gen-db"
                  <> metavar "PATH"
                  <> help "Write a sample SimpleTx SQLite event log to PATH and exit"
              )
        )
    <|> ( FollowCheck
            <$> strOption
              ( long "follow-check"
                  <> metavar "PATH"
                  <> help "One-shot poll: report all SimpleTx events in PATH and exit"
              )
        )
    <|> ( Simulate
            <$> option
              auto
              ( long "simulate"
                  <> metavar "PORT"
                  <> help "Run a 3-party SimpleTx simulation UI on PORT"
              )
        )
    <|> ( Serve
            <$> option
              auto
              ( long "port"
                  <> metavar "PORT"
                  <> value 8080
                  <> showDefault
                  <> help "Port to serve the UI on"
              )
            <*> sourceParser
            <*> flag
              Cardano
              Simple
              ( long "simple"
                  <> help "Interpret the database as SimpleTx (default: Cardano Tx)"
              )
            <*> optional
              ( option
                  auto
                  ( long "start-at"
                      <> metavar "N"
                      <> help "Initial cursor position (zero-based index into the event log)"
                  )
              )
            <*> switch
              ( long "follow"
                  <> help "Poll the event log for newly persisted rows (live mode)"
              )
        )

sourceParser :: Parser Source
sourceParser =
  ( FromDb
      <$> strOption
        ( long "db"
            <> metavar "PATH"
            <> help "SQLite event log produced by a Hydra node"
        )
  )
    <|> pure NoSource

opts :: ParserInfo Mode
opts =
  info
    (helper <*> modeParser)
    (fullDesc <> progDesc "Interactive HeadLogic visualizer")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  mode <- execParser opts
  case mode of
    Smoke -> runSmoke
    GenDb{dbPath} -> do
      writeSampleDb dbPath
      putTextLn $ "wrote sample event log to " <> toText dbPath
    FollowCheck{dbPath} -> do
      events <- loadEventsAfter dbPath Nothing :: IO [Hydra.HeadLogic.StateEvent.StateEvent Hydra.Ledger.Simple.SimpleTx]
      putTextLn $ "found " <> show (length events) <> " events in " <> toText dbPath
      forM_ events $ \e ->
        putTextLn $
          "  eventId="
            <> show (Hydra.HeadLogic.StateEvent.eventId e)
            <> " time="
            <> show (Hydra.HeadLogic.StateEvent.time e)
    Simulate{port} -> do
      let initialState = initNodeState (SimpleChainState (ChainSlot 0))
          envFor self others sk =
            Environment
              { party = self
              , signingKey = sk
              , otherParties = others
              , contestationPeriod = cperiod
              , depositPeriod = DepositPeriod 20
              , unsyncedPeriod = defaultUnsyncedPeriodFor cperiod
              , participants = deriveOnChainId <$> [alice, bob, carol]
              , configuredPeers = ""
              }
          mkSpec self sk peers =
            PartySpec
              { specEnvironment = envFor self peers sk
              , specLedger = sampleLedger
              , specPresets = [("Tick (next slot)", sampleTick), ("OnInitTx", sampleOnInitTx)]
              , specInitialState = initialState
              }
          mm =
            mkMultiModel
              [ mkSpec alice aliceSk [bob, carol]
              , mkSpec bob bobSk [alice, carol]
              , mkSpec carol carolSk [alice, bob]
              ]
      runMultiServer port mm
    Serve{port, source, txKind = Simple, startAt, followFlag} -> do
      let initial = initNodeState (SimpleChainState (ChainSlot 0))
          (path, steps0) = case source of
            NoSource -> (Nothing, pure [])
            FromDb{dbPath} -> (Just dbPath, loadHistoryFor initial dbPath)
          authoring =
            Just
              AuthoringCtx
                { ctxEnvironment = sampleEnvironment
                , ctxLedger = sampleLedger
                , ctxPresets =
                    [ ("Tick (next slot)", sampleTick)
                    , ("OnInitTx", sampleOnInitTx)
                    ]
                }
      steps <- steps0
      runServer port authoring path followFlag initial (fromMaybe 0 startAt) steps
    Serve{port, source, txKind = Cardano, startAt, followFlag} -> do
      let initial = initNodeState ChainStateAt{spendableUTxO = mempty, recordedAt = Nothing}
          (path, steps0) = case source of
            NoSource -> (Nothing, pure [])
            FromDb{dbPath} -> (Just dbPath, loadHistoryFor initial dbPath)
      steps <- steps0
      runServer port Nothing path followFlag initial (fromMaybe 0 startAt) steps
