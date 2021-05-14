module Hydra.MockRepl where

import Cardano.Prelude hiding (try, (<|>))

import Data.String (String)
import Hydra.Ledger.MockTx
import Hydra.Logic (ClientRequest (..), Party (..))
import Hydra.Node (HydraNode, handleClientRequest)
import System.Console.Repline (CompleterStyle (Word0), ExitDecision (Exit), evalRepl)
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token (integer, makeTokenParser)

startHydraRepl :: HydraNode MockTx IO -> IO ()
startHydraRepl node = link =<< async runRepl
 where
  runRepl = evalRepl (const $ pure prompt) replCommand [] Nothing Nothing (Word0 replComplete) replInit (pure Exit)

  prompt = ">>> "

  -- TODO(SN): avoid redundancy
  commands = ["init", "commit", "newtx", "close", "contest"]

  replCommand c =
    case parseCommand c of
      Right com -> liftIO $ handleClientRequest node com
      Left err -> liftIO $ putText $ err <> ", unknown command, use any of: " <> show commands

  replComplete n = pure $ filter (n `isPrefixOf`) commands

  replInit = liftIO $ putText "Welcome to the Hydra Node REPL, you can even use tab completion! (Ctrl+D to exit)"

parseCommand :: String -> Either Text (ClientRequest MockTx)
parseCommand = first show . runParser commandParser () ""

commandParser :: Parsec String () (ClientRequest MockTx)
commandParser = initParser <|> try commitParser <|> newTxParser <|> closeParser
 where
  haskellTokens = makeTokenParser haskellDef

  initParser = string "init" *> spaces *> (Init <$> many1 party)

  party = Party . fromInteger <$> integer haskellTokens

  commitParser = string "commit" *> spaces $> Commit

  newTxParser = string "newtx" *> spaces *> (NewTx . ValidTx <$> txId)

  txId = integer haskellTokens

  closeParser = string "close" *> spaces $> Close
