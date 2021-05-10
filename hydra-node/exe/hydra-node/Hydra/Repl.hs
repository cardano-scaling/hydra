module Hydra.Repl where

import Cardano.Prelude

import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Logic (ClientRequest (..))
import Hydra.Node (HydraNode, handleClientRequest)
import System.Console.Repline (CompleterStyle (Word0), ExitDecision (Exit), evalRepl)

startHydraRepl :: HydraNode MaryTest.MaryTestTx IO -> IO ()
startHydraRepl node = link =<< async runRepl
 where
  runRepl = evalRepl (const $ pure prompt) replCommand [] Nothing Nothing (Word0 replComplete) replInit (pure Exit)

  prompt = ">>> "

  -- TODO(SN): avoid redundancy
  commands = ["init", "commit", "newtx", "close", "contest"]

  replCommand c
    | c == "init" = liftIO $ handleClientRequest node Init
    | c == "close" = liftIO $ handleClientRequest node Close
    | c == "commit" = liftIO $ handleClientRequest node Commit
    | c == "newtx" = liftIO $ do
      putText "Imagine a simple payment transaction where Alice gives 5 ADA to bob"
      handleClientRequest node (NewTx MaryTest.txSimpleTransfer)
    | c == "contest" = panic "not implemented"
    | otherwise = liftIO $ putText $ "Unknown command, use any of: " <> show commands

  replComplete n = pure $ filter (n `isPrefixOf`) commands

  replInit = liftIO $ putText "Welcome to the Hydra Node REPL, you can even use tab completion! (Ctrl+D to exit)"
