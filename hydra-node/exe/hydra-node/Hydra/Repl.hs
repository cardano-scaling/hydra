module Hydra.Repl where

import Cardano.Prelude

import qualified Data.ByteString.Lazy as BSL
import Hydra.Ledger.MaryTest (MaryTestTx, decodeTx)
import Hydra.Logic (ClientRequest (..))
import Hydra.Node (HydraNode, handleClientRequest)
import System.Console.Repline (CompleterStyle (Word0), ExitDecision (Exit), evalRepl)

startHydraRepl :: HydraNode MaryTestTx IO -> IO ()
startHydraRepl node = link =<< async runRepl
 where
  runRepl = evalRepl (const $ pure prompt) replCommand [] Nothing Nothing (Word0 replComplete) replInit (pure Exit)

  prompt = ">>> "

  -- TODO(SN): avoid redundancy
  commands = ["init", "commit", "newtx", "close", "contest"]

  replCommand c
    | c == "init" = liftIO $ handleClientRequest node Init
    | c == "close" = liftIO $ handleClientRequest node Close
    -- c == "commit" =
    | c == "newtx" = liftIO $ do
      loadTx "tx1.cbor" >>= handleClientRequest node . NewTx
    -- c == "contest" =
    | otherwise = liftIO $ putText $ "Unknown command, use any of: " <> show commands

  replComplete n = pure $ filter (n `isPrefixOf`) commands

  replInit = liftIO $ putText "Welcome to the Hydra Node REPL, you can even use tab completion! (Ctrl+D to exit)"

  loadTx fp = do
    putText $ "Load and decode a tx from " <> show (fp :: FilePath)
    bs <- BSL.readFile fp
    either (\err -> panic $ "Failed to decode tx: " <> show err) pure $ decodeTx bs
