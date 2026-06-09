-- | The smoke-test entry point factored out of the executable so the same
-- code is callable from @--smoke@ and from any future test suite. Drives
-- 'HeadLogic.update' over the canonical 'HydraVis.Sample' script and prints
-- one block of pretty-JSON per step on stdout.
module HydraVis.Smoke (runSmoke) where

import Hydra.Prelude

import Data.ByteString.Lazy.Char8 qualified as BSL
import Hydra.HeadLogic (aggregateState, update)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (Outcome)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node.Environment (Environment (party))
import Hydra.Node.State (NodeState)
import HydraVis.Sample (
  sampleEnvironment,
  sampleInitialState,
  sampleLedger,
  sampleScript,
  sampleStepTime,
 )

runSmoke :: IO ()
runSmoke = do
  putTextLn "hydra-vis smoke test: stepping HeadLogic over sample script"
  putTextLn $ "  party:  " <> show (party sampleEnvironment)
  putTextLn $ "  inputs: " <> show (length sampleScript)
  putTextLn ""
  go 1 sampleInitialState sampleScript

go :: Int -> NodeState SimpleTx -> [Input SimpleTx] -> IO ()
go _ finalState [] = do
  putTextLn "--- final state ---"
  printJson finalState
go n nodeState (i : rest) = do
  let now = sampleStepTime n
      outcome :: Outcome SimpleTx
      outcome = update sampleEnvironment sampleLedger now nodeState i
      nodeState' = aggregateState nodeState outcome
  putTextLn $ "=== step " <> show n <> " ==="
  putTextLn "input:"
  printJson i
  putTextLn "outcome:"
  printJson outcome
  putTextLn ""
  go (n + 1) nodeState' rest

printJson :: ToJSON a => a -> IO ()
printJson = BSL.putStrLn . encodePretty
