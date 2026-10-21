{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitPrelude #-}

-- | Run in the tx-cost benchmark's GHCi environment; see benchmarks/profiling.md.
module ProfileIncrement where

import Cardano.Ledger.Alonzo.Plutus.Evaluate (collectPlutusScriptsWithContext)
import Cardano.Ledger.Binary (getVersion)
import Cardano.Ledger.Plutus qualified as Ledger
import Control.Monad (unless, when)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hydra.Cardano.Api
import Hydra.Chain.Direct.State (getKnownUTxO)
import Hydra.Contract.Head qualified as Head
import PlutusLedgerApi.Common qualified as Plutus
import System.Environment (getArgs)
import Test.Hydra.Chain.Direct.State (genIncrementTx, pickChainContext)
import Test.Hydra.Ledger.Cardano.Fixtures (epochInfo, pparams, systemStart)
import Test.Hydra.Prelude (Gen, generateWith)
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek
import Prelude

main :: IO ()
main = do
  args <- getArgs
  output <- case args of
    [path] -> pure path
    _ -> fail "Usage: :main <budget-trace.csv>"
  let (tx, utxo) = generateWith fixture 42
      headHash = toShelleyScriptHash $ hashScript $ PlutusScript Head.validatorScript
  contexts <-
    either (fail . show) pure $
      collectPlutusScriptsWithContext epochInfo systemStart pparams (toLedgerTx tx) (toLedgerUTxO ShelleyBasedEraConway utxo)
  case filter ((== headHash) . Ledger.pwcScriptHash) contexts of
    [context] -> profile output context
    _ -> fail "Expected exactly one Head validator"
 where
  fixture :: Gen (Tx, UTxO)
  fixture = do
    (ctx, st, spendable, tx) <- genIncrementTx 5
    chainContext <- pickChainContext ctx
    pure (tx, getKnownUTxO st <> getKnownUTxO chainContext <> spendable)

profile :: FilePath -> Ledger.PlutusWithContext -> IO ()
profile output context@Ledger.PlutusWithContext{..} = do
  script <- either (fail . show) pure $ Ledger.plutusRunnableResult pwcScript
  term <- either (fail . show) pure $ Ledger.mkTermToEvaluate pwcProtocolVersion script pwcArgs
  let evaluationContext = Ledger.getEvaluationContext pwcCostModel
      (_, validation) = Ledger.evaluatePlutusRunnableBudget pwcProtocolVersion Plutus.Quiet evaluationContext script pwcArgs
  validatedBudget <- either (fail . show) pure validation
  let parameters = Plutus.toMachineParameters (Plutus.MajorProtocolVersion $ getVersion pwcProtocolVersion) evaluationContext
      Cek.CekReport result (Cek.CountingSt budget) logs = Cek.runCekDeBruijn parameters Cek.counting Cek.logWithBudgetEmitter term
  _ <- either (fail . show) pure $ Cek.cekResultToEither result
  unless (budget == validatedBudget) $ fail "Profile and ledger evaluator budgets differ"
  when (null logs) $ fail "No profiling traces; enable Plinth.Plugin:profile-all in Hydra.Contract.Head"
  -- traceToStacks expects each function label to contain no spaces.
  -- Keep source locations, but join them to the function name.
  Text.writeFile output $ Text.unlines $ map (Text.replace " (" "(") logs
  putStrLn $ "Script: " <> show (Ledger.pwcScriptHash context)
  putStrLn $ "Protocol version: " <> show pwcProtocolVersion
  putStrLn $ "Budget: " <> show budget
  putStrLn $ "Trace rows: " <> show (length logs)
