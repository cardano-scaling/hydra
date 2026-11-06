{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Main where

import Cardano.Binary (decodeFull', serialize')
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Aeson (eitherDecodeFileStrict', encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hydra.HeadLogic (
  CoordinatedHeadState (
    allTxs,
    confirmedSnapshot,
    localTxs,
    localUTxO
  ),
  HeadState (Open),
  OpenState (coordinatedHeadState, headId),
  Outcome (Continue, Error, Wait),
  StateChanged,
  aggregateState,
  onOpenNetworkReqTxProgram,
  ruleEnvironment,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Trace (traceLabel)
import Hydra.Ledger.Simple (SimpleTx (..), SimpleTxOut (..), simpleLedger)
import Hydra.Node.State (NodeState (..))
import Hydra.Prelude hiding (size, state)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import System.Environment (withArgs)
import Test.Hydra.HeadLogic.Crem.Comparison qualified as CremComparison
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.DecommitDependency qualified as DecommitDependency
import Test.Hydra.HeadLogic.Explorer qualified as Explorer
import Test.Hydra.HeadLogic.InputComparison qualified as InputComparison
import Test.Hydra.HeadLogic.MachineDiagrams qualified as MachineDiagrams
import Test.Hydra.HeadLogic.Mermaid qualified as Mermaid
import Test.Hydra.HeadLogic.RegistryDemo qualified as RegistryDemo
import Test.Hydra.HeadLogic.Replay qualified as Replay
import Test.Hydra.HeadLogic.Resolved qualified as Resolved
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.HeadLogic.Settlement qualified as Settlement

main :: IO ()
main =
  getArgs >>= \case
    ["decommit-dependency", file] -> do
      (report, sections) <- demand DecommitDependency.demonstration
      LBS.writeFile file $ encode report
      Text.writeFile (file <> ".mmd") $ Mermaid.renderTraces sections
      putStrLn ("Confirmed the dependent decommit in snapshot 3; every prefix passes the ownership checks." :: String)
    ["decommit-dependency", "--historical", file] -> do
      (report, sections) <- demand DecommitDependency.historicalDemonstration
      LBS.writeFile file $ encode report
      Text.writeFile (file <> ".mmd") $ Mermaid.renderTraces sections
      putStrLn ("Reproduced the reservation loss and snapshot-2 stall using frozen rules and replay." :: String)
    ["trace-inputs", directory] -> InputComparison.writeDiagrams directory
    ["compare-crem", directory] -> CremComparison.writeDiagrams directory
    ["machines", directory] -> MachineDiagrams.writeDiagrams directory
    ["settlement", file] -> do
      cases <- forM [(confirmed, slot, recipients) | confirmed <- [False, True], slot <- [2, 3], recipients <- permutations [0, 1, 2]] $ \(confirmed, slot, recipients) -> do
        steps <- demand $ Settlement.settlementSequence confirmed slot recipients
        pure $ object ["confirmedBeforeRollback" .= confirmed, "rollbackSlot" .= slot, "reobservationOrder" .= recipients, "steps" .= steps]
      LBS.writeFile file $ encode $ object ["schemaVersion" .= (1 :: Int), "cases" .= cases]
      steps <- demand $ Settlement.settlementSequence True 2 [0, 1, 2]
      Text.writeFile (file <> ".mmd") $ Mermaid.renderTimeline [("Party " <> show step.recipient, step.label) | step <- steps]
      putStrLn ("All 24 settlement schedules passed, including event and checkpoint replay." :: String)
    ["registry-demo", file] -> do
      (report, sections) <- demand RegistryDemo.registryDemo
      LBS.writeFile file $ encode report
      Text.writeFile (file <> ".mmd") $ Mermaid.renderTraces sections
      forM_ sections $ \(title, observations) -> mapM_ Text.putStrLn $ title : map traceLabel observations
      putStrLn $ "All registry checks passed. Complete outcomes and states: " <> file
    "explore" : file : options -> do
      (bounds, settings) <- either fail pure $ exploreOptions options
      let start = Scenario.initialFor settings.family
          report = Explorer.explore bounds settings
      LBS.putStr $ encode report
      putStrLn ("" :: String)
      when (isJust report.failure) $ do
        let minimized = Explorer.minimize settings.executionMode start report.path
        (scenario, views) <- demand $ Replay.recordScenario bounds settings start minimized
        LBS.writeFile file $ encode scenario
        Text.writeFile (file <> ".mmd") $ Mermaid.renderSteps views
        putStrLn $ "Saved " <> show (length minimized) <> " actions, minimized under single-action deletion: " <> file
    "replay" : file : options -> do
      scenario <- either (fail . toString) pure . first toText =<< eitherDecodeFileStrict' file
      if "--normal" `elem` options
        then do
          world <- demand $ Explorer.runPath Scenario.VerifySignatures scenario.startingWorld scenario.schedule
          print $ Explorer.invariantFailure world
          outcomes <- demand $ Explorer.pathOutcomes Scenario.VerifySignatures scenario.startingWorld scenario.schedule
          mapM_ print [err | Error err <- outcomes]
          let deliveries = [d | action <- scenario.schedule, d <- case action of Scenario.Deliver d -> [d]; Scenario.Retry d -> [d]; _ -> []]
          putStrLn $ "Normal interpreter replayed " <> show (length deliveries) <> " deliveries with real verification."
        else do
          (world, views) <- demand $ Replay.replayScenario scenario
          print $ Explorer.invariantFailure world
          Text.writeFile (file <> ".replay.mmd") $ Mermaid.renderSteps views
          if "--step" `elem` options
            then do
              putStrLn ("Stepping the prepared trace; primitive replies were validated before inspection." :: String)
              stepLoop views
            else case dropWhile (/= "--break") options of
              _ : name : _ -> mapM_ print $ take 1 $ snd $ Debug.breakAt (toText name) views
              _ -> mapM_ (Text.putStrLn . Debug.description) views
    ["trace", file] -> do
      let route = honestRoute 40 Scenario.initialWorld
      (scenario, views) <- demand $ Replay.recordScenario (Scenario.Limits 40 1) Scenario.defaultExploration Scenario.initialWorld route
      LBS.writeFile file $ encode scenario
      Text.writeFile (file <> ".mmd") $ Mermaid.renderSteps views
      putStrLn file
    "resolved" : options -> withArgs options resolvedBenchmarks
    _ ->
      mapM_
        putStrLn
        [ "head-logic-lab decommit-dependency FILE"
        , "head-logic-lab trace-inputs DIRECTORY"
        , "head-logic-lab compare-crem DIRECTORY"
        , "head-logic-lab machines DIRECTORY"
        , "head-logic-lab registry-demo FILE"
        , "head-logic-lab settlement FILE"
        , "head-logic-lab explore FILE [--fault | --invalid-ack] [--scenario dependent|duplicate|expiry] [--search deliveries-first|retries-first|breadth-first|retry-windows] [--states N] [--deliveries N]"
        , "head-logic-lab replay FILE [--normal | --step | --break NAME]"
        , "head-logic-lab trace FILE"
        , "head-logic-lab resolved [Criterion options]"
        ]

-- | Reject malformed options instead of silently exploring different bounds.
exploreOptions :: [String] -> Either String (Scenario.Limits, Scenario.Exploration)
exploreOptions = go Scenario.defaultLimits Scenario.defaultExploration
 where
  go bounds settings = \case
    [] -> do
      when (settings.searchOrder == Scenario.RetryWindows && settings.disturbances /= Scenario.ValidMessagesOnly) $ Left "retry-windows explores honest delivery/retry schedules only"
      Right (bounds, settings)
    "--fault" : rest -> go bounds settings{Scenario.executionMode = Scenario.BypassVerification, Scenario.disturbances = Scenario.AllowInvalidAcknowledgement} rest
    "--invalid-ack" : rest -> go bounds settings{Scenario.disturbances = Scenario.AllowInvalidAcknowledgement} rest
    "--deliveries" : value : rest -> do
      n <- positive value
      go bounds{Scenario.maxDeliveries = n} settings rest
    "--states" : value : rest -> do
      n <- positive value
      go bounds{Scenario.maxStates = n} settings rest
    "--scenario" : value : rest -> do
      family <- case value of
        "dependent" -> Right Scenario.DependentTransactions
        "duplicate" -> Right Scenario.DuplicateTransaction
        "expiry" -> Right Scenario.DelayedDependency
        _ -> Left "scenario must be dependent, duplicate or expiry"
      go bounds settings{Scenario.family = family} rest
    "--search" : value : rest -> do
      order <- case value of
        "deliveries-first" -> Right Scenario.DeliveriesFirst
        "retries-first" -> Right Scenario.RetriesFirst
        "breadth-first" -> Right Scenario.BreadthFirst
        "retry-windows" -> Right Scenario.RetryWindows
        _ -> Left "search must be deliveries-first, retries-first, breadth-first or retry-windows"
      go bounds settings{Scenario.searchOrder = order} rest
    option : _ -> Left $ "unknown or incomplete exploration option: " <> option
  positive :: String -> Either String Int
  positive value = case readMaybe value of
    Just n | n > 0 -> Right n
    _ -> Left "exploration limits must be positive integers"

demand :: Either Text a -> IO a
demand = either (fail . toString) pure

stepLoop :: [Debug.Step] -> IO ()
stepLoop views = case Debug.stepOnce views of
  Nothing -> pure ()
  Just (view, rest) -> do
    print view
    putStrLn ("Enter: step; break NAME: next named decision; q: quit" :: String)
    getLine >>= \case
      "q" -> pure ()
      line | Just name <- Text.stripPrefix "break " line -> stepLoop $ snd $ Debug.breakAt name rest
      _ -> stepLoop rest

honestRoute :: Int -> Scenario.World -> [Scenario.Action]
honestRoute remaining world
  | remaining <= 0 = []
  | otherwise = case Scenario.pending world of
      d : _ -> let action = Scenario.Deliver d in action : honestRoute (remaining - 1) (either error id $ Explorer.advance Scenario.VerifySignatures world action)
      [] -> case Scenario.retries world of
        d : _ -> let action = Scenario.Retry d in action : honestRoute (remaining - 1) (either error id $ Explorer.advance Scenario.VerifySignatures world action)
        [] -> []

-- | Large unrelated background sets make the full-state difference cost visible.
-- Criterion measures time and (--regress allocated:iters +RTS -T) allocations.
resolvedBenchmarks :: IO ()
resolvedBenchmarks = do
  benchmarks <- forM [100, 10000, 30000] $ \size -> do
    let initial = withBackground size Scenario.initialNode
        os = Scenario.openState initial
        program = onOpenNetworkReqTxProgram (ruleEnvironment (Scenario.environment 1)) initial.chainPointTime os 2 mempty Scenario.transactionOne
        config = Interpreter.ledgerConfig simpleLedger
        events = Resolved.committed $ either (error . show) id $ Resolved.runResolved os.headId config program
        logEntry = Resolved.ResolvedLog os.headId events
        bytes = serialize' logEntry
        staged = Resolved.committed $ fst $ Interpreter.evaluateChanges id (\() _ -> ()) () config program
        deltaInput = fromMaybe (error "program did not apply the transaction") $ find (\case Interpreter.AppliedTransaction{} -> True; _ -> False) staged
        outcome = Interpreter.runProgram simpleLedger program
        expected = aggregateState initial outcome
    decoded <- either (fail . show) pure $ decodeFull' bytes
    unless (Resolved.replay initial decoded == Right expected) $ fail "resolved recovery differs from compatibility replay"
    putStrLn $ "background=" <> show size <> " resolved-bytes=" <> show (BS.length bytes) <> " compatibility-bytes=" <> show (BS.length $ serialize' $ changesOf outcome)
    pure $
      bgroup
        ("background-" <> show size)
        [ bench "construct-delta" $ whnf (deltaSize . Resolved.resolve os.headId) deltaInput
        , bench "resolved-recovery" $ whnf (either (error . show) checksum . Resolved.replay initial) decoded
        , bench "compatibility-recovery" $ whnf (checksum . aggregateState initial) outcome
        , bench "decode-and-recover" $ whnf (either (error . show) (either (error . show) checksum . Resolved.replay initial) . decodeFull') bytes
        ]
  defaultMain benchmarks
 where
  deltaSize :: Either Resolved.ExperimentError (Resolved.ResolvedChange SimpleTx) -> Int
  deltaSize = \case
    Right (Resolved.ApplyDelta _ removed added) -> Set.size removed + Set.size added
    _ -> error "expected a resolved application"
  checksum st = let c = (Scenario.openState st).coordinatedHeadState in Set.size c.localUTxO + Seq.length c.localTxs + Map.size c.allTxs

changesOf :: Outcome tx -> [StateChanged tx]
changesOf = \case
  Continue events _ -> events
  Wait _ events -> events
  Error{} -> []

background :: Int -> Set SimpleTxOut
background size = Set.fromList $ SimpleTxOut . fromIntegral <$> [100 .. size + 99]

withBackground :: Int -> NodeState SimpleTx -> NodeState SimpleTx
withBackground size st = st{headState = Open os{coordinatedHeadState = c{localUTxO = background size <> c.localUTxO, confirmedSnapshot = ConfirmedSnapshot snapshot mempty}}}
 where
  os = Scenario.openState st
  c = os.coordinatedHeadState
  u = background size <> c.localUTxO
  snapshot = Scenario.initialSnapshot{utxo = u, accumulator = Accumulator.buildFromUTxO @SimpleTx u, appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx u}
