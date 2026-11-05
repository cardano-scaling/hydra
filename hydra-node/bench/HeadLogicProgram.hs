{-# LANGUAGE OverloadedRecordDot #-}

-- | Compare the frozen ReqTx handler with the operational program on identical
-- inputs. Both measurements replay every change and force the outgoing request's
-- transaction IDs. Fixture construction and equivalence checks are untimed.
--
-- Criterion can report allocation per iteration as well as elapsed time:
--
-- > cabal bench hydra-node:head-logic-program --benchmark-options='--regress allocated:iters +RTS -T -N1'
--
-- HEADLOGIC_BENCH_MAX_UTXO bounds the default 100/10000-output grid.
-- HEADLOGIC_BENCH_MAX_BACKLOG bounds the default 0/1000-transaction grid.
-- Each ledger runs leader, follower, snapshot-in-flight, and pre-ledger output
-- collision paths with retry budget remaining or exhausted. Collision fixtures
-- add the incoming output to the local UTxO, giving it one additional entry.
-- Pass --trace DIRECTORY to render executions of the actual ReqTx program.
module Main where

import Hydra.Prelude hiding (label, local, state, trace)
import Hydra.Prelude qualified as Prelude

import Cardano.Api.UTxO qualified as UTxO
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, whnf)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (Tx)
import Hydra.Chain.ChainState (ChainSlot (..), ChainStateType, IsChainState)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  HeadState (..),
  OpenState (..),
  Outcome (..),
  SeenSnapshot (..),
  StateChanged (TxInvalid),
  WaitReason (WaitOnNotApplicableTx),
  aggregateState,
  isLeader,
  onOpenNetworkAckSn,
  onOpenNetworkReqSn,
  onOpenNetworkReqTx,
  onOpenNetworkReqTxProgram,
  ruleEnvironment,
 )
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Cardano (cardanoLedger)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), SimpleTxOut (..), simpleLedger)
import Hydra.Network.Message (Message (AckSn, ReqSn))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..), PendingDeposits)
import Hydra.Tx (HeadParameters (..), IsTx (..), Snapshot (..), UTxOType)
import Hydra.Tx.Accumulator (buildFromUTxO)
import Hydra.Tx.Crypto (sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), getSnapshot)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Test.Hydra.HeadLogic.LegacyReqTx (legacyOnOpenNetworkReqTx)
import Test.Hydra.HeadLogic.LegacySnapshot
import Test.Hydra.HeadLogic.Mermaid qualified as Mermaid
import Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, testEnvironment)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, cperiod, dperiod, testHeadId, testHeadSeed)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (mkQCGen)

type Handler tx = Environment -> Ledger tx -> ChainPointTime -> OpenState tx -> TTL -> PendingDeposits tx -> tx -> Outcome tx

data Fixture tx = Fixture
  { environment :: Environment
  , ledger :: Ledger tx
  , state :: NodeState tx
  , incoming :: tx
  , utxoSize :: UTxOType tx -> Int
  , expectsRequest :: Bool
  , ttl :: TTL
  , expectedDisposition :: Disposition
  }

data Disposition = AppliedNormally | WaitingForRetry | InvalidAfterTimeout
  deriving stock (Eq)

main :: IO ()
main =
  getArgs >>= \case
    ["--trace", directory] -> writeTraces directory
    _ -> runBenchmarks

runBenchmarks :: IO ()
runBenchmarks = do
  maxUTxO <- fromMaybe 10000 . (>>= readMaybe) <$> lookupEnv "HEADLOGIC_BENCH_MAX_UTXO"
  maxBacklog <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "HEADLOGIC_BENCH_MAX_BACKLOG"
  let cells = [(n, backlog) | n <- [100, 10000], n <= maxUTxO, backlog <- [0, 1000], backlog <= maxBacklog]
  when (null cells) $ Prelude.error "benchmark bounds exclude every fixture"
  benchmarks <- forM cells $ \(n, backlog) -> do
    simple <- benchmarkLedger "simple" n backlog (simpleFixture n backlog)
    cardano <- benchmarkLedger "cardano" n backlog (cardanoFixture n backlog)
    pure [simple, cardano]
  reverseOrder <- (== Just "program-first") <$> lookupEnv "HEADLOGIC_BENCH_ORDER"
  rounds <- forM cells $ \(n, backlog) -> do
    simple <- benchmarkRound reverseOrder "simple" n backlog (simpleFixture n backlog True)
    cardano <- benchmarkRound reverseOrder "cardano" n backlog (cardanoFixture n backlog True)
    pure [simple, cardano]
  defaultMain $ concat benchmarks <> concat rounds

-- | Every diagram comes from interpreting the same exposed rule that production
-- executes. Legacy agreement checks keep each rendering tied to its scenario.
writeTraces :: FilePath -> IO ()
writeTraces directory = do
  createDirectoryIfMissing True directory
  diagrams <- forM scenarios $ \(name, fixture) ->
    case fixture.state.headState of
      Open st -> do
        let program = onOpenNetworkReqTxProgram (ruleEnvironment fixture.environment) fixture.state.chainPointTime st fixture.ttl mempty fixture.incoming
            (outcome, trace) = Interpreter.traceProgram fixture.ledger program
            expected = runHandler legacyOnOpenNetworkReqTx fixture fixture.incoming
            destination = directory </> name <.> "mmd"
            diagram = (Mermaid.renderLinearTrace . fmap Trace.traceLabel) trace
        unless (outcome == expected) $ Prelude.error $ "trace disagrees with legacy handler: " <> toText name
        TextIO.writeFile destination diagram
        putStrLn destination
        pure (name, diagram)
      _ -> Prelude.error "trace fixture is not open"
  let index = directory </> "README.md"
  TextIO.writeFile index $
    Text.unlines
      [ "# Head logic: concrete ReqTx executions"
      , ""
      , "These diagrams come from interpreting the actual `onOpenNetworkReqTxProgram`"
      , "with the simple test ledger. Each diagram covers one concrete input; unvisited"
      , "branches are outside its scope. Every generated run checks its complete outcome"
      , "against the frozen migration baseline."
      , ""
      , "Regenerate this page and its `.mmd` files with:"
      , ""
      , "```sh"
      , "cabal bench hydra-node:head-logic-program --benchmark-options='--trace " <> toText directory <> "'"
      , "```"
      ]
      <> foldMap
        ( \(name, diagram) ->
            "\n## " <> Text.replace "-" " " (toText name) <> "\n\n```mermaid\n" <> diagram <> "```\n"
        )
        diagrams
  putStrLn index
 where
  leader = simpleFixture 100 0 True
  missingInput = leader{incoming = leader.incoming{txInputs = Set.singleton 999999}, expectsRequest = False, expectedDisposition = WaitingForRetry}
  collision = withOutputCollision 5 leader
  inFlight = withSnapshotInFlight leader

  scenarios :: [(String, Fixture SimpleTx)]
  scenarios =
    [ ("leader-success", leader)
    , ("follower", simpleFixture 100 0 False)
    , ("wait", missingInput)
    , ("timeout", missingInput{ttl = 0, expectedDisposition = InvalidAfterTimeout})
    , ("output-collision", collision)
    , ("snapshot-in-flight", inFlight)
    ]

benchmarkLedger :: IsChainState tx => String -> Int -> Int -> (Bool -> Fixture tx) -> IO Benchmark
benchmarkLedger label n backlog buildFixture = do
  roles <- forM scenarios $ \(role, fixture) -> do
    let old = runHandler legacyOnOpenNetworkReqTx fixture fixture.incoming
        new = runHandler onOpenNetworkReqTx fixture fixture.incoming
    unless (old == new) $ Prelude.error $ toText label <> "/" <> toText role <> ": handler outcomes differ"
    unless (aggregateState fixture.state old == aggregateState fixture.state new) $
      Prelude.error $
        toText label <> "/" <> toText role <> ": replayed states differ"
    oldChecksum <- evaluate $ runAndChecksum legacyOnOpenNetworkReqTx fixture fixture.incoming
    newChecksum <- evaluate $ runAndChecksum onOpenNetworkReqTx fixture fixture.incoming
    unless (oldChecksum == newChecksum) $ Prelude.error "benchmark checksums differ"
    pure $
      bgroup
        role
        [ bench "legacy" $ whnf (runAndChecksum legacyOnOpenNetworkReqTx fixture) fixture.incoming
        , bench "program" $ whnf (runAndChecksum onOpenNetworkReqTx fixture) fixture.incoming
        ]
  pure $ bgroup (label <> "/utxo-" <> show n <> "/backlog-" <> show backlog) roles
 where
  scenarios =
    [ ("leader", buildFixture True)
    , ("follower", buildFixture False)
    , ("snapshot-in-flight", withSnapshotInFlight $ buildFixture True)
    , ("collision-wait", withOutputCollision 5 $ buildFixture True)
    , ("collision-timeout", withOutputCollision 0 $ buildFixture True)
    ]

withOutputCollision :: IsTx tx => TTL -> Fixture tx -> Fixture tx
withOutputCollision ttl fixture = case fixture.state.headState of
  Open st ->
    let coordinated = st.coordinatedHeadState
        colliding = coordinated{localUTxO = coordinated.localUTxO <> utxoFromTx fixture.incoming}
     in fixture
          { state = fixture.state{headState = Open st{coordinatedHeadState = colliding}}
          , expectsRequest = False
          , ttl
          , expectedDisposition = if ttl > 0 then WaitingForRetry else InvalidAfterTimeout
          }
  _ -> Prelude.error "benchmark fixture is not open"

withSnapshotInFlight :: Fixture tx -> Fixture tx
withSnapshotInFlight fixture = case fixture.state.headState of
  Open st ->
    fixture
      { state = fixture.state{headState = Open st{coordinatedHeadState = st.coordinatedHeadState{seenSnapshot = RequestedSnapshot 1 2}}}
      , expectsRequest = False
      }
  _ -> Prelude.error "benchmark fixture is not open"

runHandler :: Handler tx -> Fixture tx -> tx -> Outcome tx
runHandler handler Fixture{environment, ledger, state, ttl} tx =
  case state.headState of
    Open open -> handler environment ledger state.chainPointTime open ttl Map.empty tx
    _ -> Prelude.error "benchmark fixture is not open"

-- | An Int result demands the state changes and every requested ID without
-- serializing the whole state or traversing unrelated background outputs.
runAndChecksum :: forall tx. IsChainState tx => Handler tx -> Fixture tx -> tx -> Int
runAndChecksum handler fixture tx =
  case outcome of
    Continue{stateChanges, effects}
      | fixture.expectedDisposition /= WaitingForRetry
      , any isInvalid stateChanges == (fixture.expectedDisposition == InvalidAfterTimeout) ->
          checksum stateChanges effects
    Wait{reason = WaitOnNotApplicableTx{}, stateChanges}
      | fixture.expectedDisposition == WaitingForRetry -> checksum stateChanges []
    _ -> Prelude.error "benchmark fixture takes an unexpected terminal path"
 where
  outcome = runHandler handler fixture tx

  isInvalid :: StateChanged tx -> Bool
  isInvalid = \case
    TxInvalid{} -> True
    _ -> False

  checksum :: [StateChanged tx] -> [Effect tx] -> Int
  checksum stateChanges effects =
    case (aggregateState fixture.state outcome).headState of
      Open OpenState{coordinatedHeadState = coordinated} ->
        if length effects /= fromEnum fixture.expectsRequest
          then Prelude.error "benchmark fixture takes an unexpected request path"
          else
            fixture.utxoSize coordinated.localUTxO
              + Seq.length coordinated.localTxs
              + Map.size coordinated.allTxs
              + length stateChanges
              + foldl' (\total effect -> total + effectChecksum effect) 0 effects
      _ -> Prelude.error "ReqTx unexpectedly changes the Head phase"

  effectChecksum :: Effect tx -> Int
  effectChecksum = \case
    NetworkEffect (ReqSn version number txIds Nothing Nothing) ->
      fromIntegral version
        + fromIntegral number
        + foldl' (\total tid -> total + BS.foldl' (\n byte -> n + fromIntegral byte) 0 (txIdBytes @tx tid)) 0 txIds
    _ -> Prelude.error "benchmark receives an unexpected effect"

simpleFixture :: Int -> Int -> Bool -> Fixture SimpleTx
simpleFixture n backlog =
  makeFixture simpleLedger (SimpleChainState 0) Set.size initial txs
 where
  initial = Set.fromList $ SimpleTxOut <$> [1 .. fromIntegral n]
  txs =
    [ SimpleTx
      { txSimpleId = fromIntegral i
      , txInputs = Set.singleton $ SimpleTxOut $ if i == 1 then 1 else fromIntegral (n + i - 1)
      , txOutputs = Set.singleton $ SimpleTxOut $ fromIntegral (n + i)
      }
    | i <- [1 .. backlog + 1]
    ]

cardanoFixture :: Int -> Int -> Bool -> Fixture Tx
cardanoFixture n backlog =
  makeFixture ledger initialChainState UTxO.size (seedUTxO <> background) txs
 where
  ledger = cardanoLedger defaultGlobals defaultLedgerEnv
  (seedUTxO, txs) = deterministic (genFixedSizeSequenceOfSimplePaymentTransactions (backlog + 1)) (n + backlog)
  background = deterministic (genUTxOAdaOnlyOfSize (max 0 (n - UTxO.size seedUTxO))) (n + backlog + 1)

deterministic :: Gen a -> Int -> a
deterministic generator seed = unGen generator (mkQCGen seed) 30

makeFixture ::
  forall tx.
  IsTx tx =>
  Ledger tx ->
  ChainStateType tx ->
  (UTxOType tx -> Int) ->
  UTxOType tx ->
  [tx] ->
  Bool ->
  Fixture tx
makeFixture ledger chainState utxoSize initial txs leads =
  Fixture{environment, ledger, state, incoming, utxoSize, expectsRequest = leads, ttl = 5, expectedDisposition = AppliedNormally}
 where
  (incoming, backlog) = case reverse txs of
    [] -> Prelude.error "benchmark fixture has no incoming transaction"
    tx : rest -> (tx, reverse rest)
  local = case applyTransactions ledger (ChainSlot 1) initial backlog of
    Left _ -> Prelude.error "benchmark backlog fails ledger validation"
    Right utxo -> utxo
  parameters = HeadParameters cperiod dperiod [alice, bob, carol]
  environment = testEnvironment{party = selectedParty, signingKey = fromMaybe (Prelude.error "missing fixture key") $ Map.lookup selectedParty (Map.fromList [(alice, aliceSk), (bob, bobSk), (carol, carolSk)])}
  selectedParty =
    fromMaybe (Prelude.error "benchmark cannot find the requested leader role") $
      find (\party -> isLeader parameters party 2 == leads) [alice, bob, carol]
  accumulator = buildFromUTxO @tx initial
  snapshot =
    Snapshot
      { headId = testHeadId
      , version = 0
      , number = 1
      , confirmed = []
      , utxo = initial
      , utxoToCommit = Nothing
      , depositTxId = Nothing
      , utxoToDecommit = Nothing
      , accumulator
      , appliedAccumulator = accumulator
      }
  state =
    NodeInSync
      { headState =
          Open
            OpenState
              { parameters
              , coordinatedHeadState =
                  CoordinatedHeadState
                    { localUTxO = local
                    , localTxs = Seq.fromList backlog
                    , allTxs = Map.fromList [(txId tx, tx) | tx <- backlog]
                    , confirmedSnapshot = ConfirmedSnapshot{snapshot, signatures = mempty}
                    , seenSnapshot = LastSeenSnapshot 1
                    , currentDepositTxId = Nothing
                    , decommitTx = Nothing
                    , version = 0
                    , finalizedCommit = Nothing
                    , finalizedDecommit = Nothing
                    }
              , chainState
              , headId = testHeadId
              , headSeed = testHeadSeed
              }
      , deposits = mempty
      , chainPointTime = ChainPointTime{currentSlot = ChainSlot 1, currentChainTime = posixSecondsToUTCTime 0, drift = 0}
      }

-- | One requested snapshot and every acknowledgement, including aggregation and
-- next-round selection. Other parties' signatures are made in both timed paths.
-- Trace observers are disabled. Equivalence checks are outside the timed region.
benchmarkRound :: IsChainState tx => Bool -> String -> Int -> Int -> Fixture tx -> IO Benchmark
benchmarkRound reverseOrder ledgerName n backlog fixture = do
  let old = runRound True fixture fixture.incoming
      new = runRound False fixture fixture.incoming
  unless (old == new) $ Prelude.error "snapshot round outcomes or states differ"
  -- Equality alone also accepts two equally broken rounds. Force a successful
  -- confirmation before measuring either implementation.
  case (fixture.state.headState, (snd new).headState) of
    (Open before, Open after)
      | ConfirmedSnapshot{snapshot} <- after.coordinatedHeadState.confirmedSnapshot
      , snapshot.number == (getSnapshot before.coordinatedHeadState.confirmedSnapshot).number + 1
      , all (\case Continue{} -> True; _ -> False) (fst new) ->
          pure ()
    _ -> Prelude.error "snapshot round benchmark does not confirm the next snapshot"
  let pair =
        [ bench "legacy" $ whnf (roundChecksum . runRound True fixture) fixture.incoming
        , bench "program" $ whnf (roundChecksum . runRound False fixture) fixture.incoming
        ]
  pure $ bgroup ("round/" <> ledgerName <> "/utxo-" <> show n <> "/backlog-" <> show backlog) (if reverseOrder then reverse pair else pair)
 where
  roundChecksum (outcomes, st) = case st.headState of
    Open os ->
      let c = os.coordinatedHeadState
       in fixture.utxoSize c.localUTxO + Seq.length c.localTxs + Map.size c.allTxs + length outcomes + case c.confirmedSnapshot of
            InitialSnapshot{} -> 0
            ConfirmedSnapshot{snapshot} -> fromIntegral snapshot.number + length snapshot.confirmed
    _ -> Prelude.error "round leaves open head"

runRound :: IsChainState tx => Bool -> Fixture tx -> tx -> ([Outcome tx], NodeState tx)
runRound legacy fixture incoming =
  case fixture.state.headState of
    Open os ->
      let txOutcome = (if legacy then legacyOnOpenNetworkReqTx else onOpenNetworkReqTx) fixture.environment fixture.ledger fixture.state.chainPointTime os fixture.ttl mempty incoming
          afterTx = aggregateState fixture.state txOutcome
       in case (txOutcome, afterTx.headState) of
            (Continue{effects = [NetworkEffect (ReqSn sv sn ids decommit deposit)]}, Open requestedOpen) ->
              let reqOutcome = (if legacy then legacyOnOpenNetworkReqSn else onOpenNetworkReqSn) fixture.environment fixture.ledger mempty fixture.state.chainPointTime requestedOpen fixture.ttl fixture.environment.party sv sn ids decommit deposit
                  afterRequest = aggregateState afterTx reqOutcome
               in case (reqOutcome, afterRequest.headState) of
                    (Continue{effects = [NetworkEffect (AckSn localSignature _)]}, Open OpenState{coordinatedHeadState = CoordinatedHeadState{seenSnapshot = SeenSnapshot{snapshot}}}) ->
                      foldl' (ack snapshot sn localSignature) ([txOutcome, reqOutcome], afterRequest) [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]
                    _ -> Prelude.error "round request does not produce a snapshot"
            _ -> Prelude.error "round fixture does not request a snapshot"
    _ -> Prelude.error "round fixture is not open"
 where
  ack snapshot sn localSignature (outcomes, st) (sender, sk) = case st.headState of
    Open os ->
      let outcome = (if legacy then legacyOnOpenNetworkAckSn else onOpenNetworkAckSn) fixture.environment st.chainPointTime mempty os sender (if sender == fixture.environment.party then localSignature else sign sk snapshot) sn
       in (outcomes <> [outcome], aggregateState st outcome)
    _ -> Prelude.error "acknowledgement leaves open head"
