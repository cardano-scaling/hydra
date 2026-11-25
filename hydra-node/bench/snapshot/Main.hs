{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Measure transaction receipt, snapshot requests and partial/final
-- acknowledgements with reproducible Cardano transactions. Each comparison
-- demands the same outcome before optionally aggregating its events. The API
-- projection retains its runtime while folding events, as 'Hydra.API.Server'
-- does. A separate comparison reconstructs its machines after each event.
-- Networking, persistence and output encoding remain outside these timings.
module Main where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, whnf)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (Tx)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.State.Node qualified as NodeMachine
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Cardano (cardanoLedger)
import Hydra.Network.Message (Message (AckSn, ReqSn, ReqTx))
import Hydra.Node (mkNetworkInput)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.HeadLogic (
  CoordinatedHeadState (..),
  Effect (NetworkEffect),
  HeadState (Open),
  Input,
  OpenState (..),
  Outcome (Continue, stateChanges),
  SeenSnapshot (LastSeenSnapshot, SeenSnapshot),
  StateChanged (..),
  aggregateNodeState,
  aggregateState,
  isLeader,
  update,
 )
import Hydra.Node.State (ChainPointTime (..), NodeState (..))
import Hydra.Tx (HeadParameters (..), Snapshot (..), txId)
import Hydra.Tx.Accumulator (buildFromUTxO, getAccumulatorHash, maxAccumulatorSize)
import Hydra.Tx.Crypto (sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))
import Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, testEnvironment)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, cperiod, dperiod, testHeadId, testHeadSeed)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

main :: IO ()
main = do
  -- Larger head sizes are opt-in:
  --
  -- > BENCH_MAX_UTXO=32767 cabal bench hydra-node:snapshot
  maxN <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "BENCH_MAX_UTXO"
  seed <- fromMaybe 42 . (>>= readMaybe) <$> lookupEnv "BENCH_SEED"
  let utxoSizes = filter (<= maxN) [10, 100, 1000, 4095, 8191, 16383, maxAccumulatorSize]
      txCounts = [1, 10, 100]
  benches <- forM [(n, m) | n <- utxoSizes, m <- txCounts] $ uncurry (benchCell seed)
  defaultMain benches

benchCell :: Int -> Int -> Int -> IO Benchmark
benchCell seed n m = do
  let ((seedUTxO, allTransactions), background) = unGen fixture (mkQCGen $ seed + n * 1000 + m) 30
      fixture = do
        payments@(initialUTxO, _) <- genFixedSizeSequenceOfSimplePaymentTransactions (2 * m)
        padding <- genUTxOAdaOnlyOfSize (max 0 (n - UTxO.size initialUTxO))
        pure (payments, padding)
      (txs, backlog) = splitAt m allTransactions
      utxo = seedUTxO <> background
      acc0 = buildFromUTxO @Tx utxo
      snap0 =
        Snapshot
          { headId = testHeadId
          , version = 0
          , number = 1
          , confirmed = []
          , utxo
          , utxoToCommit = Nothing
          , utxoToDecommit = Nothing
          , depositTxId = Nothing
          , accumulator = acc0
          , appliedAccumulator = acc0
          }
      st =
        NodeInSync
          { headState =
              Open
                OpenState
                  { parameters
                  , coordinatedHeadState =
                      CoordinatedHeadState
                        { localUTxO = utxo
                        , allTxs = Map.fromList [(txId tx, tx) | tx <- txs]
                        , localTxs = Seq.fromList txs
                        , confirmedSnapshot = ConfirmedSnapshot{snapshot = snap0, signatures = mempty}
                        , seenSnapshot = LastSeenSnapshot 1
                        , currentDepositTxId = Nothing
                        , decommitTx = Nothing
                        , version = 0
                        , finalizedCommit = Nothing
                        , finalizedDecommit = Nothing
                        }
                  , chainState = initialChainState
                  , headId = testHeadId
                  , headSeed = testHeadSeed
                  }
          , deposits = mempty
          , chainPointTime =
              ChainPointTime
                { currentSlot = ChainSlot 1
                , currentChainTime = now
                , drift = 0
                }
          }
      reqSn = mkNetworkInput leader (ReqSn 0 2 (txId <$> txs) Nothing Nothing)
      requested = advance Request testEnvironment st reqSn
      snapshot = case (coordinated requested).seenSnapshot of
        SeenSnapshot cached _ -> Cached.snapshot cached
        _ -> error "ReqSn did not retain its candidate"
      acks = [mkNetworkInput p $ AckSn (sign sk snapshot) 2 | (p, sk) <- [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]]
      (firstAck, secondAck, finalAck) = case acks of
        [a, b, c] -> (a, b, c)
        _ -> error "expected three acknowledgements"
      once = advance PartialAck testEnvironment requested firstAck
      twice = advance PartialAck testEnvironment once secondAck
      queued = mapCoordinated (\s -> s{localTxs = Seq.fromList backlog, allTxs = Map.fromList [(txId tx, tx) | tx <- allTransactions], localUTxO = applyOrCrash snapshot.utxo backlog}) twice
      withoutTransactions = mapCoordinated (\s -> s{localTxs = mempty, allTxs = mempty}) st
      reqTx = mkNetworkInput alice $ ReqTx $ fromMaybe (error "empty transaction fixture") $ listToMaybe txs
      nextLeader = testEnvironment{party = carol, signingKey = carolSk, otherParties = [alice, bob]}
  -- Pre-force the confirmed snapshot's commitment so only the requested
  -- snapshot's accumulator work is measured.
  _ <- evaluate $ getAccumulatorHash acc0
  -- Check and warm every fixture before Criterion starts the clock. Signing
  -- incoming acknowledgements belongs to the sender, not the measured rule.
  forM_ acks $ \input -> evaluate $ input `seq` ()
  evaluate $ checkOutcome Request $ update testEnvironment ledger now st reqSn
  evaluate $ checkOutcome (FinalAck False) $ update testEnvironment ledger now queued finalAck
  evaluate $ checkOutcome (FinalAck True) $ update nextLeader ledger now queued finalAck
  pure $
    bgroup
      ("utxo-" <> show n <> "/txs-" <> show m)
      ( [ inputBenches "reqtx" Transaction testEnvironment withoutTransactions reqTx
        , inputBenches "reqsn" Request testEnvironment st reqSn
        , inputBenches "acksn/first" PartialAck testEnvironment requested firstAck
        , inputBenches "acksn/second" PartialAck testEnvironment once secondAck
        , inputBenches "acksn/final-follower" (FinalAck False) testEnvironment queued finalAck
        , inputBenches "acksn/final-leader" (FinalAck True) nextLeader queued finalAck
        , bench "reqsn-and-three-acks" $ whnf (\input -> foldl' (advanceAck testEnvironment) (advance Request testEnvironment st input) (zip [PartialAck, PartialAck, FinalAck False] acks)) reqSn
        , bench "ledger-apply-only" $ whnf (UTxO.size . applyOrCrash utxo) txs
        ]
          -- These do not depend on the number of transactions, so only emit
          -- them once per UTxO size.
          <> concat
            [ [ bench "accumulator-only" $
                  whnf (getAccumulatorHash . buildFromUTxO @Tx) utxo
              , bench "sign-only" $ whnf (sign aliceSk) snap0
              ]
            | m == 1
            ]
      )
 where
  parameters = HeadParameters cperiod dperiod [alice, bob, carol]

  leader =
    fromMaybe (error "no leader found for snapshot 2") $
      find (\p -> isLeader parameters p 2) [alice, bob, carol]

  applyOrCrash utxo txs =
    either (\(tx, err) -> error $ "tx does not apply: " <> show (txId tx) <> ": " <> show err) id $
      applyTransactions ledger (ChainSlot 1) utxo txs

now :: UTCTime
now = posixSecondsToUTCTime 0

ledger :: Ledger Tx
ledger = cardanoLedger defaultGlobals defaultLedgerEnv

data Expected = Transaction | Request | PartialAck | FinalAck Bool

-- | Reject fixture drift and force the signature or request payload that the
-- node sends. In particular both ReqSn cases demand the AckSn signature.
checkOutcome :: Expected -> Outcome Tx -> ()
checkOutcome expected outcome = case (expected, outcome) of
  (Transaction, Continue [TransactionReceived{}, TransactionAppliedToLocalUTxO{}] []) -> ()
  (Request, Continue [SnapshotRequested{}] [NetworkEffect (AckSn signed _)]) -> signed `seq` ()
  (PartialAck, Continue [PartySignedSnapshot{}] []) -> ()
  (FinalAck False, Continue [PartySignedSnapshot{}, SnapshotConfirmed{}] []) -> ()
  (FinalAck True, Continue [PartySignedSnapshot{}, SnapshotConfirmed{}, SnapshotRequestDecided{}] [NetworkEffect (ReqSn _ _ ids _ _)]) -> length ids `seq` ()
  _ -> error $ "unexpected outcome: " <> show outcome

inputBenches :: String -> Expected -> Environment -> NodeState Tx -> Input Tx -> Benchmark
inputBenches name expected env st input =
  checkOutcome expected outcome
    `seq` bgroup
      name
      [ bench "full-update" $ whnf (checkOutcome expected . run) input
      , bench "update-and-aggregate" $ whnf (advance expected env st) input
      , bench "update-and-aggregate-and-api" $ whnf withAPI input
      , bench "aggregate-only" $ whnf (aggregateState st) outcome
      , bench "api-projection-only" $ whnf projectAPI outcome
      , bench "api-projection-restarted" $ whnf projectRestartedAPI outcome
      ]
 where
  run = update env ledger now st
  outcome = run input
  projectRestartedAPI = \case
    Continue{stateChanges} -> foldl' aggregateNodeState st stateChanges
    other -> error $ "unexpected outcome: " <> show other
  runtime = NodeMachine.start st
  projectAPI = \case
    Continue{stateChanges} -> NodeMachine.currentState $ foldl' NodeMachine.step runtime stateChanges
    other -> error $ "unexpected outcome: " <> show other
  withAPI i =
    let result = run i
     in checkOutcome expected result `seq` aggregateState st result `seq` projectAPI result `seq` ()

advance :: Expected -> Environment -> NodeState Tx -> Input Tx -> NodeState Tx
advance expected env st input =
  let outcome = update env ledger now st input
   in checkOutcome expected outcome `seq` aggregateState st outcome

advanceAck :: Environment -> NodeState Tx -> (Expected, Input Tx) -> NodeState Tx
advanceAck env st (expected, input) = advance expected env st input

coordinated :: NodeState Tx -> CoordinatedHeadState Tx
coordinated st = case st.headState of
  Open open -> open.coordinatedHeadState
  _ -> error "expected an Open Head"

mapCoordinated :: (CoordinatedHeadState Tx -> CoordinatedHeadState Tx) -> NodeState Tx -> NodeState Tx
mapCoordinated f st = case st.headState of
  Open open -> st{headState = Open open{coordinatedHeadState = f open.coordinatedHeadState}}
  _ -> error "expected an Open Head"
