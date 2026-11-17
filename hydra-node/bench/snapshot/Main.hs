-- | Criterion benchmark of the per-snapshot work a single hydra-node performs
-- when processing a 'ReqSn': transaction re-application, accumulator build, G1
-- commitment and Ed25519 signing, isolated from networking and persistence.
--
-- Drives the pure 'HeadLogic.update' with a synthetic open head at a grid of
-- UTxO sizes and transactions per snapshot. The result projection demands the
-- 'AckSn' signature, which transitively forces the accumulator commitment,
-- and errors loudly on any other outcome so fixture drift cannot silently
-- benchmark an error path.
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
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (NetworkEffect),
  HeadState (Open),
  OpenState (..),
  Outcome (Continue, effects),
  SeenSnapshot (LastSeenSnapshot),
  aggregateState,
  checkSnapshotUTxOSize,
  isLeader,
  update,
 )
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Cardano (cardanoLedger)
import Hydra.Network.Message (Message (AckSn, ReqSn))
import Hydra.Node (mkNetworkInput)
import Hydra.Node.State (ChainPointTime (..), NodeState (..))
import Hydra.Tx (HeadParameters (..), Snapshot (..), txId)
import Hydra.Tx.Accumulator (buildFromUTxO, getAccumulatorHash, maxAccumulatorSize)
import Hydra.Tx.Crypto (Signature, sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))
import Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, testEnvironment)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, carol, cperiod, dperiod, testHeadId, testHeadSeed)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (mkQCGen)

main :: IO ()
main = do
  -- Larger head sizes are opt-in:
  --
  -- > BENCH_MAX_UTXO=32767 cabal bench hydra-node:snapshot
  maxN <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "BENCH_MAX_UTXO"
  let utxoSizes = filter (<= maxN) [10, 100, 1000, 4095, 8191, 16383, maxAccumulatorSize]
      txCounts = [1, 10, 100]
  benches <- forM [(n, m, pending) | n <- utxoSizes, m <- txCounts, pending <- if m == 1 then [0, 100, 1000] else [0]] $ \(n, m, pending) -> benchCell n m pending
  defaultMain benches

benchCell :: Int -> Int -> Int -> IO Benchmark
benchCell n m pending = do
  let (seedUTxO, localTransactions) = generateWith (100000 + m) $ genFixedSizeSequenceOfSimplePaymentTransactions (m + pending)
      txs = take m localTransactions
      background = generateWith (200000 + n) $ genUTxOAdaOnlyOfSize (max 0 (n - UTxO.size seedUTxO))
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
                        , allTxs = Map.fromList [(txId tx, tx) | tx <- localTransactions]
                        , localTxs = Seq.fromList localTransactions
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
  -- Pre-force the confirmed snapshot's commitment so only the requested
  -- snapshot's accumulator work is measured.
  let !_ = getAccumulatorHash acc0
      !_ = applyOrCrash utxo localTransactions
      !_ = ackSignature $ update testEnvironment ledger now st reqSn
  pure $
    bgroup
      ("reqsn/utxo-" <> show n <> "/txs-" <> show m <> if pending == 0 then "" else "/backlog-" <> show pending)
      ( [ bench "full-update" $ whnf (ackSignature . update testEnvironment ledger now st) reqSn
        , -- update plus the state aggregation the node loop (and, today, the
          -- API server again) performs on its outcome; WHNF of the resulting
          -- NodeState forces the strict-field cascade like state hydration
          -- does. Compare against full-update to isolate the aggregate share.
          bench "update-and-aggregate" $
            whnf (aggregateState st . update testEnvironment ledger now st) reqSn
        , bench "ledger-apply-only" $ whnf (applyOrCrash utxo) txs
        ]
          -- These do not depend on the number of transactions, so only emit
          -- them once per UTxO size.
          <> concat
            [ [ bench "accumulator-only" $
                  whnf (getAccumulatorHash . buildFromUTxO @Tx) utxo
              , bench "sign-only" $ whnf (sign aliceSk) snap0
              , bench "capacity-check" $ whnf (\u -> checkSnapshotUTxOSize @Tx u Nothing Nothing) utxo
              ]
            | m == 1 && pending == 0
            ]
      )
 where
  parameters = HeadParameters cperiod dperiod [alice, bob, carol]

  leader =
    fromMaybe (error "no leader found for snapshot 2") $
      find (\p -> isLeader parameters p 2) [alice, bob, carol]

  now = posixSecondsToUTCTime 0

  ledger = cardanoLedger defaultGlobals defaultLedgerEnv

  applyOrCrash utxo txs =
    either (\(tx, err) -> error $ "tx does not apply: " <> show (txId tx) <> ": " <> show err) UTxO.size $
      applyTransactions ledger (ChainSlot 1) utxo txs

generateWith :: Int -> Gen a -> a
generateWith seed gen = unGen gen (mkQCGen seed) 30

-- | Extract the AckSn signature from the outcome of processing a ReqSn.
-- 'Signature' is a newtype over the Ed25519 signature, so forcing it to WHNF
-- forces the signing and, transitively, the accumulator commitment it signs.
ackSignature :: Outcome Tx -> Signature (Snapshot Tx)
ackSignature = \case
  Continue{effects} ->
    case [sig | NetworkEffect (AckSn sig _) <- effects] of
      [sig] -> sig
      other -> error $ "expected exactly one AckSn effect, got " <> show (length other)
  outcome -> error $ "unexpected outcome: " <> show outcome
