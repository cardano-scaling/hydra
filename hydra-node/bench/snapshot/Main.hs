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
import Hydra.Cardano.Api (Tx, UTxO)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (NetworkEffect),
  HeadState (Open),
  Input,
  OpenState (..),
  Outcome (Continue, effects),
  SeenSnapshot (LastSeenSnapshot),
  isLeader,
  update,
 )
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Cardano (cardanoLedger)
import Hydra.Network.Message (Message (AckSn, ReqSn))
import Hydra.Node (mkNetworkInput)
import Hydra.Node.State (ChainPointTime (..), NodeState (..))
import Hydra.Tx (HeadParameters (..), Snapshot (..), txId)
import Hydra.Tx.Accumulator (buildFromSnapshotUTxOs, getAccumulatorHash)
import Hydra.Tx.Crypto (Signature, sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))
import Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, testEnvironment)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, carol, cperiod, testHeadId, testHeadSeed)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck (generate)

main :: IO ()
main = do
  -- The largest size takes minutes per commitment pre-optimization, so it is
  -- opt-in:
  --
  -- > BENCH_MAX_UTXO=4000 cabal bench hydra-node:snapshot
  maxN <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "BENCH_MAX_UTXO"
  let utxoSizes = filter (<= maxN) [10, 100, 1000, 4000]
      txCounts = [1, 10, 100]
  benches <- forM [(n, m) | n <- utxoSizes, m <- txCounts] $ uncurry benchCell
  defaultMain benches

benchCell :: Int -> Int -> IO Benchmark
benchCell n m = do
  (seedUTxO, txs) <- generate $ genFixedSizeSequenceOfSimplePaymentTransactions m
  background <- generate $ genUTxOAdaOnlyOfSize (max 0 (n - UTxO.size seedUTxO))
  let utxo = seedUTxO <> background
      acc0 = buildFromSnapshotUTxOs @Tx utxo Nothing Nothing
      snap0 =
        Snapshot
          { headId = testHeadId
          , version = 0
          , number = 1
          , confirmed = []
          , utxo
          , utxoToCommit = Nothing
          , utxoToDecommit = Nothing
          , accumulator = acc0
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
                        }
                  , chainState = initialChainState
                  , headId = testHeadId
                  , headSeed = testHeadSeed
                  }
          , pendingDeposits = mempty
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
  pure $
    bgroup
      ("reqsn/utxo-" <> show n <> "/txs-" <> show m)
      ( [ bench "full-update" $ whnf (ackSignature . update testEnvironment ledger now st) reqSn
        , bench "ledger-reapply-only" $ whnf (reapplyOrCrash utxo) txs
        ]
          -- These do not depend on the number of transactions, so only emit
          -- them once per UTxO size.
          <> concat
            [ [ bench "accumulator-only" $
                  whnf (\u -> getAccumulatorHash (buildFromSnapshotUTxOs @Tx u Nothing Nothing)) utxo
              , bench "sign-only" $ whnf (sign aliceSk) snap0
              ]
            | m == 1
            ]
      )
 where
  parameters = HeadParameters cperiod [alice, bob, carol]

  leader =
    fromMaybe (error "no leader found for snapshot 2") $
      find (\p -> isLeader parameters p 2) [alice, bob, carol]

  now = posixSecondsToUTCTime 0

  ledger = cardanoLedger defaultGlobals defaultLedgerEnv

  reapplyOrCrash utxo txs =
    either (\(tx, err) -> error $ "tx does not reapply: " <> show (txId tx) <> ": " <> show err) UTxO.size $
      reapplyTransactions ledger (ChainSlot 1) utxo txs

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
