-- | Canonical sample data for driving HeadLogic manually with SimpleTx.
--
-- This module exposes the building blocks the visualizer (and the smoke-test
-- executable) need to step the protocol without any real chain or network:
--
-- * a default 'Environment' for one party out of a three-party head,
-- * the trivial 'Ledger' for 'SimpleTx',
-- * an initial 'NodeState' (idle, slot 0),
-- * a hand-rolled @[Input SimpleTx]@ that walks an idle node through a Tick
--   and an @OnInitTx@ observation.
--
-- The fixture parties (alice, bob, carol) and the canonical contestation
-- period come from @Test.Hydra.Tx.Fixture@; we re-derive nothing.
module HydraVis.Sample where

import Hydra.Prelude

import Hydra.Chain (
  ChainEvent (..),
  OnChainTx (..),
 )
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.Ledger (Ledger)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx, simpleLedger)
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (NodeState, initNodeState, initialChainTime)
import Hydra.Node.UnsyncedPeriod (defaultUnsyncedPeriodFor)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Test.Hydra.Tx.Fixture (
  alice,
  aliceSk,
  bob,
  carol,
  cperiod,
  deriveOnChainId,
  testHeadId,
  testHeadSeed,
 )

-- | A three-party head environment from alice's point of view. Uses the
-- canonical fixture parties so it lines up with the test suite.
sampleEnvironment :: Environment
sampleEnvironment =
  Environment
    { party = alice
    , signingKey = aliceSk
    , otherParties = [bob, carol]
    , contestationPeriod = cperiod
    , depositPeriod = DepositPeriod 20
    , unsyncedPeriod = defaultUnsyncedPeriodFor cperiod
    , participants = deriveOnChainId <$> [alice, bob, carol]
    , configuredPeers = ""
    }

-- | The mock ledger used by every sample party.
sampleLedger :: Ledger SimpleTx
sampleLedger = simpleLedger

-- | An Idle node sitting at slot 0.
sampleInitialState :: NodeState SimpleTx
sampleInitialState = initNodeState (SimpleChainState (ChainSlot 0))

-- | A walltime to feed @update@ with. Picked deterministically so smoke-test
-- output is stable.
sampleNow :: UTCTime
sampleNow = initialChainTime

-- | The wall-clock time we hand to 'update' for the @n@th step of the script;
-- monotonically increasing seconds.
sampleStepTime :: Int -> UTCTime
sampleStepTime n = addUTCTime (fromIntegral n) sampleNow

-- | A short scripted sequence of @Input SimpleTx@. We start with a 'Tick' to
-- advance chain time and then observe the head-init transaction, which should
-- drive the state out of Idle.
sampleScript :: [Input SimpleTx]
sampleScript = [sampleTick, sampleOnInitTx]

sampleTick :: Input SimpleTx
sampleTick =
  ChainInput
    Tick
      { chainTime = addUTCTime 1 sampleNow
      , chainPoint = ChainSlot 1
      }

sampleOnInitTx :: Input SimpleTx
sampleOnInitTx =
  ChainInput
    Observation
      { observedTx =
          OnInitTx
            { headId = testHeadId
            , headSeed = testHeadSeed
            , headParameters =
                HeadParameters
                  { contestationPeriod = cperiod
                  , parties = [alice, bob, carol]
                  }
            , participants = deriveOnChainId <$> [alice, bob, carol]
            }
      , newChainState = SimpleChainState (ChainSlot 2)
      }
