module Test.Hydra.Node.Fixture (
  module Test.Hydra.Node.Fixture,

  -- * Re-exports from upstream fixtures
  module Fixture,
) where

import Hydra.Prelude

import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (LedgerEra, SystemStart (..))
import Hydra.Ledger.Cardano (Globals, LedgerEnv, newLedgerEnv)
import Test.Hydra.Tx.Fixture as Fixture

-- | Default environment for the L2 ledger using the fixed L1 'pparams' with
-- zeroed fees and prices. NOTE: This is using still a constant SlotNo = 0
defaultLedgerEnv :: LedgerEnv LedgerEra
defaultLedgerEnv = newLedgerEnv defaultPParams

defaultGlobals :: Globals
defaultGlobals =
  Ledger.Globals
    { Ledger.epochInfo = epochInfo
    , Ledger.slotsPerKESPeriod = 20
    , Ledger.stabilityWindow = 33
    , Ledger.randomnessStabilisationWindow = 33
    , Ledger.securityParameter = 10
    , Ledger.maxKESEvo = 10
    , Ledger.quorum = 5
    , Ledger.maxMajorPV = maxBound
    , Ledger.maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , Ledger.activeSlotCoeff = Ledger.mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , Ledger.networkId = Ledger.Testnet
    , Ledger.systemStart = SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ Ledger.boundRational r
