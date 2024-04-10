{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic Cardano constants for use in testing.
module Hydra.Chain.Direct.Fixture (
  module Hydra.Chain.Direct.Fixture,
  pparams,
  systemStart,
  slotLength,
  epochInfo,
) where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Core (ppPricesL)
import Cardano.Ledger.Alonzo.Scripts (Prices (..))
import Cardano.Ledger.BaseTypes (BoundedRational (..))
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (ppMinFeeAL, ppMinFeeBL)
import Cardano.Slotting.Time qualified as Slotting
import Control.Lens ((.~))
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PParams,
  PolicyId,
  TxIn,
  genTxIn,
 )
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Ledger.Cardano ()
import Hydra.Ledger.Cardano.Configuration (LedgerEnv, newLedgerEnv)
import Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams, slotLength, systemStart)

-- * Cardano tx utilities

testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)

testPolicyId :: PolicyId
testPolicyId = headPolicyId testSeedInput

testSeedInput :: TxIn
testSeedInput = generateWith genTxIn 42

-- | Default environment for the L2 ledger using the fixed L1 'pparams' with
-- zeroed fees and prices. NOTE: This is using still a constant SlotNo = 1.
defaultLedgerEnv :: LedgerEnv LedgerEra
defaultLedgerEnv = newLedgerEnv defaultPParams

-- | Default protocol parameters with zeroed fees and prices.
defaultPParams :: PParams
defaultPParams =
  pparams
    & ppPricesL
      .~ ( Prices
            { prMem = fromJust $ boundRational 0
            , prSteps = fromJust $ boundRational 0
            }
         )
    & ppMinFeeAL .~ Coin 0
    & ppMinFeeBL .~ Coin 0

defaultGlobals :: Ledger.Globals
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
    , Ledger.systemStart = Slotting.SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ Ledger.boundRational r
