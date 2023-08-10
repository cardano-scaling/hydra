{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic Cardano constants for use in testing.
module Hydra.Chain.Direct.Fixture (
  module Hydra.Chain.Direct.Fixture,
  pparams,
  systemStart,
  epochInfo,
) where

import Hydra.Prelude

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Slotting.Time as Slotting
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  ExecutionUnitPrices (ExecutionUnitPrices),
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PolicyId,
  ProtocolParameters (..),
  TxIn,
  genTxIn,
 )
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Ledger.Cardano ()
import Hydra.Ledger.Cardano.Configuration (LedgerEnv, ProtocolParametersConversionException, newLedgerEnv)
import Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams, systemStart)
import System.IO.Unsafe (unsafeDupablePerformIO)

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
defaultLedgerEnv =
  -- XXX: Ideally we would use the Either or Maybe instance of MonadThrow here,
  -- however that is not possible in the io-classes variants of this type class.
  unsafeDupablePerformIO $
    try (newLedgerEnv defaultPParams) >>= \case
      Left (err :: ProtocolParametersConversionException) ->
        error $ "Failed to create ledger env from fixture: " <> show err
      Right env -> pure env

defaultPParams :: ProtocolParameters
defaultPParams =
  pparams
    { protocolParamPrices = Just $ ExecutionUnitPrices 0 0
    , protocolParamTxFeePerByte = 0
    , protocolParamTxFeeFixed = 0
    }

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
