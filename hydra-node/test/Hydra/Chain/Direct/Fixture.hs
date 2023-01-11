{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Fixture (
  module Hydra.Chain.Direct.Fixture,
  pparams,
  systemStart,
  epochInfo,
) where

import Hydra.Prelude

import Cardano.Crypto.Hash (hashToBytes)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Slotting.Time as Slotting
import Codec.CBOR.Magic (uintegerFromBytes)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  ExecutionUnitPrices (ExecutionUnitPrices),
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PolicyId,
  ProtocolParameters (..),
  TxIn,
  verificationKeyHash,
 )
import Hydra.Chain.Direct.Tx ()
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Crypto (Hash (HydraKeyHash))
import Hydra.Ledger.Cardano.Configuration (newLedgerEnv)
import Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams, systemStart)
import Hydra.Party (Party (..))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

-- * Party / key utilities

-- | Generate some 'a' given the Party as a seed. NOTE: While this is useful to
-- generate party-specific values, it DOES depend on the generator used. For
-- example, `genForParty genVerificationKey` and `genForParty (fst <$>
-- genKeyPair)` do not yield the same verification keys!
genForParty :: Gen a -> Party -> a
genForParty gen Party{vkey} =
  generateWith gen seed
 where
  seed =
    fromIntegral
      . uintegerFromBytes
      . hydraKeyHashToBytes
      $ verificationKeyHash vkey

  hydraKeyHashToBytes (HydraKeyHash h) = hashToBytes h

-- * Cardano tx utilities

testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)

testPolicyId :: PolicyId
testPolicyId = headPolicyId testSeedInput

testSeedInput :: TxIn
testSeedInput = generateWith arbitrary 42

-- | Default environment for the L2 ledger using the fixed L1 'pparams' with
-- zeroed fees and prices. NOTE: This is using still a constant SlotNo = 1.
defaultLedgerEnv :: Ledger.LedgerEnv LedgerEra
defaultLedgerEnv =
  newLedgerEnv pparams'
 where
  pparams' =
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
    , Ledger.maxMajorPV = 1000
    , Ledger.maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , Ledger.activeSlotCoeff = Ledger.mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , Ledger.networkId = Ledger.Testnet
    , Ledger.systemStart = Slotting.SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ Ledger.boundRational r
