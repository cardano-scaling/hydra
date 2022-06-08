{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Fixture (
  module Hydra.Chain.Direct.Fixture,
  pparams,
) where

import Hydra.Prelude

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.BaseTypes (TxIx (TxIx))
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SlotLength, SystemStart (SystemStart), mkSlotLength)
import qualified Cardano.Slotting.Time as Slotting
import Codec.CBOR.Magic (uintegerFromBytes)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PolicyId,
  SlotNo (..),
  TxIn,
  verificationKeyHash,
 )
import Hydra.Chain.Direct.Tx (headPolicyId)
import Hydra.Crypto (Hash (HydraKeyHash))
import Hydra.Ledger.Cardano.Evaluate (pparams)
import Hydra.Party (Party (..))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.ModelState (accountStateZero)

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

-- REVIEW(SN): taken from 'testGlobals'
epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo epochSize slotLength

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

defaultLedgerEnv :: Ledger.LedgerEnv LedgerEra
defaultLedgerEnv =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 1
    , Ledger.ledgerIx = TxIx 0
    , Ledger.ledgerPp = pparams
    , Ledger.ledgerAccount = accountStateZero
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
