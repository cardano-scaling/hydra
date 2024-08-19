{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic Cardano constants for use in testing.
module Test.Hydra.Tx.Fixture (
  module Test.Hydra.Tx.Fixture,
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
import Cardano.Ledger.Core (PParams, ppMinFeeAL, ppMinFeeBL)
import Cardano.Slotting.Time qualified as Slotting
import Control.Lens ((.~))
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  Key (VerificationKey),
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PolicyId,
  SigningKey,
  TxIn,
  deserialiseFromRawBytes,
  genTxIn,
  getVerificationKey,
  serialiseToRawBytes,
  verificationKeyHash,
 )
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Crypto (HydraKey, generateSigningKey)
import Hydra.Tx.Party (Party, deriveParty)

-- import Hydra.Ledger.Cardano ()
import Hydra.Ledger.Cardano.Configuration (LedgerEnv, newLedgerEnv)
import Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams, slotLength, systemStart)
import Hydra.Tx (HeadId (..), HeadSeed (..), Party (..))
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (AsType (..), OnChainId)

-- | Our beloved alice, bob, and carol.
alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

-- | Hydra signing keys for 'alice', 'bob', and 'carol'.
aliceSk, bobSk, carolSk :: SigningKey HydraKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
-- NOTE: Using 'zcarol' as seed results in ordered 'deriveParty' values
carolSk = generateSigningKey "zcarol"

-- | Hydra verification keys for 'alice', 'bob', and 'carol'.
aliceVk, bobVk, carolVk :: VerificationKey HydraKey
aliceVk = getVerificationKey aliceSk
bobVk = getVerificationKey bobSk
carolVk = getVerificationKey carolSk

testHeadId :: HeadId
testHeadId = UnsafeHeadId "1234"

testHeadSeed :: HeadSeed
testHeadSeed = UnsafeHeadSeed "000000000000000000#0"

-- | Derive some 'OnChainId' from a Hydra party. In the real protocol this is
-- currently not done, but in this simulated chain setting this is definitely
-- fine.
deriveOnChainId :: Party -> OnChainId
deriveOnChainId Party{vkey} =
  case deserialiseFromRawBytes AsOnChainId bytes of
    Left _ -> error "deriveOnChainId failed"
    Right oid -> oid
 where
  bytes = serialiseToRawBytes $ verificationKeyHash vkey

-- | An environment fixture for testing.
testEnvironment :: Environment
testEnvironment =
  Environment
    { party = alice
    , signingKey = aliceSk
    , otherParties = [bob, carol]
    , contestationPeriod = cperiod
    , participants = deriveOnChainId <$> [alice, bob, carol]
    }

-- | Head parameters fixture for testing.
testHeadParameters :: HeadParameters
testHeadParameters =
  HeadParameters
    { contestationPeriod = cperiod
    , parties = [alice, bob, carol]
    }

cperiod :: ContestationPeriod
cperiod = UnsafeContestationPeriod 4

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

defaultPParams :: PParams LedgerEra
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
