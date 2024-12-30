{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (PParams, ppMinFeeAL, ppMinFeeBL)
import Control.Lens ((.~))
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PolicyId,
  SigningKey,
  TxIn,
  deserialiseFromRawBytes,
  genTxIn,
  serialiseToRawBytes,
  verificationKeyHash,
 )
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams, slotLength, systemStart)
import Hydra.Tx (HeadId (..), HeadSeed (..), Party (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Crypto (HydraKey, generateSigningKey)
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (AsType (..), OnChainId)
import Hydra.Tx.Party (deriveParty)
import System.IO.Unsafe (unsafePerformIO)

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

testHeadId :: HeadId
testHeadId = UnsafeHeadId "1234"

testHeadSeed :: HeadSeed
testHeadSeed = UnsafeHeadSeed "000000000000000000#0"

depositDeadline :: UTCTime
depositDeadline = unsafePerformIO getCurrentTime
{-# NOINLINE depositDeadline #-}

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
