{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Generic Cardano constants for use in testing.
module Test.Hydra.Tx.Fixture (
  module Test.Hydra.Tx.Fixture,
  pparams,
  systemStart,
  slotLength,
  epochInfo,
) where

import Hydra.Cardano.Api.Gen
import Hydra.Prelude

import Cardano.Ledger.Alonzo.Core (ppPricesL)
import Cardano.Ledger.Alonzo.Scripts (Prices (..))
import Cardano.Ledger.BaseTypes (BoundedRational (..))
import Cardano.Ledger.Coin (Coin (..), CoinPerByte (..), compactCoinOrError)
import Cardano.Ledger.Core (PParams, ppTxFeeFixedL, ppTxFeePerByteL)
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
  serialiseToRawBytes,
  verificationKeyHash,
 )
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Tx (HeadId (..), HeadSeed (..), Party (..), mkHeadId)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Crypto (HydraKey, generateSigningKey)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (AsType (..), OnChainId)
import Hydra.Tx.Party (deriveParty)
import Hydra.Tx.Secret (Secret)
import Test.Hydra.Ledger.Cardano.Fixtures (epochInfo, pparams, slotLength, systemStart)
import Test.Hydra.Prelude

-- | Our beloved alice, bob, and carol.
alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

-- | Hydra signing keys for 'alice', 'bob', and 'carol'. Wrapped in
-- 'Secret' so the codebase never holds an unwrapped key.
aliceSk, bobSk, carolSk :: Secret (SigningKey HydraKey)
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
-- NOTE: Using 'zcarol' as seed results in ordered 'deriveParty' values
carolSk = generateSigningKey "zcarol"

testHeadId :: HeadId
testHeadId = mkHeadId testPolicyId

-- XXX: DRY with testSeedInput
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

-- | Head parameters fixture for testing.
testHeadParameters :: HeadParameters
testHeadParameters =
  HeadParameters
    { contestationPeriod = cperiod
    , parties = [alice, bob, carol]
    }

cperiod :: ContestationPeriod
cperiod = 4

-- * Fanout test constants

-- | Typical number of outputs per partial fanout chunk. Used in tests and
-- benchmarks to generate realistic scenarios.
fanoutChunkSize :: Int
fanoutChunkSize = 7

-- | UTxO count at or below which a single regular fanout transaction is used
-- as the preferred first attempt. Used in tests to produce scenarios that
-- exercise both the single-tx and the partial-fanout paths.
fanoutOutputThreshold :: Int
fanoutOutputThreshold = 10

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
    & ppTxFeePerByteL .~ CoinPerByte (compactCoinOrError (Coin 0))
    & ppTxFeeFixedL .~ Coin 0
