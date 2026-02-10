{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Generic Cardano constants for use in testing.
module Test.Hydra.Tx.Fixture (
  module Test.Hydra.Tx.Fixture,
  pparams,
  systemStart,
  slotLength,
  epochInfo,
) where

import "hydra-cardano-api" Hydra.Cardano.Api.Gen
import "hydra-prelude" Hydra.Prelude

import "base" Data.Maybe (fromJust)
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Core (ppPricesL)
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Scripts (Prices (..))
import "cardano-ledger-core" Cardano.Ledger.BaseTypes (BoundedRational (..))
import "cardano-ledger-core" Cardano.Ledger.Coin (Coin (..))
import "cardano-ledger-core" Cardano.Ledger.Core (PParams, ppMinFeeAL, ppMinFeeBL)
import "hydra-cardano-api" Hydra.Cardano.Api (
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
import "hydra-plutus" Hydra.Contract.HeadTokens (headPolicyId)
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams, slotLength, systemStart)
import "hydra-tx" Hydra.Tx (HeadId (..), HeadSeed (..), Party (..), mkHeadId)
import "hydra-tx" Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import "hydra-tx" Hydra.Tx.Crypto (HydraKey, generateSigningKey)
import "hydra-tx" Hydra.Tx.HeadParameters (HeadParameters (..))
import "hydra-tx" Hydra.Tx.OnChainId (AsType (..), OnChainId)
import "hydra-tx" Hydra.Tx.Party (deriveParty)
import "lens" Control.Lens ((.~))

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
