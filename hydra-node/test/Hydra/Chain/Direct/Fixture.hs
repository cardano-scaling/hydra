{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.Fixture (
  module Hydra.Chain.Direct.Fixture,
  pparams,
) where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import qualified Cardano.Ledger.Alonzo.PParams as Ledger.Alonzo
import Cardano.Ledger.Alonzo.Scripts (CostModel)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.Slot as Ledger
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import qualified Cardano.Slotting.EpochInfo as Slotting
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SlotLength, SystemStart (SystemStart), mkSlotLength)
import qualified Cardano.Slotting.Time as Slotting
import Data.Array (Array, array)
import Data.Bits (shift)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  ExecutionUnits (..),
  LedgerEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PolicyId,
  SlotNo (..),
  TxIn,
  toLedgerExUnits,
 )
import Hydra.Chain.Direct.Tx (headPolicyId)
import Hydra.Ledger.Cardano.Evaluate (pparams)
import Plutus.V1.Ledger.Api (PubKeyHash (PubKeyHash), toBuiltin)
import qualified Test.Cardano.Ledger.Alonzo.AlonzoEraGen as Ledger.Alonzo
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck.Instances ()

testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)

testPolicyId :: PolicyId
testPolicyId = headPolicyId testSeedInput

testSeedInput :: TxIn
testSeedInput = generateWith arbitrary 42

-- | Current mainchain max transaction size in bytes.
maxTxSize :: Int64
maxTxSize = 1 `shift` 14 -- 16kB

-- | Current mainchain max transaction execution unit budget.
maxTxExecutionUnits :: ExecutionUnits
maxTxExecutionUnits =
  ExecutionUnits
    { executionMemory = 14_000_000
    , executionSteps = 10_000_000_000
    }

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . toBuiltin <$> (arbitrary :: Gen ByteString)

-- REVIEW(SN): taken from 'testGlobals'
epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo epochSize slotLength

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

-- NOTE(SN): copied from Test.Cardano.Ledger.Alonzo.Tools as not exported
costModels :: Array Language CostModel
costModels = array (PlutusV1, PlutusV1) [(PlutusV1, fromJust defaultCostModel)]

defaultLedgerEnv :: Ledger.LedgerEnv LedgerEra
defaultLedgerEnv =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 1
    , Ledger.ledgerIx = 0
    , Ledger.ledgerPp =
        def
          { Ledger.Alonzo._maxTxSize = 1024 * 1024
          , Ledger.Alonzo._maxValSize = 5000
          , Ledger.Alonzo._maxCollateralInputs = 10
          , Ledger.Alonzo._maxTxExUnits =
              toLedgerExUnits maxTxExecutionUnits
          , Ledger.Alonzo._maxBlockExUnits =
              Ledger.Alonzo.ExUnits
                { Ledger.Alonzo.exUnitsMem = 50_000_000
                , Ledger.Alonzo.exUnitsSteps = 40_000_000_000
                }
          , Ledger.Alonzo._costmdls =
              -- XXX(SN): This is a sledgehammer approach: The genTx would hit
              -- execution budgets with the defaultCostModel. There is a TODO in
              -- cardano-ledger's AlonzoEraGen.hs about not using freeCostModel
              Map.fromList $
                [ (lang, Ledger.Alonzo.freeCostModel)
                | lang <- [minBound .. maxBound]
                ]
          }
    , Ledger.ledgerAccount = error "ledgerEnv: ledgersAccount undefined"
    }

defaultGlobals :: Ledger.Globals
defaultGlobals =
  Ledger.Globals
    { Ledger.epochInfoWithErr = Slotting.fixedEpochInfo (Ledger.EpochSize 100) (Slotting.mkSlotLength 1)
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
