{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.Fixture where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits (..), Prices (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), boundRational)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import Data.Array (Array, array)
import Data.Bits (shift)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Ledger.Cardano (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Plutus.V1.Ledger.Api (PubKeyHash (PubKeyHash), toBuiltin)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck.Instances ()

testNetworkId :: NetworkId
testNetworkId = Testnet (NetworkMagic 42)

maxTxSize :: Int64
maxTxSize = 1 `shift` 14

pparams :: PParams Era
pparams =
  def
    { _costmdls = Map.singleton PlutusV1 $ fromJust defaultCostModel
    , _maxValSize = 1000000000
    , _maxTxExUnits = ExUnits 10000000000 10000000000
    , _maxBlockExUnits = ExUnits 10000000000 10000000000
    , _protocolVersion = ProtVer 5 0
    , _prices =
        Prices
          { prMem = fromJust $ boundRational $ 721 % 10000000
          , prSteps = fromJust $ boundRational $ 577 % 10000
          }
    }

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . toBuiltin <$> (arbitrary :: Gen ByteString)

-- REVIEW(SN): taken from 'testGlobals'
epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

-- NOTE(SN): copied from Test.Cardano.Ledger.Alonzo.Tools as not exported
costModels :: Array Language CostModel
costModels = array (PlutusV1, PlutusV1) [(PlutusV1, fromJust defaultCostModel)]
