{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.Fixture where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams, ProtVer (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Data.Bits (shift)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Tx (OnChainHeadState (..), threadToken)
import Hydra.Chain.Direct.Util (Era)
import Plutus.V1.Ledger.Api (PubKeyHash (PubKeyHash), toBuiltin)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters hiding (vkey)
import Test.QuickCheck (Gen, listOf, oneof)
import Test.QuickCheck.Instances ()

maxTxSize :: Int64
maxTxSize = 1 `shift` 15 -- FIXME: current value on mainnet is 2 ^ 14 but this is not enough for SM based Head Script

pparams :: PParams Era
pparams =
  newPParams
    (Alonzo Standard)
    [ Costmdls $ Map.singleton PlutusV1 $ fromJust defaultCostModel
    , MaxValSize 1000000000
    , MaxTxExUnits $ ExUnits 100000000 100000000
    , MaxBlockExUnits $ ExUnits 100000000 100000000
    , ProtocolVersion $ ProtVer 5 0
    ]

instance Arbitrary OnChainHeadState where
  arbitrary = oneof [pure Closed, Initial <$> ((,,) <$> arbitrary <*> pure threadToken <*> arbitrary) <*> listOf initialOutputs]
   where
    initialOutputs = (,) <$> arbitrary <*> arbitrary

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . toBuiltin <$> (arbitrary :: Gen ByteString)
