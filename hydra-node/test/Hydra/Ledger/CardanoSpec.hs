{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.CardanoSpec where

import Hydra.Prelude

import Hydra.Ledger.Cardano ()
import qualified Shelley.Spec.Ledger.API as Cardano
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Cardano.Ledger.EraBuffet (MaryEra, TestCrypto)
import Test.Cardano.Ledger.MaryEraGen ()
import Test.Hspec

spec :: Spec
spec = describe "Cardano Head Ledger" $ do
  roundtripAndGoldenSpecs (Proxy @(Cardano.UTxO (MaryEra TestCrypto)))
