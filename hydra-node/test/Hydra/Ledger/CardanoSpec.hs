{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.CardanoSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Data.Aeson as Aeson
import Hydra.Ledger.Cardano (CardanoTxWitnesses)
import qualified Shelley.Spec.Ledger.API as Cardano
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Cardano.Ledger.EraBuffet (MaryEra)
import Test.Cardano.Ledger.MaryEraGen ()

spec :: Spec
spec = describe "Cardano Head Ledger" $ do
  roundtripAndGoldenSpecs (Proxy @(Cardano.UTxO (MaryEra StandardCrypto)))
  roundtripAndGoldenSpecs (Proxy @(CardanoTxWitnesses StandardCrypto))

  -- TODO(SN): ensure we use bech32 for addresses in a test
  it "should parse a Cardano.UTxO" $ do
    let bs =
          "{\"9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903#0\":\
          \  {\"address\":\"addr1vx35vu6aqmdw6uuc34gkpdymrpsd3lsuh6ffq6d9vja0s6spkenss\",\
          \   \"value\":{\"lovelace\":14}}}"
    case Aeson.eitherDecode bs of
      Left err -> failure @IO err
      Right (_ :: Cardano.UTxO (MaryEra StandardCrypto)) -> pure ()
