{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.Chain.Direct.State ()
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenSpecsWithSettings,
 )

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (DraftCommitTxRequest SimpleTx)))

  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (DraftCommitTxRequest SimpleTx)))

  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (DraftCommitTxResponse Tx)))

  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (DraftCommitTxResponse Tx)))

settings :: Settings
settings =
  defaultSettings
    { sampleSize = 200
    }
