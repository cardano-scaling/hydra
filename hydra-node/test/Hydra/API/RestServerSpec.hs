{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.Chain.Direct.State ()
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (
  defaultSettings,
  roundtripAndGoldenSpecsWithSettings,
 )

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecsWithSettings
    defaultSettings
    (Proxy @(DraftCommitTxResponse Tx))

  roundtripAndGoldenSpecsWithSettings
    defaultSettings
    (Proxy @(DraftCommitTxRequest Tx))
