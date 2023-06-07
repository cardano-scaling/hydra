{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.Chain.Direct.State ()
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecs
    (Proxy @(ReasonablySized (DraftCommitTxResponse Tx)))

  roundtripAndGoldenSpecs
    (Proxy @(ReasonablySized (DraftCommitTxRequest Tx)))
