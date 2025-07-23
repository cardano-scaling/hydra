module Hydra.Tx.HeadParametersSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Tx.HeadParameters (HeadParameters)
import Hydra.Tx.IsTxSpec (roundtripCBOR)

spec :: Spec
spec =
  parallel $ do
    describe "HeadParameters (cardano)" $
      prop "Roundtrip CBOR encoding HeadParameters" $
        roundtripCBOR @HeadParameters
