module Hydra.API.ClientInputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (Result (..), fromJSON)
import Hydra.API.ClientInput (ClientInput)
import Hydra.Cardano.Api (serialiseToTextEnvelope)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenSpecsWithSettings,
 )
import Test.QuickCheck (counterexample, forAll, property)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (ClientInput SimpleTx)))
  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (ClientInput Tx)))

  describe "FromJSON (ValidatedTx era)" $ do
    prop "accepts transactions produced via cardano-cli" $
      forAll (arbitrary @Tx) $ \tx ->
        let envelope = toJSON $ serialiseToTextEnvelope (Just "Tx Babbage") tx
         in case fromJSON @Tx envelope of
              Success{} -> property True
              Error e -> counterexample (toString $ toText e) $ property False

settings :: Settings
settings =
  defaultSettings
    { sampleSize = 200
    }
