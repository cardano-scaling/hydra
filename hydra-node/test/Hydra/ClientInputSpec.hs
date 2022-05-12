{-# LANGUAGE TypeApplications #-}

module Hydra.ClientInputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (Result (..), Value (String), fromJSON, object, (.=))
import Hydra.ClientInput (ClientInput)
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
    (Proxy @(ReasonablySized (ClientInput SimpleTx)))
  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (ClientInput Tx)))

  describe "FromJSON (ValidatedTx era)" $ do
    let cborHex =
          "84a5008182582097b2af6dfc6a4825e934146f424cdd6ede43ff98c355d2\
          \ae3aa95b0f70b63949070d800181825839004a294f1ef53b30cdbf7caf17\
          \798422a90227224f9fbf037fcf6c47a5bc2ec1952d1189886fe018214eed\
          \45f83ab04171c41f373d530ca7a61a3b984433021a000285cd0e80a10081\
          \8258209c3b2886f7b196ee20ce39c46abda9a76278534678b3e74288055f\
          \8b73f8ba3a5840b684bee361b3e4513dd961083d76aff4d5c56ac56d130d\
          \ecc772963aac8dbc4caaa78ce62e1ac289f05ec85fe79a78007be6a22ddf\
          \c52b3ffbb7fb8d3cd8cd0ff5f6"
    let envelope =
          object
            [ "type" .= String "Tx AlonzoEra"
            , "description" .= String ""
            , "cborHex" .= String cborHex
            ]

    it "accepts transactions produced via cardano-cli" $ do
      fromJSON @Tx envelope `shouldSatisfy` \case
        Success{} -> True
        Error e -> error (toText e)

    it "accepts raw CBOR-base16-encoded transactions" $ do
      fromJSON @Tx (String cborHex) `shouldSatisfy` \case
        Success{} -> True
        Error e -> error (toText e)

settings :: Settings
settings =
  defaultSettings
    { sampleSize = 200
    }
