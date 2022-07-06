{-# LANGUAGE TypeApplications #-}

module Hydra.ClientInputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
import Data.Aeson (Result (..), Value (String), fromJSON)
import qualified Data.ByteString.Base16 as Base16
import Hydra.Cardano.Api (serialiseToTextEnvelope, toLedgerTx)
import Hydra.ClientInput (ClientInput)
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

    prop "accepts raw CBOR-base16-encoded transactions" $ do
      forAll (arbitrary @Tx) $ \tx ->
        let cborHex = decodeUtf8 $ Base16.encode $ serialize' $ toLedgerTx tx
         in case fromJSON @Tx (String cborHex) of
              Success{} -> True
              Error e -> error (toText e)

settings :: Settings
settings =
  defaultSettings
    { sampleSize = 200
    }
