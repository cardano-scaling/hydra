{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.APISpec where

import Hydra.Prelude

import Hydra.ClientInput (ClientInput)
import Hydra.JSONSchema (prop_validateToJSON, withJsonSpecifications)
import Hydra.Ledger (Utxo)
import Hydra.Ledger.Cardano (CardanoTx)
import Hydra.ServerOutput (ServerOutput)
import System.FilePath ((</>))
import Test.Hspec (Spec, aroundAll, context, parallel, specify)
import Test.QuickCheck (property)

spec :: Spec
spec = parallel $ do
  context "validates JSON representations against API specification" $ do
    aroundAll (withJsonSpecifications "api.yaml") $ do
      specify "ClientInput" $ \(specs, tmp) ->
        property $ prop_validateToJSON @(ClientInput CardanoTx) specs "inputs" (tmp </> "ClientInput")
      specify "ServerOutput" $ \(specs, tmp) ->
        property $ prop_validateToJSON @(ServerOutput CardanoTx) specs "outputs" (tmp </> "ServerOutput")
      specify "Utxo" $ \(specs, tmp) ->
        property $ prop_validateToJSON @(Utxo CardanoTx) specs "utxo" (tmp </> "Utxo")
      specify "CardanoTx" $ \(specs, tmp) ->
        property $ prop_validateToJSON @CardanoTx specs "txs" (tmp </> "CardanoTx")
