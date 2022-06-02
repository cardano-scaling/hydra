module Hydra.Cardano.Api.ScriptHash where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Credential as Ledger

-- | Extract the payment part of an address, as a script hash.
getPaymentScriptHash :: AddressInEra era -> Maybe ScriptHash
getPaymentScriptHash = \case
  AddressInEra _ (ShelleyAddress _ (Ledger.ScriptHashObj h) _) ->
    Just (fromShelleyScriptHash h)
  _ ->
    Nothing
