module Hydra.Cardano.Api.ScriptHash where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Credential as Ledger

-- | Extract the payment part of an address, as a script hash. If any
--
-- Used: Hydra.Ledger.Cardano#findScriptOutput L621
getPaymentScriptHash :: AddressInEra Era -> Maybe ScriptHash
getPaymentScriptHash = \case
  AddressInEra _ (ShelleyAddress _ (Ledger.ScriptHashObj h) _) ->
    Just (fromShelleyScriptHash h)
  _ ->
    Nothing
