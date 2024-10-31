{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ScriptHash where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Credential qualified as Ledger
import Hydra.Cardano.Api.PlutusScript ()

-- * Extras

-- | Extract the payment part of an address, as a script hash.
getPaymentScriptHash :: AddressInEra era -> Maybe ScriptHash
getPaymentScriptHash = \case
  AddressInEra _ (ShelleyAddress _ (Ledger.ScriptHashObj h) _) ->
    Just (fromShelleyScriptHash h)
  _ ->
    Nothing

-- | Like 'hashScript', but for a 'ScriptInAnyLang'.
hashScriptInAnyLang :: ScriptInAnyLang -> ScriptHash
hashScriptInAnyLang (ScriptInAnyLang _ script) =
  hashScript script

-- * Orphans

instance Arbitrary ScriptHash where
  arbitrary = do
    hashScript . PlutusScript PlutusScriptV3 <$> arbitrary
