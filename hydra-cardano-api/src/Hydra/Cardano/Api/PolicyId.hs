{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PolicyId where

import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Plutus.TxInfo qualified as Ledger
import "cardano-ledger-mary" Cardano.Ledger.Mary.Value qualified as Ledger
import "plutus-ledger-api" PlutusLedgerApi.V3 (CurrencySymbol, fromBuiltin, unCurrencySymbol)

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptHash ()

-- * Type conversions

-- | Convert Cardano api 'PolicyId' to Cardano ledger `PolicyID`.
toLedgerPolicyID :: PolicyId -> Ledger.PolicyID
toLedgerPolicyID (PolicyId sh) = Ledger.PolicyID (toShelleyScriptHash sh)

-- | Convert Cardano api 'PolicyId' to Plutus `CurrencySymbol`.
toPlutusCurrencySymbol :: PolicyId -> CurrencySymbol
toPlutusCurrencySymbol = Ledger.transPolicyID . toLedgerPolicyID

-- | Convert a plutus 'CurrencySymbol' into a cardano-api 'PolicyId'.
fromPlutusCurrencySymbol :: MonadFail m => CurrencySymbol -> m PolicyId
fromPlutusCurrencySymbol cs =
  case deserialiseFromRawBytes AsPolicyId bytes of
    Left err -> fail (show err)
    Right pid -> pure pid
 where
  bytes = fromBuiltin $ unCurrencySymbol cs
