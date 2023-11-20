module Hydra.Cardano.Api.PolicyId where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Alonzo.TxInfo qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Ledger
import Hydra.Cardano.Api.Hash (unsafeScriptHashFromBytes)
import PlutusLedgerApi.V2 (CurrencySymbol, fromBuiltin, unCurrencySymbol)

toLedgerScriptHash :: PolicyId -> Ledger.ScriptHash StandardCrypto
toLedgerScriptHash (PolicyId scriptHash) = toShelleyScriptHash scriptHash

-- | Convert Cardano api 'PolicyId' to Cardano ledger `PolicyID`.
toLedgerPolicyID :: PolicyId -> Ledger.PolicyID StandardCrypto
toLedgerPolicyID (PolicyId sh) = Ledger.PolicyID (toShelleyScriptHash sh)

-- | Convert Cardano api 'PolicyId' to Plutus `CurrencySymbol`.
toPlutusCurrencySymbol :: PolicyId -> CurrencySymbol
toPlutusCurrencySymbol = Ledger.transPolicyID . toLedgerPolicyID

-- | Convert a plutus 'CurrencySymbol' into a cardano-api 'PolicyId'.
fromPlutusCurrencySymbol :: CurrencySymbol -> PolicyId
fromPlutusCurrencySymbol = PolicyId . unsafeScriptHashFromBytes . fromBuiltin . unCurrencySymbol
{-# DEPRECATED fromPlutusCurrencySymbol "this is partial" #-}
