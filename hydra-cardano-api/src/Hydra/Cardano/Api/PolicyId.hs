{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PolicyId where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Alonzo.Plutus.TxInfo qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Ledger
import Hydra.Cardano.Api.ScriptHash ()
import PlutusLedgerApi.V3 (CurrencySymbol, fromBuiltin, unCurrencySymbol)

-- * Orphans

-- missing CBOR instances

instance ToCBOR PolicyId where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR PolicyId where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes AsPolicyId bs of
      Left err -> fail (show err)
      Right v -> pure v

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
