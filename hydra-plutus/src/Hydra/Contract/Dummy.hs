{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- | Simple asserting validators that are primarily useful for testing.
module Hydra.Contract.Dummy where

import Hydra.Prelude hiding (foldMap, (<$>), (==))

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (wrapValidator)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  ScriptContext (..),
  ScriptInfo (..),
  TokenName,
  Value (..),
  serialiseCompiledCode,
  txInfoOutputs,
  txOutValue,
  unsafeFromBuiltinData,
 )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Eq ((==))
import PlutusTx.Foldable (foldMap)
import PlutusTx.Functor ((<$>))
import PlutusTx.List qualified as L
import PlutusTx.Prelude (check, traceIfFalse)

dummyValidatorScript :: PlutusScript
dummyValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = SpendingScript{}} -> True
                _ -> False
            ||]
        )

alwaysFailingScript :: () -> () -> ScriptContext -> Bool
alwaysFailingScript _ _ _ = traceIfFalse "alwaysFailingScript" False

dummyValidatorScriptAlwaysFails :: PlutusScript
dummyValidatorScriptAlwaysFails =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$(PlutusTx.compile [||wrap alwaysFailingScript||])
 where
  wrap = wrapValidator @() @()

dummyMintingScript :: PlutusScript
dummyMintingScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = MintingScript{}} -> True
                _ -> False
            ||]
        )

dummyRewardingScript :: PlutusScript
dummyRewardingScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = CertifyingScript{}} -> True
                ScriptContext{scriptContextScriptInfo = RewardingScript{}} -> True
                _ -> False
            ||]
        )

-------------------------------------------------------------------
-- Example user script to demonstrate committing to correct Head --
-------------------------------------------------------------------
newtype R = R
  { expectedHeadId :: CurrencySymbol
  }
  deriving stock (Show, Generic)

unstableMakeIsData ''R

exampleValidator ::
  () ->
  R ->
  ScriptContext ->
  Bool
exampleValidator _ redeemer ctx =
  checkCorrectHeadId
 where
  checkCorrectHeadId =
    let outputValue = foldMap txOutValue (txInfoOutputs (scriptContextTxInfo ctx))
        pts = findParticipationToken expectedHeadId outputValue
     in traceIfFalse "HeadId is not correct" (L.length pts == 1)

  findParticipationToken :: CurrencySymbol -> Value -> [(TokenName, Integer)]
  findParticipationToken headCurrency (Value val) =
    case AssocMap.toList <$> AssocMap.lookup headCurrency val of
      Just tokens ->
        L.filter (\(_, n) -> n == 1) tokens
      _ ->
        []
  {-# INLINEABLE findParticipationToken #-}

  R{expectedHeadId} = redeemer

exampleSecureValidatorScript :: PlutusScript
exampleSecureValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||wrap exampleValidator||]
        )
 where
  wrap = wrapValidator @() @R
