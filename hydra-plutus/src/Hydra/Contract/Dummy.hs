{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- | Simple asserting validators that are primarily useful for testing.
module Hydra.Contract.Dummy where

import Hydra.Prelude

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (wrapValidator)
import PlutusLedgerApi.V3 (
  Address,
  Credential (..),
  CurrencySymbol,
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash,
  ScriptInfo (..),
  TxInInfo,
  TxOut,
  addressCredential,
  fromBuiltinData,
  getDatum,
  serialiseCompiledCode,
  txInInfoResolved,
  txInfoInputs,
  txOutAddress,
  txOutDatum,
  unsafeFromBuiltinData,
 )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Prelude (check, traceError, traceIfFalse)

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
data R
  = R
  { expectedHeadId :: CurrencySymbol
  , expectedInitialValidator :: ScriptHash
  }
  deriving stock (Show, Generic)

unstableMakeIsData ''R

exampleValidator ::
  () ->
  R ->
  ScriptContext ->
  Bool
exampleValidator _ redeemer ctx =
  checkInitialInputIsSpent
    && checkCorrectHeadId
 where
  checkInitialInputIsSpent =
    traceIfFalse "Initial input not found" (isNothing initialInput)

  checkCorrectHeadId =
    case extractDatum of
      Nothing -> traceError "Could not decode initial datum"
      Just headId -> traceIfFalse "HeadId is not correct" $ headId == expectedHeadId

  extractDatum =
    case initialInput of
      Nothing -> traceError "Initial input not found"
      Just i -> Just =<< decodeDatum (txInInfoResolved i)

  decodeDatum :: TxOut -> Maybe CurrencySymbol
  decodeDatum txOut = case txOutDatum txOut of
    OutputDatum d -> fromBuiltinData (getDatum d)
    _ -> Nothing

  initialInput = findInitialInput expectedInitialValidator

  findInitialInput :: ScriptHash -> Maybe TxInInfo
  findInitialInput initialScriptHash =
    let allInputs = txInfoInputs info
     in find (isInitialAddress initialScriptHash . txOutAddress . txInInfoResolved) allInputs

  -- Check if an address is a script address with the specified script hash
  isInitialAddress :: ScriptHash -> Address -> Bool
  isInitialAddress expectedHash addr =
    case addressCredential addr of
      ScriptCredential vh -> vh == expectedHash
      PubKeyCredential _ -> False

  info = scriptContextTxInfo ctx

  R{expectedHeadId, expectedInitialValidator} = redeemer

exampleSecureValidatorScript :: PlutusScript
exampleSecureValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||wrap exampleValidator||]
        )
 where
  wrap = wrapValidator @() @R
