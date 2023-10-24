{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-simplifier-inline #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Hydra.Contract.Dummy where

import PlutusTx.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Hydra.Cardano.Api (CtxUTxO, PlutusScriptVersion (PlutusScriptV2), fromPlutusTxOut, fromPlutusTxOutRef, toPlutusTxOut, toPlutusTxOutRef)
import qualified Hydra.Cardano.Api as OffChain
import Hydra.Cardano.Api.Network (Network)
import Hydra.Contract.CommitError (CommitError (..), errorCode)
import Hydra.Contract.Util (hasST, mustBurnST)
import Hydra.Data.Party (Party)
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Hydra.ScriptContext (ScriptContext (..), TxInfo (..))
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Datum (..),
  Redeemer (Redeemer),
  ScriptHash,
  SerialisedScript,
  TxOutRef,
  serialiseCompiledCode,
  txOutValue,
 )
import PlutusTx (CompiledCode, fromData, toBuiltinData, toData)
import qualified PlutusTx
import qualified Prelude as Haskell

type DatumType = ()
type RedeemerType = ()

validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator _ _ _ = True

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash PlutusScriptV2 validatorScript

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)

dummyPolicy :: OffChain.PolicyId
dummyPolicy =
  OffChain.scriptPolicyId $
    OffChain.PlutusScript $
      OffChain.fromPlutusScript (serialiseCompiledCode compiledValidator)
