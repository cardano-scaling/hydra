{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | The validator used to deposit and recover locked funds
module Hydra.Contract.Deposit where

import PlutusTx.Prelude

import Hydra.Cardano.Api (PlutusScriptVersion (PlutusScriptV2))
import Hydra.Contract.Commit (Commit)
import Hydra.Contract.DepositError (
  DepositError (
    DepositDeadlineNotReached,
    DepositDeadlineSurpassed,
    DepositNoLowerBoundDefined,
    DepositNoUpperBoundDefined,
    IncorrectDepositHash,
    WrongHeadIdInDepositDatum
  ),
 )
import Hydra.Contract.Error (errorCode)
import Hydra.Contract.Head (hashPreSerializedCommits, hashTxOuts)
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Datum (Datum),
  Extended (Finite),
  Interval (ivFrom),
  LowerBound (LowerBound),
  POSIXTime,
  Redeemer (Redeemer),
  ScriptContext (..),
  ScriptHash,
  SerialisedScript,
  UpperBound (..),
  ivTo,
  serialiseCompiledCode,
  txInfoOutputs,
  txInfoValidRange,
 )
import PlutusTx (CompiledCode, toBuiltinData)
import PlutusTx qualified

data DepositRedeemer
  = -- | Claims already deposited funds.
    -- FIXME: Make sure to change the spec and add head CS to the Claim redeemer.
    Claim CurrencySymbol
  | -- | Recovers m number of deposited outputs.
    Recover Integer

PlutusTx.unstableMakeIsData ''DepositRedeemer

-- | Deposit datum containing HeadId, deadline and a list of deposits.
newtype DepositDatum
  = DepositDatum (CurrencySymbol, POSIXTime, [Commit])

PlutusTx.unstableMakeIsData ''DepositDatum

-- | v_deposit validator checks
--
-- * Claim redeemer ->
--     * The deadline has not been reached.
--     * HeadId matches.
--
-- * Recover redeemer
--     * The deadline has been reached.
--     * The hash of recovered outputs are matching the deposited outputs.
validator :: DepositDatum -> DepositRedeemer -> ScriptContext -> Bool
validator depositDatum r ctx =
  case r of
    Claim headId' -> beforeDeadline && checkHeadId headId'
    Recover m ->
      afterDeadline
        && recoverOutputs m
 where
  DepositDatum (headId, dl, deposits) = depositDatum

  checkHeadId headId' =
    traceIfFalse $(errorCode WrongHeadIdInDepositDatum) $
      headId' == headId

  recoverOutputs m =
    traceIfFalse $(errorCode IncorrectDepositHash) $
      hashOfOutputs m == hashPreSerializedCommits deposits

  hashOfOutputs m =
    hashTxOuts $ take m (txInfoOutputs txInfo)

  beforeDeadline =
    case ivTo (txInfoValidRange txInfo) of
      UpperBound (Finite t) _ ->
        traceIfFalse $(errorCode DepositDeadlineSurpassed) $
          t <= dl
      _ -> traceError $(errorCode DepositNoUpperBoundDefined)

  afterDeadline =
    case ivFrom (txInfoValidRange txInfo) of
      LowerBound (Finite t) _ ->
        traceIfFalse $(errorCode DepositDeadlineNotReached) $
          t > dl
      _ -> traceError $(errorCode DepositNoLowerBoundDefined)

  ScriptContext{scriptContextTxInfo = txInfo} = ctx

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @DepositDatum @DepositRedeemer

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash PlutusScriptV2 validatorScript

datum :: DepositDatum -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: DepositRedeemer -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
