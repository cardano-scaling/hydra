{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | The validator used to deposit funds
module Hydra.Contract.Deposit where

import PlutusTx.Prelude

import Hydra.Cardano.Api (PlutusScriptVersion (PlutusScriptV2))
import Hydra.Contract.Commit (Commit)
import Hydra.Contract.CommitError (CommitError (STIsMissingInTheOutput))
import Hydra.Contract.DepositError (
  DepositError (
    DepositDeadlineNotReached,
    DepositDeadlineSurpassed,
    DepositNoLowerBoundDefined,
    DepositNoUpperBoundDefined,
    IncorrectDepositHash
  ),
 )
import Hydra.Contract.Error (errorCode)
import Hydra.Contract.Head (hashPreSerializedCommits, hashTxOuts)
import Hydra.Contract.Util (depositTokenV1, hasST)
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
  TokenName (TokenName),
  TxInfo (txInfoMint),
  TxOut (txOutValue),
  UpperBound (UpperBound),
  Value (getValue),
  ivTo,
  serialiseCompiledCode,
  txInfoOutputs,
  txInfoValidRange,
 )
import PlutusTx (CompiledCode, toBuiltinData)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import Prelude qualified as Haskell

data DepositRedeemer
  = -- | Claims already deposited funds.
    Claim
  | -- | Recovers deposited funds.
    Recover

PlutusTx.unstableMakeIsData ''DepositRedeemer

type DepositDatum = (CurrencySymbol, POSIXTime, [Commit])

validator :: DepositDatum -> DepositRedeemer -> ScriptContext -> Bool
validator (headId, dl, deposits) r ctx =
  case r of
    Claim ->
      beforeDeadline
        && mustBurnDT
        && hasHeadST
    Recover ->
      afterDeadline
        && recoverOutputs
 where
  recoverOutputs =
    traceIfFalse $(errorCode IncorrectDepositHash) $
      hashOfOutputs == hashPreSerializedCommits deposits

  hashOfOutputs =
    hashTxOuts $ take (length deposits) (tail $ txInfoOutputs txInfo)

  hasHeadST =
    traceIfFalse
      $(errorCode STIsMissingInTheOutput)
      (hasST headId headOutputValue)

  headOutputValue =
    txOutValue . head $ txInfoOutputs (scriptContextTxInfo ctx)

  tokenVal = txInfoMint $ scriptContextTxInfo ctx

  -- TODO: this value is \mu_deposit validator hash parametarized by seed
  dtCurrencySymbol = Haskell.undefined

  mustBurnDT =
    case AssocMap.lookup dtCurrencySymbol (getValue tokenVal) of
      Nothing -> False
      Just tokenMap ->
        case AssocMap.lookup (TokenName depositTokenV1) tokenMap of
          Nothing -> False
          Just v -> v == negate 1

  beforeDeadline =
    case ivTo (txInfoValidRange txInfo) of
      UpperBound (Finite t) _ ->
        traceIfFalse $(errorCode DepositDeadlineSurpassed) $
          t < dl
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
