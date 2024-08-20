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

import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (fromShort)
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
import Hydra.Contract.Head (compareRef, hashPreSerializedCommits, hashTxOuts)
import Hydra.Contract.Util (depositTokenV1, hasST)
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Hydra.ScriptContext (ownCurrencySymbol)
import Hydra.ScriptContext qualified as Hydra
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
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
  TxOutRef,
  UpperBound (UpperBound),
  Value (getValue),
  ivTo,
  serialiseCompiledCode,
  toData,
  txInfoOutputs,
  txInfoValidRange,
 )
import PlutusTx (CompiledCode, toBuiltinData)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

data DepositRedeemer
  = -- | Claims already deposited funds.
    Claim
  | -- | Recovers deposited funds.
    Recover

PlutusTx.unstableMakeIsData ''DepositRedeemer

-- | Minting policy redeemer. Provided `[TxOutRef]` will be used as a minted `TokenName`.
newtype Mint = Mint
  { depositRefs :: [TxOutRef]
  -- ^ Points to the deposited Utxo.
  }

PlutusTx.unstableMakeIsData ''Mint

-- | Deposit datum containing Deposit token currency symbol, deadline and a list of deposits.
type DepositDatum = (CurrencySymbol, POSIXTime, [Commit])

-- | v_deposit validator checks that:
--
-- * On Claim redeemer
--     * The deadline has NOT been reached.
--     * The Deposit token has been burned.
--     * The ST is present in the output.
--
-- * On Recover redeemer
--     * The deadline HAS BEEN reached.
--     * The outputs are recoverable.
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

  dtCurrencySymbol = CurrencySymbol . toBuiltin $ fromShort depositMintValidatorScript

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

depositValidatorScript :: SerialisedScript
depositValidatorScript = serialiseCompiledCode compiledValidator

depositValidatorHash :: ScriptHash
depositValidatorHash = scriptValidatorHash PlutusScriptV2 depositValidatorScript

datum :: DepositDatum -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: DepositRedeemer -> Redeemer
redeemer a = Redeemer (toBuiltinData a)

-- | mu_deposit minting policy checks:
--
-- * The deposit token is minted using provided
--     v_deposit hash as the `CurrencySymbol` and `[TxOutRef]` as `TokenName`.
depositMintValidator ::
  -- | Hash of the v_deposit validator
  ScriptHash ->
  () -> -- TODO: Remove unneeded datum parameter
  Mint ->
  Hydra.ScriptContext ->
  Bool
depositMintValidator _depositValidatorHash _ r ctx =
  case AssocMap.lookup depositCurrencySymbol (getValue tokenVal) of
    Nothing -> False
    Just tokenMap ->
      case AssocMap.lookup (TokenName $ hashTxOutRefs depositRefs) tokenMap of
        Nothing -> False
        Just v -> v == 1
 where
  tokenVal = Hydra.txInfoMint hydraTxInfo

  depositCurrencySymbol = ownCurrencySymbol ctx

  Mint{depositRefs} = r

  Hydra.ScriptContext{Hydra.scriptContextTxInfo = hydraTxInfo} = ctx

-- | Serialise and hash the given `[TxOutRef]` using sha2_256 algorithm.
hashTxOutRefs :: [TxOutRef] -> BuiltinByteString
hashTxOutRefs outRefs =
  sha2_256
    . foldMap (toBuiltin . toStrict . serialise . toData)
    $ sortBy compareRef outRefs
{-# INLINEABLE hashTxOutRefs #-}

depositMintCompiledValidator :: CompiledCode ValidatorType
depositMintCompiledValidator =
  $$(PlutusTx.compile [||wrap . depositMintValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 depositValidatorHash
 where
  wrap = wrapValidator @() @Mint

depositMintValidatorScript :: SerialisedScript
depositMintValidatorScript = serialiseCompiledCode depositMintCompiledValidator

depositMintValidatorHash :: ScriptHash
depositMintValidatorHash = scriptValidatorHash PlutusScriptV2 depositValidatorScript
