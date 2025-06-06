{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Avoid trace calls to be optimized away when inlining functions.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-simplifier-inline #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import Hydra.Cardano.Api (
  PolicyId,
  TxIn,
  scriptPolicyId,
  toPlutusTxOutRef,
  pattern PlutusScript,
  pattern PlutusScriptSerialised,
 )
import Hydra.Cardano.Api qualified as Api
import PlutusTx.Foldable qualified as F
import PlutusTx.List qualified as L

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState (seed)
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokensError (HeadTokensError (..), errorCode)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Hydra.Contract.Util (hasST, scriptOutputsAt)
import Hydra.Plutus (initialValidatorScript)
import Hydra.Plutus.Extras (MintingPolicyType, scriptValidatorHash, wrapMintingPolicy)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 (
  Datum (getDatum),
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash,
  TxInInfo (..),
  TxInfo (..),
  TxOutRef,
  Value (getValue),
  mintValueToMap,
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol)
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

validate ::
  ScriptHash ->
  ScriptHash ->
  TxOutRef ->
  MintAction ->
  ScriptContext ->
  Bool
validate initialValidator headValidator seedInput action context =
  case action of
    Mint -> validateTokensMinting initialValidator headValidator seedInput context
    Burn -> validateTokensBurning context
{-# INLINEABLE validate #-}

-- | When minting head tokens we want to make sure that:
--
-- * The number of minted PTs == number of participants (+1 for the ST) evident
--   from the datum.
--
-- * There is single state token that is paid into v_head, which ensures
--   continuity.
--
-- * PTs are distributed to v_initial
--
-- * Each v_initial has the policy id as its datum
--
-- * Ensure out-ref and the headId are in the datum of the first output of the
--   transaction which mints tokens.
validateTokensMinting :: ScriptHash -> ScriptHash -> TxOutRef -> ScriptContext -> Bool
validateTokensMinting initialValidator headValidator seedInput context =
  seedInputIsConsumed
    && checkNumberOfTokens
    && singleSTIsPaidToTheHead
    && allInitialOutsHavePTs
    && allInitialOutsHaveCorrectDatum
    && checkDatum
 where
  seedInputIsConsumed =
    traceIfFalse $(errorCode SeedNotSpent) $
      seedInput `L.elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkNumberOfTokens =
    traceIfFalse $(errorCode WrongNumberOfTokensMinted) $
      mintedTokenCount == nParties + 1

  singleSTIsPaidToTheHead =
    traceIfFalse $(errorCode MissingST) $
      hasST currency headValue

  allInitialOutsHavePTs =
    traceIfFalse $(errorCode WrongNumberOfInitialOutputs) (nParties == L.length initialTxOutValues)
      && L.all hasASinglePT initialTxOutValues

  allInitialOutsHaveCorrectDatum =
    L.all hasHeadIdDatum (fst <$> scriptOutputsAt initialValidator txInfo)

  checkDatum =
    traceIfFalse $(errorCode WrongDatum) $
      headId == currency && seed == seedInput

  hasASinglePT val =
    case AssocMap.lookup currency (getValue val) of
      Nothing -> traceError $(errorCode NoPT)
      (Just tokenMap) -> case AssocMap.toList tokenMap of
        [(_, qty)]
          | qty == 1 -> True
        _ -> traceError $(errorCode WrongQuantity)

  hasHeadIdDatum = \case
    NoOutputDatum ->
      traceError $(errorCode WrongInitialDatum)
    OutputDatum dat ->
      checkInitialDatum dat
    OutputDatumHash _dh ->
      traceError $(errorCode WrongInitialDatum)

  checkInitialDatum dat =
    case fromBuiltinData @Initial.DatumType $ getDatum dat of
      Nothing -> traceError $(errorCode WrongInitialDatum)
      Just hid -> traceIfFalse $(errorCode WrongInitialDatum) $ hid == currency

  mintedTokenCount =
    maybe 0 F.sum
      . AssocMap.lookup currency
      . mintValueToMap
      $ txInfoMint txInfo

  (headId, seed, nParties) =
    case headDatum of
      OutputDatum datum ->
        case fromBuiltinData @Head.DatumType $ getDatum datum of
          Just Head.Initial{Head.parties = parties, headId = h, seed = s} ->
            (h, s, L.length parties)
          _ -> traceError $(errorCode ExpectedHeadDatumType)
      _ -> traceError $(errorCode ExpectedInlineDatum)

  (headDatum, headValue) =
    case scriptOutputsAt headValidator txInfo of
      [(dat, val)] -> (dat, val)
      _ -> traceError $(errorCode MultipleHeadOutput)

  initialTxOutValues = snd <$> scriptOutputsAt initialValidator txInfo

  currency = ownCurrencySymbol context

  ScriptContext{scriptContextTxInfo = txInfo} = context

-- | Token burning check should:
-- * Not restrict burning on the mu_head at all.
--
-- It is ensured by the v_head validator, when tokens of a specific headId may
-- be burned.
--
-- 'validateTokensBurning' just makes sure all tokens have negative quantity.
validateTokensBurning :: ScriptContext -> Bool
validateTokensBurning context =
  traceIfFalse $(errorCode MintingNotAllowed) burnHeadTokens
 where
  currency = ownCurrencySymbol context

  ScriptContext{scriptContextTxInfo = txInfo} = context

  minted = mintValueToMap $ txInfoMint txInfo

  burnHeadTokens =
    case AssocMap.lookup currency minted of
      Nothing -> False
      Just tokenMap -> AssocMap.all (< 0) tokenMap

-- | Raw minting policy code where the 'TxOutRef' is still a parameter.
unappliedMintingPolicy :: CompiledCode (TxOutRef -> MintingPolicyType)
unappliedMintingPolicy =
  $$(PlutusTx.compile [||\vInitial vHead ref -> wrapMintingPolicy (validate vInitial vHead ref)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 (scriptValidatorHash initialValidatorScript)
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 (scriptValidatorHash Head.validatorScript)

-- | Get the applied head minting policy script given a seed 'TxOutRef'.
mintingPolicyScript :: TxOutRef -> Api.PlutusScript
mintingPolicyScript txOutRef =
  PlutusScriptSerialised . serialiseCompiledCode $
    unappliedMintingPolicy
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 txOutRef

-- * Create PolicyId

-- | Get the head policy id (a.k.a headId) given a seed 'TxIn'.
headPolicyId :: TxIn -> PolicyId
headPolicyId =
  scriptPolicyId . PlutusScript . mkHeadTokenScript

-- | Get the applied head minting policy script given a seed 'TxIn'.
mkHeadTokenScript :: TxIn -> Api.PlutusScript
mkHeadTokenScript =
  mintingPolicyScript . toPlutusTxOutRef
