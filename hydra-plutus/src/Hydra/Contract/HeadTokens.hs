{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Avoid trace calls to be optimized away when inlining functions.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-simplifier-inline #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import Hydra.Cardano.Api (
  PlutusScriptV2,
  PolicyId,
  TxIn,
  fromPlutusScript,
  scriptPolicyId,
  toPlutusTxOutRef,
  pattern PlutusScript,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState (seed)
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokensError (HeadTokensError (..), errorCode)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Hydra.Contract.Util (hasST)
import Hydra.Plutus.Extras (MintingPolicyType)
import Hydra.ScriptContext (ScriptContext (..), TxInfo (txInfoInputs, txInfoMint), ownCurrencySymbol, scriptOutputsAt)
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2 (
  Datum (getDatum),
  FromData (fromBuiltinData),
  OutputDatum (..),
  ScriptHash,
  SerialisedScript,
  TxInInfo (..),
  TxOutRef,
  Value (getValue),
  serialiseCompiledCode,
 )
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
      seedInput `elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkNumberOfTokens =
    traceIfFalse $(errorCode WrongNumberOfTokensMinted) $
      mintedTokenCount == nParties + 1

  singleSTIsPaidToTheHead =
    traceIfFalse $(errorCode MissingST) $
      hasST currency headValue

  allInitialOutsHavePTs =
    traceIfFalse $(errorCode WrongNumberOfInitialOutputs) (nParties == length initialTxOutValues)
      && all hasASinglePT initialTxOutValues

  allInitialOutsHaveCorrectDatum =
    all hasHeadIdDatum (fst <$> scriptOutputsAt initialValidator txInfo)

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
    maybe 0 sum
      . AssocMap.lookup currency
      . getValue
      $ txInfoMint txInfo

  (headId, seed, nParties) =
    case headDatum of
      OutputDatum datum ->
        case fromBuiltinData @Head.DatumType $ getDatum datum of
          Just Head.Initial{Head.parties = parties, headId = h, seed = s} ->
            (h, s, length parties)
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

  minted = getValue $ txInfoMint txInfo

  burnHeadTokens =
    case AssocMap.lookup currency minted of
      Nothing -> False
      Just tokenMap -> AssocMap.all (< 0) tokenMap

-- | Raw minting policy code where the 'TxOutRef' is still a parameter.
unappliedMintingPolicy :: CompiledCode (TxOutRef -> MintingPolicyType)
unappliedMintingPolicy =
  $$(PlutusTx.compile [||\vInitial vHead ref -> mkUntypedMintingPolicy (validate vInitial vHead ref)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 Initial.validatorHash
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 Head.validatorHash

-- | Get the applied head minting policy script given a seed 'TxOutRef'.
mintingPolicyScript :: TxOutRef -> SerialisedScript
mintingPolicyScript txOutRef =
  serialiseCompiledCode $
    unappliedMintingPolicy
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 txOutRef

-- * Create PolicyId

-- | Get the head policy id (a.k.a headId) given a seed 'TxIn'.
headPolicyId :: TxIn -> PolicyId
headPolicyId =
  scriptPolicyId . PlutusScript . mkHeadTokenScript

-- | Get the applied head minting policy script given a seed 'TxIn'.
mkHeadTokenScript :: TxIn -> Api.PlutusScript
mkHeadTokenScript =
  fromPlutusScript @PlutusScriptV2 . mintingPolicyScript . toPlutusTxOutRef
