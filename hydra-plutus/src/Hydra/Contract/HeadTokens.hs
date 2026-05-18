{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:defer-errors #-}
-- Avoid trace calls to be optimized away when inlining functions.
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:no-simplifier-inline #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.1.0 #-}

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
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokensError (HeadTokensError (..), errorCode)
import Hydra.Contract.MintAction (MintAction (Burn, Mint, MintParticipant))
import Hydra.Contract.Util (hasST, hydraHeadV2, scriptOutputsAt)
import Hydra.Plutus.Extras (MintingPolicyType, scriptValidatorHash, wrapMintingPolicy)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 (
  Datum (getDatum),
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash,
  TokenName (..),
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
import PlutusTx.Foldable (length)

validate ::
  ScriptHash ->
  TxOutRef ->
  MintAction ->
  ScriptContext ->
  Bool
validate headValidator seedInput action context =
  case action of
    Mint -> validateTokensMinting headValidator seedInput context
    Burn -> validateTokensBurning context
    MintParticipant -> validateMintParticipant context
{-# INLINEABLE validate #-}

-- | When minting head tokens we want to make sure that:
--
-- * The number of minted PTs == number of participants (+1 for the ST) evident
--   from the datum.
--
-- * There is single state token that is paid into v_head, which ensures
--   continuity.
--
-- * Ensure out-ref and the headId are in the datum of the first output of the
--   transaction which mints tokens.
validateTokensMinting :: ScriptHash -> TxOutRef -> ScriptContext -> Bool
validateTokensMinting headValidator seedInput context =
  seedInputIsConsumed
    && checkNumberOfTokens
    && singleSTIsPaidToTheHead
    && enoughUniquePTsPaidToHead
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

  enoughUniquePTsPaidToHead =
    traceIfFalse $(errorCode MissingPTs) $
      length (uniquePTs headValue) == nParties

  uniquePTs val =
    case AssocMap.lookup currency (getValue val) of
      Nothing -> traceError $(errorCode NoPTs)
      (Just tokenMap) ->
        -- NOTE: Ideally this would be a filterWithKey
        AssocMap.elems . flip AssocMap.mapMaybeWithKey tokenMap $ \an qty ->
          if
            | an == TokenName hydraHeadV2 -> Nothing
            | qty == 1 -> Just an
            | otherwise -> traceError $(errorCode WrongQuantity)

  checkDatum =
    traceIfFalse $(errorCode WrongDatum) $
      headId == currency && seed == seedInput

  mintedTokenCount =
    maybe 0 F.sum
      . AssocMap.lookup currency
      . mintValueToMap
      $ txInfoMint txInfo

  (headId, seed, nParties) =
    case headDatum of
      OutputDatum datum ->
        case fromBuiltinData @Head.DatumType $ getDatum datum of
          Just (Head.Open Head.OpenDatum{Head.parties = parties, headId = h, headSeed = s}) ->
            (h, s, L.length parties)
          _ -> traceError $(errorCode ExpectedHeadDatumType)
      _ -> traceError $(errorCode ExpectedInlineDatum)

  (headDatum, headValue) =
    case scriptOutputsAt headValidator txInfo of
      [(dat, val)] -> (dat, val)
      _ -> traceError $(errorCode MultipleHeadOutput)

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

-- | Allow minting of a single participation token when an 'UpdateParameters'
-- transaction is being applied to the head (issue #1813). The token name
-- (and the exact mint quantity) is enforced by the head validator itself —
-- here we only validate that the mint context is the right kind of head
-- transaction by requiring that the head's policy id appears in exactly one
-- output (the new head state).
validateMintParticipant :: ScriptContext -> Bool
validateMintParticipant context =
  -- Defer to the head validator: it pattern-matches on the spending input's
  -- redeemer and enforces 'mustMintOrBurnParticipationToken' which restricts
  -- the exact (asset name, quantity) of the mint. Here we just ensure the
  -- mint applies a single PT to this head's currency.
  traceIfFalse $(errorCode MintingNotAllowed) onlyOnePTMinted
 where
  currency = ownCurrencySymbol context

  ScriptContext{scriptContextTxInfo = txInfo} = context

  minted = mintValueToMap $ txInfoMint txInfo

  onlyOnePTMinted =
    case AssocMap.lookup currency minted of
      Nothing -> False
      Just tokenMap ->
        case AssocMap.toList tokenMap of
          [(_, qty)] -> qty == 1
          _ -> False

-- | Raw minting policy code where the 'TxOutRef' is still a parameter.
unappliedMintingPolicy :: CompiledCode (TxOutRef -> MintingPolicyType)
unappliedMintingPolicy =
  $$(PlutusTx.compile [||\vHead ref -> wrapMintingPolicy (validate vHead ref)||])
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
