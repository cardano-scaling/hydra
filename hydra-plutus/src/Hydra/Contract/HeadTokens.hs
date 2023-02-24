{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

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
import qualified Hydra.Cardano.Api as Api
import qualified Hydra.Contract.Head as Head
import Hydra.Contract.HeadState (headId, seed)
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Hydra.Contract.Util (hasST)
import Plutus.Extras (wrapMintingPolicy)
import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  FromData (fromBuiltinData),
  MintingPolicy (getMintingPolicy),
  OutputDatum (..),
  Script,
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInInfo (..),
  TxInfo (..),
  TxOutRef,
  ValidatorHash,
  Value (getValue),
  mkMintingPolicyScript,
 )
import Plutus.V2.Ledger.Contexts (findDatum, ownCurrencySymbol, scriptOutputsAt)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map

validate ::
  -- | Head validator
  ValidatorHash ->
  ValidatorHash ->
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
-- * PTs are distributed to v_initial.
--
-- * Ensure out-ref and the headId are in the datum of the first output of the
--   transaction which mints tokens.
validateTokensMinting :: ValidatorHash -> ValidatorHash -> TxOutRef -> ScriptContext -> Bool
validateTokensMinting initialValidator headValidator seedInput context =
  seedInputIsConsumed
    && checkNumberOfTokens
    && singleSTIsPaidToTheHead
    && allInitialOutsHavePTs
    && checkDatum
 where
  seedInputIsConsumed =
    traceIfFalse "M01" $
      seedInput `elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkNumberOfTokens =
    traceIfFalse "M02" $
      mintedTokenCount == nParties + 1

  singleSTIsPaidToTheHead =
    traceIfFalse "M03" $
      hasST currency headValue

  allInitialOutsHavePTs =
    traceIfFalse "M04" (nParties == length initialTxOutValues)
      && all hasASinglePT initialTxOutValues

  checkDatum =
    traceIfFalse "M05" $
      headId == currency && seed == seedInput

  hasASinglePT val =
    case Map.lookup currency (getValue val) of
      Nothing -> traceError "M07"
      (Just tokenMap) -> case Map.toList tokenMap of
        [(_, qty)]
          | qty == 1 -> True
        _ -> traceError "M08"

  mintedTokenCount =
    maybe 0 sum
      . Map.lookup currency
      . getValue
      $ txInfoMint txInfo

  (headId, seed, nParties) =
    case headDatum of
      OutputDatumHash dh ->
        case findDatum dh txInfo >>= fromBuiltinData @Head.DatumType . getDatum of
          Just Head.Initial{Head.parties = parties, headId = h, seed = s} ->
            (h, s, length parties)
          _ -> traceError "M09"
      _ -> traceError "M10"

  (headDatum, headValue) =
    case scriptOutputsAt headValidator txInfo of
      [(dat, val)] -> (dat, val)
      _ -> traceError "M11"

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
  traceIfFalse "M06" burnHeadTokens
 where
  currency = ownCurrencySymbol context

  ScriptContext{scriptContextTxInfo = txInfo} = context

  minted = getValue $ txInfoMint txInfo

  burnHeadTokens =
    case Map.lookup currency minted of
      Nothing -> False
      Just tokenMap -> all (< 0) tokenMap

mintingPolicy :: TxOutRef -> MintingPolicy
mintingPolicy txOutRef =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\vInitial vHead ref -> wrapMintingPolicy (validate vInitial vHead ref)||])
      `PlutusTx.applyCode` PlutusTx.liftCode Initial.validatorHash
      `PlutusTx.applyCode` PlutusTx.liftCode Head.validatorHash
      `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

mintingPolicyScript :: TxOutRef -> Script
mintingPolicyScript = getMintingPolicy . mintingPolicy

-- * Create PolicyId

-- | Resolve the head policy id (a.k.a headId) given a seed 'TxIn'.
headPolicyId :: TxIn -> PolicyId
headPolicyId =
  scriptPolicyId . PlutusScript . mkHeadTokenScript

-- | Get the applied head minting policy script given a seed 'TxIn'.
mkHeadTokenScript :: TxIn -> Api.PlutusScript
mkHeadTokenScript =
  fromPlutusScript @PlutusScriptV2 . mintingPolicyScript . toPlutusTxOutRef
