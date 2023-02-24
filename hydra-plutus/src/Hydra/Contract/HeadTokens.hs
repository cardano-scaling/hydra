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
--   transaction which mints tokens. FIXME: Need to also check out-ref and
--   headId in datum!?
validateTokensMinting :: ValidatorHash -> ValidatorHash -> TxOutRef -> ScriptContext -> Bool
validateTokensMinting initialValidator headValidator seedInput context =
  seedInputIsConsumed
    && checkNumberOfTokens
    && singleSTIsPaidToTheHead
    && allInitialOutsHavePTs
 where
  seedInputIsConsumed =
    traceIfFalse "seed not consumed" $
      seedInput `elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkNumberOfTokens =
    traceIfFalse "wrong number of tokens minted" $
      mintedTokenCount == nParties + 1

  singleSTIsPaidToTheHead =
    traceIfFalse "missing ST" $
      hasST currency headValue

  allInitialOutsHavePTs =
    traceIfFalse "wrong number of initial outputs" (nParties == length initialTxOutValues)
      && all hasASinglePT initialTxOutValues

  hasASinglePT val =
    case Map.lookup currency (getValue val) of
      Nothing -> traceError "no PT distributed"
      (Just tokenMap) -> case Map.toList tokenMap of
        [(_, qty)]
          | qty == 1 -> True
        _ -> traceError "wrong quantity"

  mintedTokenCount =
    fromMaybe 0
      . fmap sum
      . Map.lookup currency
      . getValue
      $ txInfoMint txInfo

  nParties =
    -- HACK: We cannot do a traceError in the Nothing case here because of
    -- strictness. Still we are not interested in the individual Nothing cases
    -- so let's produce an always wrong value instead.
    fromMaybe (-1) $ do
      dh <- case headDatum of
        OutputDatumHash dh -> Just dh
        _ -> Nothing
      da <- findDatum dh txInfo
      state <- fromBuiltinData @Head.DatumType $ getDatum da
      case state of
        Head.Initial{Head.parties = parties} -> Just $ length parties
        _ -> Nothing

  (headDatum, headValue) =
    case scriptOutputsAt headValidator txInfo of
      [(dat, val)] -> (dat, val)
      _ -> traceError "expected single head output"

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
  traceIfFalse "minting not allowed" burnHeadTokens
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
