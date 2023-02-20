{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import Hydra.Cardano.Api (PlutusScriptV2, PolicyId, TxIn, fromPlutusScript, scriptPolicyId, toPlutusTxOutRef, pattern PlutusScript)
import qualified Hydra.Cardano.Api as Api
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Hydra.Contract.Util (hydraHeadV1)
import Plutus.Extras (wrapMintingPolicy)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  FromData (fromBuiltinData),
  MintingPolicy (getMintingPolicy),
  OutputDatum (..),
  Script,
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TokenName (TokenName),
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
-- * There is single state token that is paid into v_head, which ensures
-- continuity. ('singleSTIsPaidToTheHead')
--
-- * The number of minted PTs == number of participants (+1 for the ST) evident
-- from the datum. ('mintedPTsMatchParties')
--
-- * Ensure out-ref and the headId are in the datum of the first output of the
-- transaction which mints tokens. ('seedInputIsConsumed')
validateTokensMinting :: ValidatorHash -> ValidatorHash -> TxOutRef -> ScriptContext -> Bool
validateTokensMinting initialValidator headValidator seedInput context =
  seedInputIsConsumed
    && singleSTIsPaidToTheHead
    && mintedPTsMatchParties
 where
  singleSTIsPaidToTheHead =
    -- we expect a single head output containing ST token
    traceIfFalse "minted wrong" $
      case scriptOutputsAt headValidator txInfo of
        [out] -> headOutputHasST out
        _outputs -> False

  headOutputHasST (_, val) =
    case Map.lookup currency (getValue val) of
      Nothing -> False
      Just tokenMap ->
        Map.lookup (TokenName hydraHeadV1) tokenMap == Just 1

  mintedPTsMatchParties =
    participationTokensAreDistributed currency initialValidator txInfo nParties
      -- here we are doing 'nParties' + 1 to account for the ST token too since
      -- it has the same policy id as PTs.
      && traceIfFalse "minted tokens do not match parties" (mintedTokenCount == nParties + 1)

  minted = getValue $ txInfoMint txInfo

  mintedTokens = fromMaybe Map.empty $ Map.lookup currency minted

  mintedTokenCount =
    foldr
      ( \tokenVal def -> def + tokenVal
      )
      0
      mintedTokens

  currency = ownCurrencySymbol context

  ScriptContext{scriptContextTxInfo = txInfo} = context

  nParties =
    case scriptOutputsAt headValidator txInfo of
      [(datum, _)] ->
        case datum of
          NoOutputDatum -> traceError "missing datum"
          OutputDatum _ -> traceError "unexpected inline datum"
          OutputDatumHash dh ->
            case findDatum dh txInfo of
              Nothing -> traceError "could not find datum"
              Just da ->
                case fromBuiltinData @Head.DatumType $ getDatum da of
                  Nothing -> traceError "expected commit datum type, got something else"
                  Just Head.Initial{Head.parties = parties} -> length parties
                  Just _ -> traceError "unexpected State in datum"
      _ -> traceError "expected single head output"

  seedInputIsConsumed =
    traceIfFalse "seed not consumed" $
      seedInput `elem` (txInInfoOutRef <$> txInfoInputs txInfo)

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
      Just tokenMap -> and $ map ((< 0) . snd) (Map.toList tokenMap)

-- | Checks that outputs from v_initial contain the right quantity of PTs
participationTokensAreDistributed :: CurrencySymbol -> ValidatorHash -> TxInfo -> Integer -> Bool
participationTokensAreDistributed currency initialValidator txInfo nParties =
  case scriptOutputsAt initialValidator txInfo of
    [] -> traceIfFalse "no initial outputs for parties" $ nParties == (0 :: Integer)
    outs -> traceIfFalse "outputs do not match parties" (nParties == length outs) && all hasParticipationToken outs
 where
  hasParticipationToken :: (OutputDatum, Value) -> Bool
  hasParticipationToken (_, val) =
    case Map.lookup currency (getValue val) of
      Nothing -> traceError "no PT distributed"
      (Just tokenMap) -> case Map.toList tokenMap of
        [(_, qty)] -> qty == 1
        _ -> traceError "wrong quantity of PT distributed"

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
