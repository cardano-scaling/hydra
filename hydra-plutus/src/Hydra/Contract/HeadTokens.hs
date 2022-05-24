{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Plutus.Extras (scriptValidatorHash, wrapMintingPolicy)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  FromData (fromBuiltinData),
  MintingPolicy (getMintingPolicy),
  OutputDatum (..),
  Script,
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
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

-- FIXME: This doesn't verify that:
--
-- (a) A ST is minted with the right 'HydraHeadV1' name
-- (b) PTs's name have the right shape (i.e. 28 bytes long)
validateTokensMinting :: ValidatorHash -> ValidatorHash -> TxOutRef -> ScriptContext -> Bool
validateTokensMinting initialValidator headValidator seedInput context =
  traceIfFalse "minted wrong" $
    participationTokensAreDistributed currency initialValidator txInfo nParties
      && checkQuantities
      && assetNamesInPolicy == nParties + 1
      && seedInputIsConsumed
 where
  currency = ownCurrencySymbol context

  minted = getValue $ txInfoMint txInfo

  (checkQuantities, assetNamesInPolicy) = case Map.lookup currency minted of
    Nothing -> (False, 0)
    Just tokenMap ->
      foldr
        (\q (assertion, n) -> (assertion && q == 1, n + 1))
        (True, 0)
        tokenMap

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

  seedInputIsConsumed = seedInput `elem` (txInInfoOutRef <$> txInfoInputs txInfo)

validateTokensBurning :: ScriptContext -> Bool
validateTokensBurning context =
  traceIfFalse "burnt wrong" checkAllPTsAreBurnt
 where
  -- we do not check the actual token names but only that all tokens pertaining
  -- to the currency scripts are burnt. This should work whether we are burning
  -- in Abort or FanOut transaction
  checkAllPTsAreBurnt =
    traceIfFalse "inconsistent quantity of head tokens burnt" $
      consumedHeadTokens == burnHeadTokens

  currency = ownCurrencySymbol context

  ScriptContext{scriptContextTxInfo = txInfo} = context

  minted = getValue $ txInfoMint txInfo

  consumedHeadTokens =
    foldr (\x acc -> acc + countOurTokens (txOutValue $ txInInfoResolved x)) 0 $ txInfoInputs txInfo

  countOurTokens v =
    maybe 0 sum (Map.lookup currency $ getValue v)

  burnHeadTokens =
    case Map.lookup currency minted of
      Nothing -> 0
      Just tokenMap -> negate $ sum tokenMap

participationTokensAreDistributed :: CurrencySymbol -> ValidatorHash -> TxInfo -> Integer -> Bool
participationTokensAreDistributed currency initialValidator txInfo nParties =
  case scriptOutputsAt initialValidator txInfo of
    [] -> traceIfFalse "no initial outputs for parties" $ nParties == (0 :: Integer)
    outs -> nParties == length outs && all hasParticipationToken outs
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

validatorScript :: TxOutRef -> Script
validatorScript = getMintingPolicy . mintingPolicy

validatorHash :: TxOutRef -> ValidatorHash
validatorHash = scriptValidatorHash . validatorScript
