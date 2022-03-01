{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import Ledger hiding (Mint)
import PlutusTx.Prelude

import Hydra.Contract.Head (Head)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Ledger.Typed.Scripts (ValidatorTypes (..), wrapMintingPolicy)
import Plutus.V1.Ledger.Api (fromBuiltinData)
import Plutus.V1.Ledger.Value (getValue)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

{-# INLINEABLE validate #-}
validate ::
  -- | Head validator
  ValidatorHash ->
  ValidatorHash ->
  TxOutRef ->
  MintAction ->
  ScriptContext ->
  Bool
validate initialValidator headValidator _ action context =
  case action of
    Mint ->
      traceIfFalse "minted wrong" $
        participationTokensAreDistributed currency initialValidator txInfo nParties
          && checkQuantities
          && assetNamesInPolicy == nParties + 1
    Burn ->
      traceIfFalse "burnt wrong" $
        checkQuantities && assetNamesInPolicy == nParties + 1
 where
  currency = ownCurrencySymbol context

  minted = getValue $ txInfoMint txInfo

  (checkQuantities, assetNamesInPolicy) = case Map.lookup currency minted of
    Nothing -> (False, 0)
    Just tokenMap ->
      foldr
        (\q (assertion, n) -> (assertion && (q == mintedQuantityExpected), n + 1))
        (True, 0)
        tokenMap

  mintedQuantityExpected = case action of
    Mint -> 1
    Burn -> -1

  ScriptContext{scriptContextTxInfo = txInfo} = context

  nParties =
    case scriptOutputsAt headValidator txInfo of
      [(dh, _)] ->
        case getDatum <$> findDatum dh txInfo of
          Nothing -> traceError "expected optional commit datum"
          Just da ->
            case fromBuiltinData @(DatumType Head) da of
              Nothing -> traceError "expected commit datum type, got something else"
              Just Head.Initial{Head.parties = parties} -> length parties
              Just _ -> traceError "unexpected State in datum"
      _ -> traceError "expected single head output"

participationTokensAreDistributed :: CurrencySymbol -> ValidatorHash -> TxInfo -> Integer -> Bool
participationTokensAreDistributed currency initialValidator txInfo nParties =
  case scriptOutputsAt initialValidator txInfo of
    [] -> traceIfFalse "no initial outputs for parties" $ nParties == (0 :: Integer)
    outs -> all hasParticipationToken outs
 where
  hasParticipationToken :: (DatumHash, Value) -> Bool
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
