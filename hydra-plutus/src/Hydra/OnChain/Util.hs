{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.OnChain.Util where

import Ledger
import PlutusTx.Prelude

import Ledger.Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints (
  mustBeSignedBy,
  mustMintCurrency,
  mustSpendScriptOutput,
 )
import Ledger.Credential (Credential (..))
import Ledger.Value (TokenName (..), assetClass, assetClassValueOf, mpsSymbol)
import qualified Ledger.Value as Value
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import PlutusTx.IsData.Class (ToData (..))

-- | This function can be rather unsafe and should *always* be used with a type
-- annotation to avoid silly mistakes and hours of debugging.
--
-- So prefer:
--
-- >>> asDatum @(DatumType MyContract) val
--
-- over
--
-- >>> asDatum val
--
-- A 'Datum' is an opaque type, and type information is lost when turning into a
-- a 'Data'. Hence, various functions of the Plutus Tx framework that rely on
-- 'Datum' or 'Redeemer' are basically stringly-typed function with no guarantee
-- whatsoever that the 'IsData a' being passed will correspond to what's
-- expected by the underlying script. So, save you some hours of debugging and
-- always explicitly specify the source type when using this function,
-- preferably using the data-family from the 'ValidatorTypes' instance.
asDatum :: ToData a => a -> Datum
asDatum = Datum . toBuiltinData
{-# INLINEABLE asDatum #-}

-- | Always use with explicit type-annotation, See warnings on 'asDatum'.
asRedeemer :: ToData a => a -> Redeemer
asRedeemer = Redeemer . toBuiltinData
{-# INLINEABLE asRedeemer #-}

mustBeSignedByOneOf ::
  [PubKeyHash] ->
  ScriptContext ->
  Bool
mustBeSignedByOneOf vks ctx =
  or ((`checkScriptContext` ctx) . mustBeSignedBy @() @() <$> vks)
{-# INLINEABLE mustBeSignedByOneOf #-}

mustReimburse ::
  TxOut ->
  ScriptContext ->
  Bool
mustReimburse txOut =
  elem txOut . txInfoOutputs . scriptContextTxInfo
{-# INLINEABLE mustReimburse #-}

-- /!\ IMPORTANT NOTICE /!\
--
-- This function does not check that a particular validator is executed with a
-- known datum. That means, for validators that are not unique (e.g. the Hydra
-- head contract parameterized with a unique policy ID), there's no guarantee
-- that this executes the _right_ validator. To consider only with great care
mustRunContract ::
  forall redeemer.
  (ToData redeemer) =>
  ValidatorHash ->
  redeemer ->
  ScriptContext ->
  Bool
mustRunContract script redeemer ctx =
  case findContractInput script ctx of
    Nothing ->
      traceIfFalse "mustRunContract: script not found." False
    Just contractRef ->
      traceIfFalse "mustRunContract: mustSpendScriptOutput failed for redeemer." $
        checkScriptContext @() @()
          ( mconcat
              [ mustSpendScriptOutput contractRef (asRedeemer redeemer)
              ]
          )
          ctx
{-# INLINEABLE mustRunContract #-}

mustForwardParty ::
  ScriptContext ->
  MintingPolicyHash ->
  PubKeyHash ->
  Bool
mustForwardParty ctx policyId vk =
  traceIfFalse "PT not spent" mustSpendToken
    && traceIfFalse "PT not produced" mustProduceToken
 where
  info = scriptContextTxInfo ctx

  mustSpendToken =
    assetClassValueOf (valueSpent info) participationToken == 1

  mustProduceToken =
    assetClassValueOf (valueProduced info) participationToken == 1

  participationToken = assetClass (mpsSymbol policyId) (mkPartyName vk)
{-# INLINEABLE mustForwardParty #-}

mustBurnParty ::
  ScriptContext ->
  MintingPolicyHash ->
  PubKeyHash ->
  Bool
mustBurnParty ctx policyId vk =
  let assetName = mkPartyName vk
   in checkScriptContext @() @() (mustMintCurrency policyId assetName (-1)) ctx
{-# INLINEABLE mustBurnParty #-}

mkParty ::
  MintingPolicyHash ->
  PubKeyHash ->
  Value
mkParty policyId vk =
  Value.singleton (Value.mpsSymbol policyId) (mkPartyName vk) 1
{-# INLINEABLE mkParty #-}

mkPartyName ::
  PubKeyHash ->
  TokenName
mkPartyName =
  TokenName . getPubKeyHash
{-# INLINEABLE mkPartyName #-}

hasParty :: MintingPolicyHash -> TxInInfo -> Bool
hasParty policyId input =
  let currency = Value.mpsSymbol policyId
   in currency `elem` symbols (txOutValue $ txInInfoResolved input)
{-# INLINEABLE hasParty #-}

filterInputs :: (TxInInfo -> Bool) -> ScriptContext -> [(TxOutRef, TxOut)]
filterInputs predicate =
  mapMaybe fn . txInfoInputs . scriptContextTxInfo
 where
  fn info
    | predicate info = Just (txInInfoOutRef info, txInInfoResolved info)
    | otherwise = Nothing
{-# INLINEABLE filterInputs #-}

findUtxo :: ScriptContext -> TxOutRef -> Maybe (TxOutRef, TxOut)
findUtxo ctx ref =
  case filterInputs (\input -> ref == txInInfoOutRef input) ctx of
    [utxo] -> Just utxo
    _ -> Nothing
{-# INLINEABLE findUtxo #-}

findContractInput :: ValidatorHash -> ScriptContext -> Maybe TxOutRef
findContractInput script ctx =
  case filterInputs (matchScript . txInInfoResolved) ctx of
    [utxo] -> Just (fst utxo)
    _ -> Nothing
 where
  matchScript :: TxOut -> Bool
  matchScript txOut =
    case txOutAddress txOut of
      Address (ScriptCredential script') _ -> script == script'
      _ -> False
{-# INLINEABLE findContractInput #-}

-- NOTE: Not using `Value.symbols` because it is broken. In fact, a 'Value' may
-- carry null quantities of some particular tokens, leading to weird situation
-- where both:
--
--   - value == lovelaceValueOf n
--   - symbols value /= [adaSymbol]
--
-- are true at the same time. The equality makes no difference between the
-- absence of a certain symbol/token, and a null quantity of that symbol. Such
-- null quantities are probably constructed when moving values around but they
-- don't really mean anything so they should be discarded from views of the
-- value itself, like 'symbols'
symbols :: Value -> [CurrencySymbol]
symbols = foldr normalize [] . Map.toList . Value.getValue
 where
  normalize :: (CurrencySymbol, Map TokenName Integer) -> [CurrencySymbol] -> [CurrencySymbol]
  normalize (currency, tokens) acc
    | currency `elem` acc = acc
    | otherwise =
      let elems = snd <$> Map.toList tokens
       in if sum elems == 0 then acc else currency : acc
{-# INLINEABLE symbols #-}
