{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.OnChain where

import Ledger
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints (
  mustBeSignedBy,
  mustMintCurrency,
  mustPayToOtherScript,
  mustPayToTheScript,
  mustSpendPubKeyOutput,
  mustSpendScriptOutput,
 )
import Ledger.Credential (Credential (..))
import Ledger.Typed.Scripts (ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (TokenName (..), assetClass, assetClassValueOf, mpsSymbol)
import qualified Ledger.Value as Value
import PlutusPrelude (Generic, (>=>))
import qualified PlutusTx
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import PlutusTx.IsData.Class (ToData (..), fromBuiltinData)
import Text.Show (Show)

--
-- Head Parameters
--

data HeadParameters = HeadParameters
  { participants :: [PubKeyHash]
  , policyId :: MintingPolicyHash
  }

PlutusTx.unstableMakeIsData ''HeadParameters
PlutusTx.makeLift ''HeadParameters

--
-- Hydra State-Machine
--

data State
  = Initial
  | Open [TxOut]
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''State
PlutusTx.unstableMakeIsData ''State

data Transition
  = CollectCom
  | Abort
  deriving (Generic)

PlutusTx.makeLift ''Transition
PlutusTx.unstableMakeIsData ''Transition

data Hydra
instance Scripts.ValidatorTypes Hydra where
  type DatumType Hydra = (HeadParameters, State)
  type RedeemerType Hydra = Transition

hydraValidator ::
  (HeadParameters, State) ->
  Transition ->
  ScriptContext ->
  Bool
hydraValidator (params@HeadParameters{participants, policyId}, s) i ctx =
  case (s, i) of
    (Initial, CollectCom) ->
      let collectComUtxos =
            snd <$> filterInputs (hasParty policyId) ctx
          committedOutputs =
            mapMaybe decodeCommit collectComUtxos
          newState =
            (params, Open committedOutputs)
          amountPaid =
            foldMap txOutValue collectComUtxos
       in and
            [ mustBeSignedByOneOf participants ctx
            , all (mustForwardParty ctx policyId) participants
            , checkScriptContext @(RedeemerType Hydra) @(DatumType Hydra)
                (mustPayToTheScript newState amountPaid)
                ctx
            ]
    (Initial, Abort) ->
      let newState =
            (params, Final)
          amountPaid =
            lovelaceValueOf 0
       in and
            [ mustBeSignedByOneOf participants ctx
            , all (mustBurnParty ctx policyId) participants
            , checkScriptContext @(RedeemerType Hydra) @(DatumType Hydra)
                (mustPayToTheScript newState amountPaid)
                ctx
            ]
    _ ->
      False
 where
  decodeCommit =
    txOutDatumHash
      >=> (`findDatum` scriptContextTxInfo ctx)
      >=> (fmap (\(_, _, c) -> c) . fromBuiltinData @(DatumType Commit) . getDatum)

{- ORMOLU_DISABLE -}
hydraTypedValidator :: Scripts.TypedValidator Hydra
hydraTypedValidator = Scripts.mkTypedValidator @Hydra
    $$(PlutusTx.compile [|| hydraValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @(DatumType Hydra) @(RedeemerType Hydra)
{- ORMOLU_ENABLE -}

hydraValidatorHash :: ValidatorHash
hydraValidatorHash = Scripts.validatorHash hydraTypedValidator
{-# INLINEABLE hydraValidatorHash #-}

--
-- Initial Validator
--

data Initial
instance Scripts.ValidatorTypes Initial where
  -- NOTE: Ideally, we would like to get rid of validator hashes in here,
  -- because they are statically known!
  -- So instead of passing them around, we could simply call
  -- `hydraValidatorHash` and `commitValidatorHash` in the `initialValidator`...
  --
  -- However, this makes the compiler chokes with:
  --
  --     GHC Core to PLC plugin: E042:Error: Unsupported feature: Type constructor: GHC.Prim.ByteArray#
  --
  -- Hence their presence here.
  type DatumType Initial = (HeadParameters, ValidatorHash, ValidatorHash, PubKeyHash)
  type RedeemerType Initial = TxOutRef

-- | The Validator 'initial' ensures that the input is consumed by a commit or
-- an abort transaction.
initialValidator ::
  (HeadParameters, ValidatorHash, ValidatorHash, PubKeyHash) ->
  TxOutRef ->
  ScriptContext ->
  Bool
initialValidator (params@HeadParameters{policyId}, hydraScript, commitScript, vk) ref ctx =
  consumedByCommit || consumedByAbort
 where
  -- A commit transaction, identified by:
  --    (a) A signature that verifies as valid with verification key defined as datum
  --    (b) Spending a UTxO also referenced as redeemer.
  --    (c) Having the commit validator in its only output, with a valid
  --        participation token for the associated key, and the total value of the
  --        committed UTxO.
  consumedByCommit =
    case findUtxo ref ctx of
      Nothing ->
        False
      Just utxo ->
        let commitDatum = asDatum @(DatumType Commit) (params, hydraScript, snd utxo)
            commitValue = txOutValue (snd utxo) <> mkParty policyId vk
         in checkScriptContext @(RedeemerType Initial) @(DatumType Initial)
              ( mconcat
                  [ mustBeSignedBy vk
                  , mustSpendPubKeyOutput (fst utxo)
                  , mustPayToOtherScript commitScript commitDatum commitValue
                  ]
              )
              ctx

  consumedByAbort =
    mustRunContract (hydraScript, params) Abort ctx

{- ORMOLU_DISABLE -}
initialTypedValidator :: Scripts.TypedValidator Initial
initialTypedValidator = Scripts.mkTypedValidator @Initial
  $$(PlutusTx.compile [|| initialValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Initial) @(RedeemerType Initial)
{- ORMOLU_ENABLE -}

initialValidatorHash :: ValidatorHash
initialValidatorHash = Scripts.validatorHash initialTypedValidator
{-# INLINEABLE initialValidatorHash #-}

--
-- Commit Validator
--

data Commit
instance Scripts.ValidatorTypes Commit where
  type DatumType Commit = (HeadParameters, ValidatorHash, TxOut)
  type RedeemerType Commit = ()

-- |  To lock outputs for a Hydra head, the ith head member will attach a commit
-- transaction to the i-th output of the initial transaction.
-- Validator 'commit' ensures that the commit transaction correctly records the
-- partial UTxO set Ui committed by the party.
--
-- The data field of the output of the commit transaction is
--
--     Ui = makeUTxO(o_1 , . . . , o_m),
--
-- where the o_j are the outputs referenced by the commit transaction’s inputs
-- and makeUTxO stores pairs (out-ref_j , o_j) of outputs o_j with the
-- corresponding output reference out-ref_j .
commitValidator ::
  (HeadParameters, ValidatorHash, TxOut) ->
  () ->
  ScriptContext ->
  Bool
commitValidator (params, hydraScript, committedOut) () ctx =
  consumedByCollectCom || consumedByAbort
 where
  consumedByCollectCom =
    mustRunContract (hydraScript, params) CollectCom ctx

  consumedByAbort =
    and
      [ mustRunContract (hydraScript, params) Abort ctx
      , mustReimburse committedOut ctx
      ]

{- ORMOLU_DISABLE -}
commitTypedValidator :: Scripts.TypedValidator Commit
commitTypedValidator = Scripts.mkTypedValidator @Commit
  $$(PlutusTx.compile [|| commitValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Commit) @(RedeemerType Commit)
{- ORMOLU_ENABLE -}

commitValidatorHash :: ValidatorHash
commitValidatorHash = Scripts.validatorHash commitTypedValidator
{-# INLINEABLE commitValidatorHash #-}

--
-- Monetary Policy
--

-- For the sake of simplicity in testing and whatnot, we currently 'fake' the
-- currencyId with an Integer chosen by a fair dice roll. In practice, we really
-- want this to a 'TxOutRef' so we can get actual guarantees from the ledger
-- about this being unique.
type FakeTxOutRef = Integer

-- The validator is parameterized by the public keys of the participants, as well
-- as a reference to a particular UTxO. A minting transaction will be considered
-- valid if it is able to spend that UTxO (automatically ensuring that minting
-- can only happen *once*).
--
-- What it not necessarily transparent here is that, the on-chain code is really
-- made of the curried function '() -> ScriptContext -> Bool', after the first parameter
-- has been partially applied. This is similar to what is called 'closures' in
-- some languages. Fundamentally, the parameter 'FakeTxOutRef' is embedded within the
-- policy and is part of the on-chain code itself!
--
-- The type of minting policy validators is defined in 'Ledger.Typed.Scripts.MonetaryPolicies'
validateMintingPolicy ::
  FakeTxOutRef ->
  -- there is no redeemer type for validating minting policy
  () ->
  ScriptContext ->
  Bool
validateMintingPolicy _outRef _ctx _ctx2 =
  validateMinting || validateBurning
 where
  -- FIXME
  validateBurning =
    True

  -- TODO: In practice, we would do:
  --
  --     let constraints = mustSpendPubKeyOutput outRef
  --      in checkScriptContext @() @() constraints ctx
  validateMinting =
    True

hydraMintingPolicy ::
  FakeTxOutRef ->
  MintingPolicy
hydraMintingPolicy outRef =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . validateMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode outRef

--
-- Helpers
--

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

mustRunContract ::
  forall datum redeemer.
  ToData redeemer =>
  -- (ToData datum, ToData redeemer) =>
  (ValidatorHash, datum) ->
  redeemer ->
  ScriptContext ->
  Bool
--FIXME: we should also control the datum here!
mustRunContract (script, _datum) redeemer ctx =
  case findContractInput script ctx of
    Nothing ->
      False
    Just contractRef ->
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

findUtxo :: TxOutRef -> ScriptContext -> Maybe (TxOutRef, TxOut)
findUtxo ref ctx =
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
