{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.OnChain where

import PlutusPrelude (Generic)
import PlutusTx.Prelude hiding (remainder)

import Data.Aeson (ToJSON)
import Ledger hiding (out, value)
import Ledger.Constraints (TxConstraints, checkScriptContext)
import Ledger.Typed.Scripts (ScriptType (DatumType, RedeemerType))
import Ledger.Value (TokenName (..))
import PlutusTx.AssocMap (Map)
import PlutusTx.IsData.Class (IsData (..))

import Ledger.Constraints.TxConstraints (
  mustBeSignedBy,
  mustForgeCurrency,
  mustPayToOtherScript,
  mustPayToTheScript,
  mustProduceAtLeast,
  mustSpendAtLeast,
  mustSpendPubKeyOutput,
 )

import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map

--
-- State-Machine
--

data HydraState
  = Initial [PubKeyHash]
  | Open [TxOut]
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)
PlutusTx.makeLift ''HydraState
PlutusTx.unstableMakeIsData ''HydraState

data HydraInput
  = CollectCom
  deriving (Generic, Show)
PlutusTx.makeLift ''HydraInput
PlutusTx.unstableMakeIsData ''HydraInput

data HeadParameters = HeadParameters
  { participants :: [PubKeyHash]
  , policyId :: MonetaryPolicyHash
  }
PlutusTx.makeLift ''HeadParameters

hydraValidator ::
  HeadParameters ->
  HydraState ->
  HydraInput ->
  ScriptContext ->
  Bool
hydraValidator (HeadParameters vks policyId) s i tx =
  case (s, i) of
    (Initial vks', CollectCom) ->
      let committed = snd <$> filterInputs (hasParticipationToken policyId) tx
       in mustSatisfy @HydraInput @HydraState
            [ mustPayToTheScript (Open committed) (foldMap txOutValue committed)
            , foldMap (mustForwardParticipationToken policyId) vks
            ]
            tx
            && tx `mustBeSignedByOneOf` vks
            && all (`elem` vks) vks'
            && all (`elem` vks') vks
    _ -> False

data Hydra
instance Scripts.ScriptType Hydra where
  type DatumType Hydra = HydraState
  type RedeemerType Hydra = HydraInput

{- ORMOLU_DISABLE -}
hydra
  :: HeadParameters
  -> Scripts.ScriptInstance Hydra
hydra params = Scripts.validator @Hydra
    ( $$(PlutusTx.compile [|| hydraValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode params
    )
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @(DatumType Hydra) @(RedeemerType Hydra)
{- ORMOLU_ENABLE -}

hydraAddress :: HeadParameters -> Address
hydraAddress = Scripts.scriptAddress . hydra

hydraHash :: HeadParameters -> ValidatorHash
hydraHash = Scripts.scriptHash . hydra
{-# INLINEABLE hydraHash #-}

mkStateOpen :: [TxOut] -> HydraState
mkStateOpen = Open
{-# INLINEABLE mkStateOpen #-}

--
-- Participation Tokens
--

type CurrencyId = Integer -- TODO: This should ultimately be a TxOutRef

validateHydraMonetaryPolicy :: CurrencyId -> ScriptContext -> Bool
validateHydraMonetaryPolicy _ _ = True

{- ORMOLU_DISABLE -}
hydraMonetaryPolicy :: CurrencyId -> MonetaryPolicy
hydraMonetaryPolicy currencyId = mkMonetaryPolicyScript $
    $$(PlutusTx.compile
      [||Scripts.wrapMonetaryPolicy . validateHydraMonetaryPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode currencyId
{- ORMOLU_ENABLE -}

hydraMonetaryPolicyHash :: CurrencyId -> MonetaryPolicyHash
hydraMonetaryPolicyHash = monetaryPolicyHash . hydraMonetaryPolicy

--
-- initial
--

-- | The Validator 'initial' ensures the following: either the output
-- is consumed by
--
-- 1. an SM abort transaction, identified by:
--    (a) Burning all participation tokens of all participants
-- 2. a commit transaction, identified by:
--    (a) Having validator 'commit' in its only output
--    (b) A signature that verifies as valid with verification key k_i
--    (c) The presence of a single participation token in outputs
--
-- TODO: According to the paper, this should also enforce that the transaction
-- only have a single output. Not only is this not possible to express with the
-- current 'TxConstraints' API, but also is this quite unpractical. In fact,
-- wallets will have to balance a transaction for fee, using extra inputs and
-- causing in most cases, a resulting change output.
--
-- I'd like therefore to challenge that 'single output' constraint with the
-- researchers. IMO, It would suffice to pay a certain amount to the commit
-- script, so long as this amount corresponds exactly to the balance of the UTxO
-- committed.
initialValidator ::
  HeadParameters ->
  ValidatorHash ->
  PubKeyHash ->
  TxOutRef ->
  ScriptContext ->
  Bool
initialValidator (HeadParameters vks policyId) commitScript vk ref tx =
  consumedByCommit || consumedByAbort
 where
  consumedByAbort =
    -- TODO
    -- mustSatisfy @() @PubKeyHash
    --   [ foldMap (mustBurnParticipationToken policyId) vks
    --   ]
    --   tx
    False

  consumedByCommit =
    case findUtxo ref tx of
      Just utxo ->
        mustSatisfy @() @PubKeyHash
          [ mustBeSignedBy vk
          , mustSpendPubKeyOutput (fst utxo)
          , mustForwardParticipationToken policyId vk
          , mustPayToOtherScript
              commitScript
              (commitDatum utxo)
              (txOutValue (snd utxo) <> mkParticipationToken policyId vk)
          ]
          tx
      Nothing ->
        False

data Initial
instance Scripts.ScriptType Initial where
  type DatumType Initial = PubKeyHash
  type RedeemerType Initial = TxOutRef

{- ORMOLU_DISABLE -}
initial
  :: HeadParameters
  -> Scripts.ScriptInstance Initial
initial policyId = Scripts.validator @Initial
  ($$(PlutusTx.compile [|| initialValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode policyId
      `PlutusTx.applyCode` PlutusTx.liftCode (commitHash policyId)
  )
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Initial) @(RedeemerType Initial)
{- ORMOLU_ENABLE -}

initialAddress :: HeadParameters -> Address
initialAddress = Scripts.scriptAddress . initial

initialHash :: HeadParameters -> ValidatorHash
initialHash = Scripts.scriptHash . initial
{-# INLINEABLE initialHash #-}

--
-- commit
--

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
  HeadParameters ->
  ValidatorHash ->
  (TxOutRef, TxOut) ->
  () ->
  ScriptContext ->
  Bool
commitValidator (HeadParameters _vks policyId) hydraScript (_outRef, out) () tx =
  consumedByCollectCom || consumedByAbort
 where
  consumedByAbort =
    mustReimburse out tx
  consumedByCollectCom =
    let committed = snd <$> filterInputs (hasParticipationToken policyId) tx
     in mustSatisfy @PubKeyHash @()
          [ mustPayToOtherScript
              hydraScript
              (asDatum $ Open committed)
              (foldMap txOutValue committed)
          ]
          tx

data Commit
instance Scripts.ScriptType Commit where
  type DatumType Commit = (TxOutRef, TxOut)
  type RedeemerType Commit = ()

{- ORMOLU_DISABLE -}
commit
  :: HeadParameters
  -> Scripts.ScriptInstance Commit
commit params = Scripts.validator @Commit
  ($$(PlutusTx.compile [|| commitValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode params
    `PlutusTx.applyCode` PlutusTx.liftCode (hydraHash params)
  )
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Commit) @(RedeemerType Commit)
{- ORMOLU_ENABLE -}

commitAddress :: HeadParameters -> Address
commitAddress = Scripts.scriptAddress . commit

commitHash :: HeadParameters -> ValidatorHash
commitHash = Scripts.scriptHash . commit
{-# INLINEABLE commitHash #-}

commitDatum :: (TxOutRef, TxOut) -> Datum
commitDatum = asDatum
{-# INLINEABLE commitDatum #-}

--
-- Helpers
--

-- | Small helper to make constraint validation a little easier to write.
mustSatisfy ::
  forall i o.
  IsData o =>
  [TxConstraints i o] ->
  ScriptContext ->
  Bool
mustSatisfy constraints =
  checkScriptContext (mconcat constraints)
{-# INLINEABLE mustSatisfy #-}

mustBeSignedByOneOf ::
  ScriptContext ->
  [PubKeyHash] ->
  Bool
mustBeSignedByOneOf tx vks =
  or ((`checkScriptContext` tx) . mustBeSignedBy @() @() <$> vks)
{-# INLINEABLE mustBeSignedByOneOf #-}

mustForwardParticipationToken ::
  forall i o.
  MonetaryPolicyHash ->
  PubKeyHash ->
  TxConstraints i o
mustForwardParticipationToken policyId vk =
  let participationToken = mkParticipationToken policyId vk
   in mconcat
        [ mustSpendAtLeast participationToken
        , mustProduceAtLeast participationToken
        ]
{-# INLINEABLE mustForwardParticipationToken #-}

mustBurnParticipationToken ::
  forall i o.
  MonetaryPolicyHash ->
  PubKeyHash ->
  TxConstraints i o
mustBurnParticipationToken policyId vk =
  let assetName = mkParticipationTokenName vk
   in mconcat
        [ mustForgeCurrency policyId assetName (-1)
        ]
{-# INLINEABLE mustBurnParticipationToken #-}

mustReimburse ::
  TxOut ->
  ScriptContext ->
  Bool
mustReimburse out =
  elem out . txInfoOutputs . scriptContextTxInfo
{-# INLINEABLE mustReimburse #-}

mkParticipationToken :: MonetaryPolicyHash -> PubKeyHash -> Value
mkParticipationToken policyId vk =
  Value.singleton (Value.mpsSymbol policyId) (mkParticipationTokenName vk) 1
{-# INLINEABLE mkParticipationToken #-}

mkParticipationTokenName :: PubKeyHash -> TokenName
mkParticipationTokenName =
  TokenName . getPubKeyHash
{-# INLINEABLE mkParticipationTokenName #-}

filterInputs :: (TxInInfo -> Bool) -> ScriptContext -> [(TxOutRef, TxOut)]
filterInputs predicate =
  mapMaybe fn . txInfoInputs . scriptContextTxInfo
 where
  fn info
    | predicate info = Just (txInInfoOutRef info, txInInfoResolved info)
    | otherwise = Nothing
{-# INLINEABLE filterInputs #-}

hasParticipationToken :: MonetaryPolicyHash -> TxInInfo -> Bool
hasParticipationToken policyId input =
  let currency = Value.mpsSymbol policyId
   in currency `elem` symbols (txOutValue $ txInInfoResolved input)
{-# INLINEABLE hasParticipationToken #-}

findUtxo :: TxOutRef -> ScriptContext -> Maybe (TxOutRef, TxOut)
findUtxo ref tx =
  case filterInputs (\input -> ref == txInInfoOutRef input) tx of
    [utxo] -> Just utxo
    _ -> Nothing
{-# INLINEABLE findUtxo #-}

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

asDatum :: IsData a => a -> Datum
asDatum = Datum . toData
{-# INLINEABLE asDatum #-}

asRedeemer :: IsData a => a -> Redeemer
asRedeemer = Redeemer . toData
{-# INLINEABLE asRedeemer #-}
