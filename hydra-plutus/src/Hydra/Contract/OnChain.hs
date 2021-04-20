{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.OnChain where

import PlutusPrelude (Generic)
import PlutusTx.Prelude

import Ledger
import Ledger.Constraints (TxConstraints, checkValidatorCtx)
import Ledger.Typed.Scripts (ScriptType (DatumType, RedeemerType))
import Ledger.Value (TokenName (..))
import Plutus.Contract.StateMachine (State (..), Void)
import PlutusTx.IsData.Class (IsData (..))

import qualified Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx

{- HLINT ignore "Use &&" -}

--
-- State-Machine
--

data HydraState
  = Initial
  | Open
PlutusTx.makeLift ''HydraState
PlutusTx.unstableMakeIsData ''HydraState

data HydraInput
  = CollectCom
  deriving (Generic)
PlutusTx.makeLift ''HydraInput
PlutusTx.unstableMakeIsData ''HydraInput

transition ::
  MonetaryPolicyHash ->
  State HydraState ->
  HydraInput ->
  Maybe (SM.TxConstraints Void Void, State HydraState)
transition policyId s i = case (s, i) of
  (state@State{stateData = Initial}, CollectCom) ->
    Just
      ( mempty
      , state{stateData = Open}
      )
  (_, _) -> Nothing
{-# INLINEABLE transition #-}

machine ::
  MonetaryPolicyHash ->
  SM.StateMachine HydraState HydraInput
machine policyId =
  SM.mkStateMachine (transition policyId) isFinal
 where
  isFinal _ = False -- FIXME
{-# INLINEABLE machine #-}

νHydra ::
  MonetaryPolicyHash ->
  Scripts.ValidatorType (SM.StateMachine HydraState HydraInput)
νHydra =
  SM.mkValidator . machine
{-# INLINEABLE νHydra #-}

{- ORMOLU_DISABLE -}
νHydraInstance
  :: MonetaryPolicyHash
  -> Scripts.ScriptInstance (SM.StateMachine HydraState HydraInput)
νHydraInstance policyId = Scripts.validator @(SM.StateMachine HydraState HydraInput)
    ( $$(PlutusTx.compile [|| νHydra ||])
      `PlutusTx.applyCode` PlutusTx.liftCode policyId
    )
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @HydraState @HydraInput
{- ORMOLU_ENABLE -}

νHydraAddress :: MonetaryPolicyHash -> Address
νHydraAddress = Scripts.scriptAddress . νHydraInstance

νHydraHash :: MonetaryPolicyHash -> ValidatorHash
νHydraHash = Scripts.scriptHash . νHydraInstance
{-# INLINEABLE νHydraHash #-}

δCollectCom :: Datum
δCollectCom = Datum (toData Initial)
{-# INLINEABLE δCollectCom #-}

--
-- Participation Tokens
--

type CurrencyId = Integer -- TODO: This should ultimately be a TxOutRef

validateHydraMonetaryPolicy :: CurrencyId -> PolicyCtx -> Bool
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
-- νInitial
--

-- | The Validator 'νInitial' ensures the following: either the output
-- is consumed by
--
-- 1. an SM abort transaction (see below) or
-- 2. a commit transaction, identified by:
--    (a) Having validator 'νCommit' in its only output
--    (b) A signature that verifies as valid with verification key k_i
--    (c) The presence of a single participation token in outputs
νInitial ::
  MonetaryPolicyHash ->
  ValidatorHash ->
  PubKeyHash ->
  () ->
  ValidatorCtx ->
  Bool
νInitial policyId νCommitHash vk () tx =
  consumedByCommit || consumedByAbort
 where
  consumedByAbort = False -- FIXME
  consumedByCommit =
    let utxos = filterInputs (not . hasParticipationToken policyId) tx
     in mustSatisfy @() @PubKeyHash
          [ mustBeSignedBy vk
          , mustCommitUtxos νCommitHash utxos
          , mustForwardParticipationToken policyId vk
          ]
          tx

data Initial
instance Scripts.ScriptType Initial where
  type DatumType Initial = PubKeyHash
  type RedeemerType Initial = ()

{- ORMOLU_DISABLE -}
νInitialInstance
  :: MonetaryPolicyHash
  -> Scripts.ScriptInstance Initial
νInitialInstance policyId = Scripts.validator @Initial
  ($$(PlutusTx.compile [||νInitial||])
      `PlutusTx.applyCode` PlutusTx.liftCode policyId
      `PlutusTx.applyCode` PlutusTx.liftCode (νCommitHash policyId)
  )
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Initial) @(RedeemerType Initial)
{- ORMOLU_ENABLE -}

νInitialAddress :: MonetaryPolicyHash -> Address
νInitialAddress = Scripts.scriptAddress . νInitialInstance

νInitialHash :: MonetaryPolicyHash -> ValidatorHash
νInitialHash = Scripts.scriptHash . νInitialInstance
{-# INLINEABLE νInitialHash #-}

δInitial :: PubKeyHash -> Datum
δInitial = Datum . toData
{-# INLINEABLE δInitial #-}

ρInitial :: Redeemer
ρInitial = Redeemer (toData ())
{-# INLINEABLE ρInitial #-}

--
-- νCommit
--

-- |  To lock outputs for a Hydra head, the ith head member will attach a commit
-- transaction to the i-th output of the initial transaction.
-- Validator 'νCommit' ensures that the commit transaction correctly records the
-- partial UTxO set Ui committed by the party.
--
-- The data field of the output of the commit transaction is
--
--     Ui = makeUTxO(o_1 , . . . , o_m),
--
-- where the o_j are the outputs referenced by the commit transaction’s inputs
-- and makeUTxO stores pairs (out-ref_j , o_j) of outputs o_j with the
-- corresponding output reference out-ref_j .
νCommit ::
  MonetaryPolicyHash ->
  ValidatorHash ->
  [TxOutRef] ->
  PubKeyHash ->
  ValidatorCtx ->
  Bool
νCommit policyId νCollectComHash outs vk tx =
  consumedByCollectCom || consumedByAbort
 where
  consumedByAbort = False -- FIXME
  consumedByCollectCom =
    let utxos = filterInputs (const True) tx
     in mustSatisfy @PubKeyHash @[TxOutRef]
          [ mustCollectCommit νCollectComHash utxos
          , mustForwardParticipationToken policyId vk
          ]
          tx

data Commit
instance Scripts.ScriptType Commit where
  type DatumType Commit = [TxOutRef]
  type RedeemerType Commit = PubKeyHash

{- ORMOLU_DISABLE -}
νCommitInstance
  :: MonetaryPolicyHash
  -> Scripts.ScriptInstance Commit
νCommitInstance policyId = Scripts.validator @Commit
  ($$(PlutusTx.compile [|| νCommit ||])
    `PlutusTx.applyCode` PlutusTx.liftCode policyId
    `PlutusTx.applyCode` PlutusTx.liftCode (νHydraHash policyId)
  )
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Commit) @(RedeemerType Commit)
{- ORMOLU_ENABLE -}

νCommitAddress :: MonetaryPolicyHash -> Address
νCommitAddress = Scripts.scriptAddress . νCommitInstance

νCommitHash :: MonetaryPolicyHash -> ValidatorHash
νCommitHash = Scripts.scriptHash . νCommitInstance
{-# INLINEABLE νCommitHash #-}

δCommit :: [TxOutRef] -> Datum
δCommit = Datum . toData
{-# INLINEABLE δCommit #-}

-- NOTE: We would like this to be INLINEABLE. Yet, behind the scene, 'datumHash'
-- relies on CBOR binary serialisation, which isn't runnable in plutus-core _yet_.
--
-- Question asked here on #plutus:
--
--     https://input-output-rnd.slack.com/archives/C21UF2WVC/p1618818447210100
--
-- If needed _immediately_ we would have to write our own serialiser, which is
-- _doable_ though a bit unpleasant / not recommended (as it would have to match
-- exactly the one from the ledger). Otherwise, fingers crossed for this to be
-- addressed in upcoming releases of Plutus.
δCommitHash :: [TxOutRef] -> DatumHash
δCommitHash =
  datumHash . δCommit

--
-- Helpers
--

-- | Small helper to make constraint validation a little easier to write.
mustSatisfy ::
  forall i o.
  IsData o =>
  [TxConstraints i o] ->
  ValidatorCtx ->
  Bool
mustSatisfy constraints =
  checkValidatorCtx (mconcat constraints)
{-# INLINEABLE mustSatisfy #-}

-- | Re-exported to make code declaring constraints a bit more uniform.
mustBeSignedBy ::
  forall i o.
  PubKeyHash ->
  TxConstraints i o
mustBeSignedBy = Constraints.mustBeSignedBy
{-# INLINEABLE mustBeSignedBy #-}

mustForwardParticipationToken ::
  forall i o.
  MonetaryPolicyHash ->
  PubKeyHash ->
  TxConstraints i o
mustForwardParticipationToken policyId vk =
  let participationToken = mkParticipationToken policyId vk
   in mconcat
        [ Constraints.mustSpendAtLeast participationToken
        , Constraints.mustProduceAtLeast participationToken
        ]
{-# INLINEABLE mustForwardParticipationToken #-}

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
mustCommitUtxos ::
  forall i o.
  ValidatorHash ->
  [(TxOutRef, Value)] ->
  TxConstraints i o
mustCommitUtxos νCommitHash utxos =
  let value = foldMap snd utxos
      datum = δCommit $ fst <$> utxos
   in Constraints.mustPayToOtherScript νCommitHash datum value
{-# INLINEABLE mustCommitUtxos #-}

mustCollectCommit ::
  forall i o.
  ValidatorHash ->
  [(TxOutRef, Value)] ->
  TxConstraints i o
mustCollectCommit νCollectComHash utxos =
  let value = foldMap snd utxos
   in Constraints.mustPayToOtherScript νCollectComHash δCollectCom value
{-# INLINEABLE mustCollectCommit #-}

mkParticipationToken :: MonetaryPolicyHash -> PubKeyHash -> Value
mkParticipationToken policyId vk =
  Value.singleton (Value.mpsSymbol policyId) (mkParticipationTokenName vk) 1
{-# INLINEABLE mkParticipationToken #-}

mkParticipationTokenName :: PubKeyHash -> TokenName
mkParticipationTokenName =
  TokenName . getPubKeyHash
{-# INLINEABLE mkParticipationTokenName #-}

filterInputs :: (TxInInfo -> Bool) -> ValidatorCtx -> [(TxOutRef, Value)]
filterInputs predicate =
  mapMaybe fn . txInfoInputs . valCtxTxInfo
 where
  fn info
    | predicate info = Just (txInInfoOutRef info, txInInfoValue info)
    | otherwise = Nothing
{-# INLINEABLE filterInputs #-}

hasParticipationToken :: MonetaryPolicyHash -> TxInInfo -> Bool
hasParticipationToken policyId input =
  let currency = Value.mpsSymbol policyId
   in currency `elem` Value.symbols (txInInfoValue input)
{-# INLINEABLE hasParticipationToken #-}
