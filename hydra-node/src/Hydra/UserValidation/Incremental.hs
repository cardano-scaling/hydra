{-# LANGUAGE OverloadedRecordDot #-}

-- | Derived policy state. Only affected output references are inspected when
-- advancing a context; unchanged outputs retain their contribution.
module Hydra.UserValidation.Incremental (
  incrementalCardanoUserValidation,
  prepareCardanoPolicyContext,
) where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Cardano.Api (AssetId (..), CtxUTxO, PolicyId, Tx, TxIn, TxOut, UTxO)
import Hydra.Cardano.Api.UTxO qualified as Api
import Hydra.Tx (utxoFromTx)
import Hydra.UserValidationRules

incrementalCardanoUserValidation :: UserValidationRules -> UserValidation Tx
incrementalCardanoUserValidation rules =
  (cardanoUserValidation rules)
    { incrementalValidation =
        Just
          IncrementalValidation
            { preparePolicyUTxO = prepareCardanoPolicyContext rules
            , localPolicyContext = Nothing
            }
    }

data Summary = Summary
  { assets :: !(Map AssetId Int)
  , rejectedPolicies :: !(Map PolicyId Int)
  , datumViolations :: !(Map TxIn PolicyViolation)
  , scriptViolations :: !(Map TxIn PolicyViolation)
  }

emptySummary :: Summary
emptySummary = Summary mempty mempty mempty mempty

prepareCardanoPolicyContext :: UserValidationRules -> UTxO -> UTxO -> PolicyContext Tx
prepareCardanoPolicyContext rules initial outgoing
  | not scansOutputs && isNothing rules.maxUTxOCount = unrestricted
  | otherwise =
      context initial $
        if scansOutputs
          then Map.foldlWithKey' addOutput emptySummary (UTxO.unUTxO $ initial <> outgoing)
          else emptySummary
 where
  -- Transaction-only policies need no UTxO and must not retain a lazy chain of
  -- unused updates in the runtime context.
  unrestricted :: PolicyContext Tx
  unrestricted = PolicyContext (Right ()) (const unrestricted) (const unrestricted)

  scansAssets = isJust rules.policyRule || isJust rules.maxOutputValueAssets
  scansOutputs = scansAssets || isJust rules.maxDatumSize || isJust rules.allowedScriptAddresses

  allowedPolicies = case rules.policyRule of
    Just ForbidAnyPolicy -> mempty
    Just (AllowOnlyPolicies allowed) -> allowed
    Nothing -> mempty

  context active summary =
    PolicyContext
      { policyResult = checkSummary active summary
      , applyPolicyTx = \tx ->
          let spent = Api.resolveInputsUTxO active tx
              created = utxoFromTx tx
           in advance
                ((active `UTxO.difference` spent) <> created)
                (Map.keysSet (UTxO.unUTxO spent) <> Map.keysSet (UTxO.unUTxO created))
      , addPolicyUTxO = \incoming ->
          advance (active <> incoming) (Map.keysSet $ UTxO.unUTxO incoming)
      }
   where
    -- Look up the effective output on both sides. This also handles overlapping
    -- references and an active output shadowing an outgoing one, exactly like
    -- the left-biased UTxO union used by the full validator.
    advance next changed =
      context next $
        if scansOutputs
          then Set.foldl' updateOutput summary changed
          else summary
     where
      effective utxo ref = UTxO.resolveTxIn ref utxo <|> UTxO.resolveTxIn ref outgoing
      updateOutput acc ref =
        let removed = maybe acc (removeOutput acc ref) (effective active ref)
         in maybe removed (addOutput removed ref) (effective next ref)

  checkSummary active Summary{assets, rejectedPolicies, datumViolations, scriptViolations} = do
    for_ rules.maxUTxOCount $ \limit -> checkMaxUTxOCount limit active
    unless (Map.null rejectedPolicies) $
      Left PolicyAssetsViolation{invalidPolicyIds = Map.keysSet rejectedPolicies, allowedPolicyIds = allowedPolicies}
    for_ rules.maxOutputValueAssets $ \limit -> do
      let count = fromIntegral $ Map.size assets
      when (count > limit) $
        Left MaxOutputValueAssetsExceeded{actualAssetCount = count, maximumAssetCount = limit}
    for_ (Map.lookupMin datumViolations) $ \(_, violation) -> Left violation
    for_ (Map.lookupMin scriptViolations) $ \(_, violation) -> Left violation

  addOutput :: Summary -> TxIn -> TxOut CtxUTxO -> Summary
  addOutput summary ref output =
    (updateAssets 1 output summary)
      { datumViolations = recordViolation ref (rules.maxDatumSize >>= \limit -> leftToMaybe $ checkMaxDatumSize limit output) summary.datumViolations
      , scriptViolations = recordViolation ref (rules.allowedScriptAddresses >>= \allowed -> leftToMaybe $ checkAllowedScriptAddresses allowed output) summary.scriptViolations
      }

  removeOutput :: Summary -> TxIn -> TxOut CtxUTxO -> Summary
  removeOutput summary ref output =
    (updateAssets (-1) output summary)
      { datumViolations = Map.delete ref summary.datumViolations
      , scriptViolations = Map.delete ref summary.scriptViolations
      }

  updateAssets :: Int -> TxOut CtxUTxO -> Summary -> Summary
  updateAssets amount output summary
    | scansAssets = Set.foldl' update summary (outputAssets [output])
    | otherwise = summary
   where
    update acc asset@(AssetId policy _) =
      acc
        { assets = if isJust rules.maxOutputValueAssets then adjustCount amount asset acc.assets else acc.assets
        , rejectedPolicies =
            if isJust rules.policyRule && policy `Set.notMember` allowedPolicies
              then adjustCount amount policy acc.rejectedPolicies
              else acc.rejectedPolicies
        }
    update acc _ = acc

  adjustCount :: Ord key => Int -> key -> Map key Int -> Map key Int
  adjustCount amount = Map.alter $ \previous ->
    let count = fromMaybe 0 previous + amount
     in if count == 0 then Nothing else Just count

  recordViolation :: TxIn -> Maybe PolicyViolation -> Map TxIn PolicyViolation -> Map TxIn PolicyViolation
  recordViolation ref = maybe id (Map.insert ref)
