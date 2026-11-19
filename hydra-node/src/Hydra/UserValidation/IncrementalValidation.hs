{-# LANGUAGE OverloadedRecordDot #-}

-- | Check Cardano user policies while reusing work from earlier output checks.
--
-- Transaction rules and deposit IDs are checked directly. For rules about the
-- UTxO, we create a 'PolicyContext' with a result and functions for trying the
-- next transaction or adding incoming outputs.
--
-- When the caller reads 'policyResult', we check the UTxO count first, using
-- the map's size. If that limit is exceeded, we skip the output-content checks.
-- Other output rules use a 'Summary', initially built by visiting the starting
-- outputs. It records asset occurrences, counts of asset entries from forbidden
-- policies, and any datum or script-address violations. Only information needed
-- by the configured rules is collected. The summary does not contain the UTxO.
--
-- To try a transaction:
--
-- 1. Look up its spent inputs in the current UTxO and obtain its new outputs.
-- 2. Build the resulting UTxO by removing those inputs and adding those outputs.
-- 3. For each changed output reference, remove the previous output's counts and
--    recorded violations from the summary, then add those of the resulting
--    output. We reuse the information for unchanged outputs.
-- 4. Return a new context whose functions use the resulting UTxO and updated
--    summary. The caller reads its result and decides whether to use it.
--
-- The 'assets' and 'rejectedPolicies' counts in 'Summary' tell us whether an
-- asset or forbidden policy is still present after an output is removed.
-- We subtract that output's counts instead of searching the remaining outputs.
-- When a batch of outputs is added directly, existing UTxO entries stay in place.
-- We update the summary for the supplied output references.
-- A rejected candidate leaves the previous context unchanged. A context can
-- still report a violation; returning a context does not mean its checks passed.
--
-- Deposit admission checks the deposit's ID and its own outputs. For a check
-- of the Head's combined outputs, the caller includes any selected or reserved
-- incoming deposit outputs. They count toward the UTxO limit and undergo the
-- same asset, datum, and address checks as existing outputs. Including them in
-- this policy check does not make them spendable.
--
-- Outgoing decommit outputs remain subject to content checks, but are excluded
-- from the UTxO count. Check each output reference once, even if it appears in
-- both UTxO maps: look it up in the active UTxO first, then in the outgoing
-- decommit UTxO.
--
-- Reusing a returned context avoids checking unchanged outputs again. Creating
-- a fresh context starts from the supplied outputs. Count-only policies need no
-- output summary, and transaction-only policies need no UTxO state here.
-- These policy checks do not replace ledger validation of the transaction.
module Hydra.UserValidation.IncrementalValidation (
  cardanoUserValidation,
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

cardanoUserValidation :: UserValidationRules -> UserValidation Tx
cardanoUserValidation rules =
  UserValidation
    { validateTransaction = preValidateUserTransactionWith rules
    , validateDeposit = \depositTxId ->
        for_ rules.allowedDeposits $ \allowed -> checkDeposit allowed depositTxId
    , utxoValidation =
        UTxOValidation
          { preparePolicyUTxO = prepareCardanoPolicyContext rules
          , localPolicyContext = Nothing
          }
    }

-- | Cached information from checking output contents. The UTxO stays separate.
-- Only fields needed by the configured rules are populated.
--
-- Adding an output increases its asset and forbidden-policy counts; removing
-- it decreases them. An entry is removed when its count reaches zero, so we
-- know when the last occurrence of an asset or forbidden policy disappears.
data Summary = Summary
  { assets :: !(Map AssetId Int)
  -- ^ Number of outputs containing each non-ADA asset. An output containing
  -- ten tokens of one asset contributes one, not ten. Used for the limit on
  -- distinct assets: the number of map entries is the distinct asset count.
  , rejectedPolicies :: !(Map PolicyId Int)
  -- ^ Number of asset entries belonging to each forbidden policy, counted
  -- across outputs. Two different assets from the same forbidden policy in
  -- one output contribute two. The same asset in two outputs also contributes
  -- two. Token quantities do not affect these counts.
  , datumViolations :: !(Map TxIn PolicyViolation)
  -- ^ Outputs whose datum exceeds the byte limit, keyed by output reference.
  , scriptViolations :: !(Map TxIn PolicyViolation)
  -- ^ Outputs with a disallowed script address, keyed by output reference.
  }

emptySummary :: Summary
emptySummary = Summary mempty mempty mempty mempty

prepareCardanoPolicyContext :: UserValidationRules -> UTxO -> UTxO -> PolicyContext Tx
prepareCardanoPolicyContext rules initialUTxO outgoingUTxO
  | needsUTxOValidation = contextFor initialUTxO initialSummary
  | otherwise = unrestricted
 where
  -- Asset restrictions and asset-count limits need the assets in each output.
  needsAssetInspection :: Bool
  needsAssetInspection =
    isJust rules.policyRule
      || isJust rules.maxOutputValueAssets

  -- Datum and address rules also need output contents. The UTxO count uses
  -- only the map's size, so it does not require inspecting outputs.
  needsOutputInspection :: Bool
  needsOutputInspection =
    isJust rules.policyRule
      || isJust rules.maxOutputValueAssets
      || isJust rules.maxDatumSize
      || isJust rules.allowedScriptAddresses

  -- A count limit still needs the UTxO even when no rule needs its contents.
  needsUTxOValidation :: Bool
  needsUTxOValidation =
    isJust rules.policyRule
      || isJust rules.maxOutputValueAssets
      || isJust rules.maxDatumSize
      || isJust rules.allowedScriptAddresses
      || isJust rules.maxUTxOCount

  initialSummary :: Summary
  initialSummary
    | needsOutputInspection = Map.foldlWithKey' addOutput emptySummary (UTxO.unUTxO $ initialUTxO <> outgoingUTxO)
    | otherwise = emptySummary

  -- Transaction-only policies need no UTxO and must not retain a lazy chain of
  -- unused updates in the runtime context.
  unrestricted :: PolicyContext Tx
  unrestricted = PolicyContext (Right ()) (const unrestricted) (const unrestricted)

  allowedPolicies :: Set PolicyId
  allowedPolicies = case rules.policyRule of
    Just ForbidAnyPolicy -> mempty
    Just (AllowOnlyPolicies allowed) -> allowed
    Nothing -> mempty

  contextFor :: UTxO -> Summary -> PolicyContext Tx
  contextFor currentUTxO currentSummary =
    PolicyContext
      { policyResult = checkSummary currentUTxO currentSummary
      , applyPolicyTx = applyTransaction
      , addPolicyUTxO = addIncomingOutputs
      }
   where
    applyTransaction :: Tx -> PolicyContext Tx
    applyTransaction tx =
      let spentInputs = Api.resolveInputsUTxO currentUTxO tx
          createdOutputs = utxoFromTx tx
          resultingUTxO = (currentUTxO `UTxO.difference` spentInputs) <> createdOutputs
          changedReferences = Map.keysSet (UTxO.unUTxO spentInputs) <> Map.keysSet (UTxO.unUTxO createdOutputs)
          updatedSummary = updateChangedOutputs currentUTxO resultingUTxO changedReferences currentSummary
       in contextFor resultingUTxO updatedSummary

    addIncomingOutputs :: UTxO -> PolicyContext Tx
    addIncomingOutputs incomingUTxO =
      let resultingUTxO = currentUTxO <> incomingUTxO
          changedReferences = Map.keysSet (UTxO.unUTxO incomingUTxO)
          updatedSummary = updateChangedOutputs currentUTxO resultingUTxO changedReferences currentSummary
       in contextFor resultingUTxO updatedSummary

  updateChangedOutputs :: UTxO -> UTxO -> Set TxIn -> Summary -> Summary
  updateChangedOutputs previousUTxO resultingUTxO changedReferences summary
    | needsOutputInspection = Set.foldl' updateOutput summary changedReferences
    | otherwise = summary
   where
    updateOutput :: Summary -> TxIn -> Summary
    updateOutput summarySoFar outputReference =
      let summaryWithoutPreviousOutput = case outputAt previousUTxO outputReference of
            Nothing -> summarySoFar
            Just previousOutput -> removeOutput summarySoFar outputReference previousOutput
       in case outputAt resultingUTxO outputReference of
            Nothing -> summaryWithoutPreviousOutput
            Just resultingOutput -> addOutput summaryWithoutPreviousOutput outputReference resultingOutput

  -- Active outputs take precedence over outgoing outputs at the same reference,
  -- matching the left-biased union used by the full validator. Spending an active
  -- output can reveal an outgoing output whose contribution must be restored.
  outputAt :: UTxO -> TxIn -> Maybe (TxOut CtxUTxO)
  outputAt activeUTxO outputReference =
    UTxO.resolveTxIn outputReference activeUTxO <|> UTxO.resolveTxIn outputReference outgoingUTxO

  checkSummary :: UTxO -> Summary -> Either PolicyViolation ()
  checkSummary activeUTxO summary = do
    -- Count first: an oversized UTxO needs no output inspection or summary.
    for_ rules.maxUTxOCount $ \limit -> checkMaxUTxOCount limit activeUTxO
    unless (Map.null summary.rejectedPolicies) $
      Left PolicyAssetsViolation{invalidPolicyIds = Map.keysSet summary.rejectedPolicies, allowedPolicyIds = allowedPolicies}
    for_ rules.maxOutputValueAssets $ \limit -> do
      let assetCount = fromIntegral $ Map.size summary.assets
      when (assetCount > limit) $
        Left MaxOutputValueAssetsExceeded{actualAssetCount = assetCount, maximumAssetCount = limit}
    for_ (Map.lookupMin summary.datumViolations) $ \(_, violation) -> Left violation
    for_ (Map.lookupMin summary.scriptViolations) $ \(_, violation) -> Left violation

  addOutput :: Summary -> TxIn -> TxOut CtxUTxO -> Summary
  addOutput summary outputReference output =
    (updateAssets 1 output summary)
      { datumViolations = recordViolation outputReference datumViolation summary.datumViolations
      , scriptViolations = recordViolation outputReference scriptViolation summary.scriptViolations
      }
   where
    datumViolation :: Maybe PolicyViolation
    datumViolation = do
      limit <- rules.maxDatumSize
      leftToMaybe $ checkMaxDatumSize limit output
    scriptViolation :: Maybe PolicyViolation
    scriptViolation = do
      allowed <- rules.allowedScriptAddresses
      leftToMaybe $ checkAllowedScriptAddresses allowed output

  removeOutput :: Summary -> TxIn -> TxOut CtxUTxO -> Summary
  removeOutput summary outputReference output =
    (updateAssets (-1) output summary)
      { datumViolations = Map.delete outputReference summary.datumViolations
      , scriptViolations = Map.delete outputReference summary.scriptViolations
      }

  updateAssets :: Int -> TxOut CtxUTxO -> Summary -> Summary
  updateAssets countChange output summary
    | needsAssetInspection = Set.foldl' update summary (outputAssets [output])
    | otherwise = summary
   where
    update :: Summary -> AssetId -> Summary
    update summarySoFar asset@(AssetId policyId _) =
      summarySoFar
        { assets = if isJust rules.maxOutputValueAssets then adjustCount countChange asset summarySoFar.assets else summarySoFar.assets
        , rejectedPolicies =
            if isJust rules.policyRule && policyId `Set.notMember` allowedPolicies
              then adjustCount countChange policyId summarySoFar.rejectedPolicies
              else summarySoFar.rejectedPolicies
        }
    update summarySoFar _ = summarySoFar

  adjustCount :: Ord key => Int -> key -> Map key Int -> Map key Int
  adjustCount countChange = Map.alter $ \previousCount ->
    let updatedCount = fromMaybe 0 previousCount + countChange
     in if updatedCount == 0 then Nothing else Just updatedCount

  recordViolation :: TxIn -> Maybe PolicyViolation -> Map TxIn PolicyViolation -> Map TxIn PolicyViolation
  recordViolation outputReference = maybe id (Map.insert outputReference)
