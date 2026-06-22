{-# LANGUAGE OverloadedRecordDot #-}

-- | Differential test (Tier 2) for the μHead minting policy (@HeadTokens.validateTokensMinting@): run
-- the Agda-extracted decidable reference checker ('Hydra.Agda.Reference.checkInit') and the real
-- Plutus minting policy on the same init transactions, and assert they agree on the decidable layer.
--
-- Property: for the healthy init tx and every generated init mutation, @reference-rejects ⇒
-- validator-rejects@. The reference mirrors the decidable token-COUNT conjunct (the tx mints exactly
-- @n + 1@ tokens of the head policy: one ST + one PT per party, the policy's @checkNumberOfTokens@) AND
-- token PLACEMENT (the ST is present and the head output carries exactly @n + 1@ head-policy tokens),
-- proved to reflect @initValid@ in spec/src/Hydra/Protocol/ReferenceBridge.agda. So @RemovePTsFromHead@
-- (a misplacement the policy catches via @MissingPTs@) is now ASSERTED, not abstained. The seed-spent
-- check and the datum binding still need lookups the spec abstracts over and stay mocked (@const True@),
-- so mutations the policy catches only via those (e.g. @MutateDropSeedInput@) still abstain here.
module Hydra.Tx.Contract.InitDifferential (spec) where

import Hydra.Prelude

import Data.List (nub)
import Data.Maybe (fromJust)
import GHC.IsList qualified as IsList
import Hydra.Agda.Reference qualified as Ref
import Hydra.Cardano.Api (
  AssetId (..),
  Quantity (..),
  Tx,
  UTxO,
  filterValue,
  getTxBody,
  getTxBodyContent,
  modifyTxOutValue,
  txMintValue,
  txMintValueToValue,
  txOutValue,
  txOuts',
 )
import Hydra.Cardano.Api.ScriptData (fromScriptData, txOutScriptData)
import Hydra.Contract.HeadState qualified as HS
import Hydra.Tx.Contract.Init (genInitMutation, healthyInitTx)
import Hydra.Tx.Utils (hydraHeadV2AssetName)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), addPTWithQuantity, applyMutation)
import Test.QuickCheck (Property, forAll, property, (===))

-- | Does the real Plutus validator accept @(tx, utxo)@ (phase-2 success, no script error)?
validatorAccepts :: (Tx, UTxO) -> Bool
validatorAccepts (tx, utxo) =
  case evaluateTx tx utxo of
    Right report -> all isRight report
    Left _ -> False

-- | The extracted reference verdict on an init @(tx, utxo)@, or 'Nothing' if the head datum cannot be
-- read or the tx does not mint under exactly one policy — in which case the reference abstains.
initRefVerdict :: (Tx, UTxO) -> Maybe Bool
initRefVerdict (tx, _utxo) = do
  headOut <- txOuts' tx !!? 0
  st <- fromScriptData =<< txOutScriptData headOut
  HS.Open od <- Just (st :: HS.State)
  let n = length od.parties
      minted = IsList.toList (txMintValueToValue (txMintValue (getTxBodyContent (getTxBody tx))))
  -- Mirror the policy's @mintedTokenCount@: the SUM of the head policy's mint quantities. The head
  -- policy is the (unique) policy minted at init = @ownCurrencySymbol@ (which @checkDatum@ ties to the
  -- datum @headId@); reading it from the mint, not the datum, abstains cleanly on datum-only mutations.
  case nub [pid | (AssetId pid _, _) <- minted] of
    [policyId] ->
      let mintedCount = sum [q | (AssetId pid _, Quantity q) <- minted, pid == policyId]
          -- PLACEMENT: read the head output (index 0) value. `stQty` is the ST quantity there;
          -- `headTokenCount` is the number of DISTINCT head-policy tokens there (1 ST + n PTs = n+1).
          headOutAssets = [(an, q) | (AssetId pid an, Quantity q) <- IsList.toList (txOutValue headOut), pid == policyId]
          stQty = sum [q | (an, q) <- headOutAssets, an == hydraHeadV2AssetName]
          headTokenCount = fromIntegral (length headOutAssets)
       in Just
            ( Ref.checkInit
                (Ref.mkOpsInit (const True))
                (Ref.MkMintIO (fromIntegral n) mintedCount stQty headTokenCount)
            )
    _ -> Nothing

-- | The healthy init tx with one extra participation token minted, so the @n + 1@ count check fails:
-- both the reference and the policy (@WrongNumberOfTokensMinted@) must reject. Demonstrates the count
-- conjunct is live (non-vacuous), not merely abstained.
extraTokenInitTx :: (Tx, UTxO)
extraTokenInitTx =
  applyMutation
    (addPTWithQuantity (fst healthyInitTx) 1 `generateWith` 42)
    healthyInitTx

-- | The healthy init with the participation tokens stripped from the head output (the mint is
-- unchanged). The mint COUNT is still @n + 1@ and the ST is still placed, so the old count-only
-- reference accepted it — but the head output now carries only the ST (@headTokenCount = 1 ≠ n + 1@),
-- so the new PLACEMENT conjunct rejects, as does the policy (@MissingPTs@). Demonstrates placement is
-- live (non-vacuous): minting n+1 tokens but not placing them all in the head output is now caught.
removePTsInitTx :: (Tx, UTxO)
removePTsInitTx =
  applyMutation (ChangeOutput 0 (modifyTxOutValue (filterValue (not . isPT)) headTxOut)) healthyInitTx
 where
  headTxOut = fromJust (txOuts' (fst healthyInitTx) !!? 0)
  isPT = \case
    AssetId _ an -> an /= hydraHeadV2AssetName
    _ -> False

spec :: Spec
spec = parallel $ do
  prop "reference accepts the healthy init tx" $
    initRefVerdict healthyInitTx === Just True

  prop "validator accepts the healthy init tx" $
    validatorAccepts healthyInitTx === True

  prop "reference rejects an init minting an extra token" $
    initRefVerdict extraTokenInitTx === Just False

  prop "validator also rejects the extra-token init" $
    validatorAccepts extraTokenInitTx === False

  prop "reference rejects an init not placing the PTs in the head output" $
    initRefVerdict removePTsInitTx === Just False

  prop "validator also rejects the missing-PTs init" $
    validatorAccepts removePTsInitTx === False

  prop "differential: reference-reject ⇒ validator-reject (Init mutations)" $
    forAll (genInitMutation healthyInitTx) $ \SomeMutation{mutation} ->
      differential (applyMutation mutation healthyInitTx)
 where
  differential :: (Tx, UTxO) -> Property
  differential m =
    case initRefVerdict m of
      Just False -> validatorAccepts m === False
      _ -> property True
