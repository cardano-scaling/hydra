module Hydra.Tx.Abort where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.MintAction (MintAction (Burn))
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  addReferenceInputs,
  burnTokens,
  emptyTxBody,
  unsafeBuildTransaction,
 )
import Hydra.Plutus (commitValidatorScript)
import Hydra.Tx (ScriptRegistry (..))
import Hydra.Tx.Utils (headTokensFromValue)

data AbortTxError
  = OverlappingInputs
  | CannotFindHeadOutputToAbort
  deriving stock (Show)

-- | Create transaction which aborts a head by spending the Head output and all
-- other "initial" outputs.
abortTx ::
  -- | Committed UTxOs to reimburse.
  UTxO ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Script for monetary policy to burn tokens
  PlutusScript ->
  -- | Data needed to spend the initial output sent to each party to the Head.
  -- Should contain the PT and is locked by initial script.
  Map TxIn (TxOut CtxUTxO) ->
  -- | Data needed to spend commit outputs.
  -- Should contain the PT and is locked by commit script.
  Map TxIn (TxOut CtxUTxO) ->
  Either AbortTxError Tx
abortTx committedUTxO scriptRegistry vk (headInput, initialHeadOutput) headTokenScript initialsToAbort commitsToAbort
  | isJust (lookup headInput initialsToAbort) =
      Left OverlappingInputs
  | otherwise =
      Right $
        unsafeBuildTransaction $
          emptyTxBody
            & addInputs ((headInput, headWitness) : initialInputs <> commitInputs)
            & addReferenceInputs [initialScriptRef, commitScriptRef, headScriptRef]
            & addOutputs reimbursedOutputs
            & burnTokens headTokenScript Burn headTokens
            & addExtraRequiredSigners [verificationKeyHash vk]
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headRedeemer =
    toScriptData Head.Abort

  initialInputs = mkAbortInitial <$> Map.keys initialsToAbort

  commitInputs = mkAbortCommit <$> Map.keys commitsToAbort

  headTokens =
    headTokensFromValue headTokenScript $
      mconcat
        [ txOutValue initialHeadOutput
        , foldMap txOutValue initialsToAbort
        , foldMap txOutValue commitsToAbort
        ]

  mkAbortInitial initialInput = (initialInput, abortInitialWitness)
  abortInitialWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference initialScriptRef initialScript InlineScriptDatum initialRedeemer
  initialScriptRef =
    fst (initialReference scriptRegistry)
  initialScript =
    fromPlutusScript @PlutusScriptV2 Initial.validatorScript
  initialRedeemer =
    toScriptData $ Initial.redeemer Initial.ViaAbort

  mkAbortCommit commitInput = (commitInput, abortCommitWitness)
  abortCommitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitScript =
    fromPlutusScript @PlutusScriptV2 commitValidatorScript
  commitRedeemer =
    toScriptData (Commit.redeemer Commit.ViaAbort)

  reimbursedOutputs = toTxContext . snd <$> UTxO.pairs committedUTxO
