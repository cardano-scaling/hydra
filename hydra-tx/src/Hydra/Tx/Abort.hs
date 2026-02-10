module Hydra.Tx.Abort where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "containers" Data.Map qualified as Map
import "hydra-plutus" Hydra.Contract.Commit qualified as Commit
import "hydra-plutus" Hydra.Contract.Head qualified as Head
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-plutus" Hydra.Contract.Initial qualified as Initial
import "hydra-plutus" Hydra.Contract.MintAction (MintAction (Burn))
import "hydra-plutus" Hydra.Plutus (commitValidatorScript, initialValidatorScript)

import Hydra.Ledger.Cardano.Builder (burnTokens, unsafeBuildTransaction)
import Hydra.Tx (ScriptRegistry (..))
import Hydra.Tx.HeadId (HeadId (..))
import Hydra.Tx.Utils (findStateToken, headTokensFromValue, mkHydraHeadV1TxName)

-- * Creation

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
          defaultTxBodyContent
            & addTxIns ((headInput, headWitness) : initialInputs <> commitInputs)
            & addTxInsReference ([headScriptRef, initialScriptRef] <> [commitScriptRef | not $ null commitInputs]) mempty
            & addTxOuts reimbursedOutputs
            & burnTokens headTokenScript Burn headTokens
            & addTxExtraKeyWits [verificationKeyHash vk]
            & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "AbortTx")
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

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
        mkScriptReference initialScriptRef initialValidatorScript InlineScriptDatum initialRedeemer
  initialScriptRef =
    fst (initialReference scriptRegistry)
  initialRedeemer =
    toScriptData $ Initial.redeemer Initial.ViaAbort

  mkAbortCommit commitInput = (commitInput, abortCommitWitness)
  abortCommitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitValidatorScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitRedeemer =
    toScriptData (Commit.redeemer Commit.ViaAbort)

  reimbursedOutputs = fromCtxUTxOTxOut . snd <$> UTxO.toList committedUTxO

-- * Observation

newtype AbortObservation = AbortObservation {headId :: HeadId}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe AbortObservation
observeAbortTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput >>= \case
    Head.Abort -> pure $ AbortObservation headId
    _ -> Nothing
