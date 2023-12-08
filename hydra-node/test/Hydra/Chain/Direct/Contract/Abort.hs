{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Data.Map qualified as Map
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Gen (genForParty)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeMintedTokens,
  changeMintedValueQuantityFrom,
  isHeadOutput,
  removePTFromMintedValue,
  replacePolicyIdWith,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (
  abortTx,
  hydraHeadV1AssetName,
  mkHeadOutputInitial,
 )
import Hydra.Chain.Direct.TxSpec (genAbortableOutputs)
import Hydra.ContestationPeriod (toChain)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.CommitError (CommitError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Contract.HeadTokensError (HeadTokensError (..))
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.InitialError (InitialError (STNotBurned))
import Hydra.Ledger.Cardano (genAddressInEra, genVerificationKey)
import Hydra.Party (Party, partyToChain)
import Test.Hydra.Fixture (cperiod)
import Test.QuickCheck (Property, choose, counterexample, elements, oneof, shuffle, suchThat)

--
-- AbortTx
--

healthyAbortTx :: HasCallStack => (Tx, UTxO)
healthyAbortTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (healthyHeadInput, toUTxOContext headOutput)
      <> UTxO (Map.fromList healthyInitials)
      <> UTxO (Map.fromList (map (\(i, o, _) -> (i, o)) healthyCommits))
      <> registryUTxO scriptRegistry

  tx =
    either (error . show) id $
      abortTx
        committedUTxO
        scriptRegistry
        somePartyCardanoVerificationKey
        (healthyHeadInput, toUTxOContext headOutput)
        headTokenScript
        (Map.fromList healthyInitials)
        (Map.fromList (map (\(i, o, _) -> (i, o)) healthyCommits))

  committedUTxO = foldMap (\(_, _, u) -> u) healthyCommits

  scriptRegistry = genScriptRegistry `generateWith` 42

  somePartyCardanoVerificationKey = flip generateWith 42 $ do
    genForParty genVerificationKey <$> elements healthyParties

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput = mkHeadOutputInitial testNetworkId testSeedInput healthyHeadParameters

healthyHeadInput :: TxIn
healthyHeadInput = generateWith arbitrary 42

healthyHeadParameters :: HeadParameters
healthyHeadParameters =
  HeadParameters
    { contestationPeriod = cperiod
    , parties = healthyParties
    }

healthyInitials :: [(TxIn, TxOut CtxUTxO)]
healthyCommits :: [(TxIn, TxOut CtxUTxO, UTxO)]
(healthyInitials, healthyCommits) =
  -- TODO: Refactor this to be an AbortTx generator because we actually want
  -- to test healthy abort txs with varied combinations of inital and commit
  -- outputs
  generateWith (genAbortableOutputs healthyParties `suchThat` thereIsTwoEach) 42
 where
  thereIsTwoEach (is, cs) = length is >= 2 && length cs >= 2

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 4]
  ]

propHasInitial :: (Tx, UTxO) -> Property
propHasInitial (_, utxo) =
  any paysToInitialScript utxo
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Initial Script: " <> show addr)
 where
  addr = mkScriptAddress @PlutusScriptV2 testNetworkId (fromPlutusScript Initial.validatorScript)
  paysToInitialScript txOut =
    txOutAddress txOut == addr

propHasCommit :: (Tx, UTxO) -> Property
propHasCommit (_, utxo) =
  any paysToCommitScript utxo
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Commit Script: " <> show addr)
 where
  addr = mkScriptAddress @PlutusScriptV2 testNetworkId (fromPlutusScript Commit.validatorScript)
  paysToCommitScript txOut =
    txOutAddress txOut == addr

data AbortMutation
  = -- | Add one more party to the hydra keys. This is essentialy the same as
    -- not collecting all inputs.
    MutateParties
  | -- | Not collect one committed UTxO by removing the input and not burn the
    -- corresponding PT.
    DropCollectedInput
  | -- | Not reimburse one of the parties.
    DropOneCommitOutput
  | -- | Burning one PT more. This should be an impossible situation, but it is
    -- tested nontheless.
    BurnOneTokenMore
  | -- | Meant to test that the minting policy is burning all PTs present in tx
    MutateThreadTokenQuantity
  | -- | Check an arbitrary key cannot authenticate abort.
    MutateRequiredSigner
  | -- | Use a different head output to abort.
    MutateUseDifferentHeadToAbort
  | -- | Spend some abortable output from a different Head e.g. replace a commit
    -- by another commit from a different Head.
    UseInputFromOtherHead
  | -- | Re-ordering outputs would not be a big deal, but it is still prevented.
    ReorderCommitOutputs
  | -- | Only burning should be allowed in abort (by the minting policy).
    MintOnAbort
  | -- | Not spend from v_head and also not burn anything to extract value.
    ExtractValue
  | -- | State token is not burned
    DoNotBurnST
  | -- | Here we want to check that the initial validator also fails on abort.
    DoNotBurnSTInitial
  deriving stock (Generic, Show, Enum, Bounded)

genAbortMutation :: (Tx, UTxO) -> Gen SomeMutation
genAbortMutation (tx, utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode BurntTokenNumberMismatch) MutateParties . ChangeInputHeadDatum <$> do
        moreParties <- (: healthyParties) <$> arbitrary
        c <- arbitrary
        pure $
          Head.Initial
            c
            (partyToChain <$> moreParties)
            (toPlutusCurrencySymbol $ headPolicyId testSeedInput)
            (toPlutusTxOutRef testSeedInput)
    , SomeMutation (Just $ toErrorCode BurntTokenNumberMismatch) DropCollectedInput <$> do
        let abortableInputs = UTxO.pairs $ UTxO.filter (not . isHeadOutput) (resolveInputsUTxO utxo tx)
        (toDropTxIn, toDropTxOut) <- elements abortableInputs
        pure $
          Changes
            [ RemoveInput toDropTxIn
            , ChangeMintedValue $ removePTFromMintedValue toDropTxOut tx
            ]
    , SomeMutation (Just $ toErrorCode ReimbursedOutputsDontMatch) DropOneCommitOutput . RemoveOutput <$> choose (0, fromIntegral (length (txOuts' tx) - 1))
    , SomeMutation (Just $ toErrorCode BurntTokenNumberMismatch) MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx (-1)
    , SomeMutation (Just $ toErrorCode BurntTokenNumberMismatch) BurnOneTokenMore <$> addPTWithQuantity tx (-1)
    , SomeMutation (Just $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (Just $ toErrorCode BurntTokenNumberMismatch) MutateUseDifferentHeadToAbort <$> do
        mutatedSeed <- arbitrary `suchThat` (/= testSeedInput)
        pure $
          ChangeInputHeadDatum
            Head.Initial
              { Head.contestationPeriod = toChain $ contestationPeriod healthyHeadParameters
              , Head.parties = map partyToChain (parties healthyHeadParameters)
              , Head.headId = toPlutusCurrencySymbol $ headPolicyId mutatedSeed
              , Head.seed = toPlutusTxOutRef mutatedSeed
              }
    , SomeMutation (Just $ toErrorCode BurntTokenNumberMismatch) UseInputFromOtherHead <$> do
        (txIn, txOut) <- elements healthyInitials
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
        pure $
          Changes
            [ -- XXX: This is changing the PT of the initial, but not the
              -- datum; it's an impossible situation as the minting policy would
              -- not allow non-matching datum & PT
              ChangeInput txIn (replacePolicyIdWith testPolicyId otherHeadId txOut) (Just $ toScriptData Initial.ViaAbort)
            , ChangeMintedValue (removePTFromMintedValue txOut tx)
            ]
    , SomeMutation (Just $ toErrorCode ReimbursedOutputsDontMatch) ReorderCommitOutputs <$> do
        let outputs = txOuts' tx
        outputs' <- shuffle outputs `suchThat` (/= outputs)
        let reorderedOutputs = uncurry ChangeOutput <$> zip [0 ..] outputs'
        pure $ Changes reorderedOutputs
    , SomeMutation (Just $ toErrorCode MintingNotAllowed) MintOnAbort <$> do
        mintAPT <- addPTWithQuantity tx 1
        -- We need to also remove one party to make sure the vHead validator
        -- still thinks it's the right number of tokens getting burned.
        let onePartyLess = List.tail healthyParties
        let removeOneParty =
              ChangeInputHeadDatum $
                Head.Initial
                  { Head.contestationPeriod = toChain $ contestationPeriod healthyHeadParameters
                  , Head.parties = map partyToChain onePartyLess
                  , Head.headId = toPlutusCurrencySymbol $ headPolicyId testSeedInput
                  , Head.seed = toPlutusTxOutRef testSeedInput
                  }
        pure $ Changes [mintAPT, removeOneParty]
    , SomeMutation Nothing ExtractValue <$> do
        divertFunds <- do
          let allValue = foldMap txOutValue $ txOuts' tx
          extractionTxOut <- do
            someAddress <- genAddressInEra testNetworkId
            pure $ TxOut someAddress allValue TxOutDatumNone ReferenceScriptNone
          pure
            [ RemoveOutput 0
            , RemoveOutput 1
            , AppendOutput extractionTxOut
            ]

        pure $
          Changes $
            [ ChangeMintedValue mempty
            , RemoveInput healthyHeadInput
            ]
              ++ divertFunds
    , SomeMutation (Just $ toErrorCode STNotBurnedError) DoNotBurnST
        <$> changeMintedTokens tx (valueFromList [(AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName, 1)])
    , SomeMutation (Just $ toErrorCode STNotBurned) DoNotBurnSTInitial
        <$> changeMintedTokens tx (valueFromList [(AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName, 1)])
    ]
