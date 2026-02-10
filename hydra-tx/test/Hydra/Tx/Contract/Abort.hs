{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Tx.Contract.Abort where

import Hydra.Cardano.Api
import Hydra.Plutus.Gen ()
import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Contract.CommitError (CommitError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Contract.HeadTokensError (HeadTokensError (..))
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.InitialError (InitialError (STNotBurned))
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Hydra.Tx (
  HeadParameters (..),
  Party,
  partyToChain,
  registryUTxO,
 )
import Hydra.Tx.Abort (abortTx)
import Hydra.Tx.Commit (mkCommitDatum)
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.Init (mkHeadOutputInitial)
import Hydra.Tx.Utils (adaOnly, hydraHeadV1AssetName, onChainIdToAssetName, verificationKeyToOnChainId)
import Test.Hydra.Tx.Fixture (
  cperiod,
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Test.Hydra.Tx.Gen (
  genAddressInEra,
  genForParty,
  genOneUTxOFor,
  genScriptRegistry,
  genVerificationKey,
 )
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeMintedTokens,
  changeMintedValueQuantityFrom,
  isHeadOutput,
  removePTFromMintedValue,
  replacePolicyIdWith,
 )
import Test.QuickCheck (Property, choose, counterexample, elements, oneof, shuffle, suchThat, vectorOf)
import "base" Data.List qualified as List
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "containers" Data.Map qualified as Map

--
-- AbortTx
--

healthyAbortTx :: HasCallStack => (Tx, UTxO)
healthyAbortTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton healthyHeadInput (toCtxUTxOTxOut headOutput)
      <> UTxO (Map.fromList healthyInitials)
      <> UTxO (Map.fromList (map (\(i, o, _) -> (i, o)) healthyCommits))
      <> registryUTxO scriptRegistry

  tx =
    either (error . show) id $
      abortTx
        committedUTxO
        scriptRegistry
        somePartyCardanoVerificationKey
        (healthyHeadInput, toCtxUTxOTxOut headOutput)
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
  -- to test healthy abort txs with varied combinations of initial and commit
  -- outputs
  generateWith (genAbortableOutputs healthyParties `suchThat` thereIsTwoEach) 42
 where
  thereIsTwoEach :: ([a], [b]) -> Bool
  thereIsTwoEach (is, cs) = length is >= 2 && length cs >= 2

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 4]
  ]

propHasInitial :: (Tx, UTxO) -> Property
propHasInitial (_, utxo) =
  any paysToInitialScript (UTxO.txOutputs utxo)
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Initial Script: " <> show addr)
 where
  addr = mkScriptAddress testNetworkId initialValidatorScript
  paysToInitialScript txOut =
    txOutAddress txOut == addr

propHasCommit :: (Tx, UTxO) -> Property
propHasCommit (_, utxo) =
  any paysToCommitScript (UTxO.txOutputs utxo)
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Commit Script: " <> show addr)
 where
  addr = mkScriptAddress testNetworkId commitValidatorScript
  paysToCommitScript txOut =
    txOutAddress txOut == addr

data AbortMutation
  = -- | Add one more party to the hydra keys. This is essentially the same as
    -- not collecting all inputs.
    MutateParties
  | -- | Not collect one committed UTxO by removing the input and not burn the
    -- corresponding PT.
    DropCollectedInput
  | -- | Not reimburse one of the parties.
    DropOneCommitOutput
  | -- | Burning one PT more. This should be an impossible situation, but it is
    -- tested nonetheless.
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
    [ SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) MutateParties . ChangeInputHeadDatum <$> do
        moreParties <- (: healthyParties) <$> arbitrary
        c <- arbitrary
        pure $
          Head.Initial
            c
            (partyToChain <$> moreParties)
            (toPlutusCurrencySymbol $ headPolicyId testSeedInput)
            (toPlutusTxOutRef testSeedInput)
    , SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) DropCollectedInput <$> do
        let abortableInputs = UTxO.toList $ UTxO.filter (not . isHeadOutput) (resolveInputsUTxO utxo tx)
        (toDropTxIn, toDropTxOut) <- elements abortableInputs
        pure $
          Changes
            [ RemoveInput toDropTxIn
            , ChangeMintedValue $ removePTFromMintedValue toDropTxOut tx
            ]
    , SomeMutation (pure $ toErrorCode ReimbursedOutputsDontMatch) DropOneCommitOutput . RemoveOutput <$> choose (0, fromIntegral (length (txOuts' tx) - 1))
    , SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx (-1)
    , SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) BurnOneTokenMore <$> addPTWithQuantity tx (-1)
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) MutateUseDifferentHeadToAbort <$> do
        mutatedSeed <- arbitrary `suchThat` (/= testSeedInput)
        pure $
          ChangeInputHeadDatum
            Head.Initial
              { Head.contestationPeriod = toChain $ contestationPeriod healthyHeadParameters
              , Head.parties = map partyToChain (parties healthyHeadParameters)
              , Head.headId = toPlutusCurrencySymbol $ headPolicyId mutatedSeed
              , Head.seed = toPlutusTxOutRef mutatedSeed
              }
    , SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) UseInputFromOtherHead <$> do
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
    , SomeMutation (pure $ toErrorCode ReimbursedOutputsDontMatch) ReorderCommitOutputs <$> do
        let outputs = txOuts' tx
        outputs' <- shuffle outputs `suchThat` (/= outputs)
        let reorderedOutputs = uncurry ChangeOutput <$> zip [0 ..] outputs'
        pure $ Changes reorderedOutputs
    , SomeMutation (pure $ toErrorCode MintingNotAllowed) MintOnAbort <$> do
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
    , SomeMutation [] ExtractValue <$> do
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
    , SomeMutation (pure $ toErrorCode STNotBurnedError) DoNotBurnST
        <$> changeMintedTokens tx (fromList [(AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName, 1)])
    , SomeMutation (pure $ toErrorCode STNotBurned) DoNotBurnSTInitial
        <$> changeMintedTokens tx (fromList [(AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName, 1)])
    ]

-- * Generators

-- NOTE: Uses 'testPolicyId' for the datum.
genAbortableOutputs :: [Party] -> Gen ([(TxIn, TxOut CtxUTxO)], [(TxIn, TxOut CtxUTxO, UTxO)])
genAbortableOutputs parties =
  go
 where
  go = do
    (initParties, commitParties) <- (`splitAt` parties) <$> choose (0, length parties)
    initials <- mapM genInitial initParties
    commits <- fmap (\(a, (b, c)) -> (a, b, c)) . Map.toList <$> generateCommitUTxOs commitParties
    pure (initials, commits)

  genInitial p =
    mkInitial (genVerificationKey `genForParty` p) <$> arbitrary

  mkInitial ::
    VerificationKey PaymentKey ->
    TxIn ->
    (TxIn, TxOut CtxUTxO)
  mkInitial vk txin =
    ( txin
    , initialTxOut vk
    )

  initialTxOut :: VerificationKey PaymentKey -> TxOut CtxUTxO
  initialTxOut vk =
    TxOut
      (mkScriptAddress testNetworkId initialValidatorScript)
      (fromList [(AssetId testPolicyId (assetNameFromVerificationKey vk), 1)])
      (mkTxOutDatumInline initialDatum)
      ReferenceScriptNone

  initialDatum = Initial.datum (toPlutusCurrencySymbol testPolicyId)

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- NOTE: Uses 'testPolicyId' for the datum.
-- NOTE: We don't generate empty commits and it is used only at one place so perhaps move it?
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUTxOs :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO, UTxO))
generateCommitUTxOs parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  let vks = (\p -> (genVerificationKey `genForParty` p, p)) <$> parties
  committedUTxO <-
    vectorOf (length parties) $
      UTxO.map adaOnly <$> (genOneUTxOFor =<< arbitrary)
  let commitUTxO =
        zip txins $
          uncurry mkCommitUTxO <$> zip vks committedUTxO
  pure $ Map.fromList commitUTxO
 where
  mkCommitUTxO :: (VerificationKey PaymentKey, Party) -> UTxO -> (TxOut CtxUTxO, UTxO)
  mkCommitUTxO (vk, party) utxo =
    ( toCtxUTxOTxOut $
        TxOut
          (mkScriptAddress testNetworkId commitValidatorScript)
          commitValue
          (mkTxOutDatumInline commitDatum)
          ReferenceScriptNone
    , utxo
    )
   where
    commitValue =
      mconcat
        [ lovelaceToValue (Coin 2000000)
        , UTxO.totalValue utxo
        , fromList
            [ (AssetId testPolicyId (assetNameFromVerificationKey vk), 1)
            ]
        ]

    commitDatum = mkCommitDatum party utxo (toPlutusCurrencySymbol testPolicyId)

assetNameFromVerificationKey :: VerificationKey PaymentKey -> AssetName
assetNameFromVerificationKey =
  onChainIdToAssetName . verificationKeyToOnChainId
