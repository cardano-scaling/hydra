{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Gen (genForParty)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeMintedValueQuantityFrom,
  replacePolicyIdWith,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (
  UTxOWithScript,
  abortTx,
  mkHeadOutputInitial,
 )
import Hydra.Chain.Direct.TxSpec (drop3rd, genAbortableOutputs)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (genVerificationKey)
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
      <> UTxO (Map.fromList (drop3rd <$> healthyInitials))
      <> UTxO (Map.fromList (map (\(i, o, _, _) -> (i, o)) healthyCommits))
      <> registryUTxO scriptRegistry

  tx =
    either (error . show) id $
      abortTx
        committedUTxO
        scriptRegistry
        somePartyCardanoVerificationKey
        (healthyHeadInput, toUTxOContext headOutput, headDatum)
        headTokenScript
        (Map.fromList (tripleToPair <$> healthyInitials))
        (Map.fromList (map (\(i, o, sd, _) -> (i, (o, sd))) healthyCommits))

  committedUTxO = foldMap (\(_, _, _, u) -> u) healthyCommits

  scriptRegistry = genScriptRegistry `generateWith` 42

  somePartyCardanoVerificationKey = flip generateWith 42 $ do
    genForParty genVerificationKey <$> elements healthyParties

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput = mkHeadOutputInitial testNetworkId testSeedInput testPolicyId healthyHeadParameters

  headDatum = unsafeGetDatum headOutput

  -- XXX: We loose type information by dealing with 'TxOut CtxTx' where datums
  -- are optional
  unsafeGetDatum = fromJust . getScriptData

  tripleToPair (a, b, c) = (a, (b, c))

healthyHeadInput :: TxIn
healthyHeadInput = generateWith arbitrary 42

healthyHeadParameters :: HeadParameters
healthyHeadParameters =
  HeadParameters
    { contestationPeriod = cperiod
    , parties = healthyParties
    }

healthyInitials :: [UTxOWithScript]
healthyCommits :: [(TxIn, TxOut CtxUTxO, ScriptData, UTxO)]
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
  = MutateParties
  | DropOneCommitOutput
  | BurnOneTokenMore
  | -- | Meant to test that the minting policy is burning all PTs present in tx
    MutateThreadTokenQuantity
  | DropCollectedInput
  | MutateRequiredSigner
  | -- | Simply change the currency symbol of the ST.
    MutateHeadId
  | -- Spend some abortable output from a different Head
    -- e.g. replace a commit by another commit from a different Head.
    UseInputFromOtherHead
  | ReorderCommitOutputs
  deriving (Generic, Show, Enum, Bounded)

genAbortMutation :: (Tx, UTxO) -> Gen SomeMutation
genAbortMutation (tx, _utxo) =
  oneof
    [ SomeMutation Nothing MutateParties . ChangeInputHeadDatum <$> do
        moreParties <- (: healthyParties) <$> arbitrary
        c <- arbitrary
        pure $
          Head.Initial
            c
            (partyToChain <$> moreParties)
            (toPlutusCurrencySymbol $ headPolicyId testSeedInput)
            (toPlutusTxOutRef testSeedInput)
    , SomeMutation Nothing DropOneCommitOutput
        . RemoveOutput
        <$> choose (0, fromIntegral (length (txOuts' tx) - 1))
    , SomeMutation Nothing MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx (-1)
    , SomeMutation Nothing BurnOneTokenMore <$> addPTWithQuantity tx (-1)
    , SomeMutation Nothing DropCollectedInput . RemoveInput <$> do
        -- TODO: This would actually not be possible alone as the ledger rejects
        -- it. Either fix the framework or simulate the forced change of also
        -- not burn all tokens.
        -- TODO: remove any of the non-head inputs
        elements (txIns' tx)
    , SomeMutation (Just "signer is not a participant") MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation Nothing MutateHeadId <$> do
        mutatedSeed <- arbitrary `suchThat` (/= testSeedInput)
        let mutatedInput =
              mkHeadOutputInitial
                testNetworkId
                mutatedSeed
                (headPolicyId mutatedSeed)
                healthyHeadParameters
        return $ ChangeInput healthyHeadInput (toUTxOContext mutatedInput) (Just $ toScriptData Head.Abort)
    , SomeMutation Nothing UseInputFromOtherHead <$> do
        (input, output, _) <- elements healthyInitials
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
        pure $
          Changes
            [ ChangeInput input (replacePolicyIdWith testPolicyId otherHeadId output) (Just $ toScriptData Initial.ViaAbort)
            , ChangeMintedValue (removePTFromMintedValue output tx)
            ]
    , SomeMutation (Just "reimbursed outputs dont match") ReorderCommitOutputs <$> do
        let outputs = txOuts' tx
        outputs' <- shuffle outputs `suchThat` (/= outputs)
        let reorderedOutputs = uncurry ChangeOutput <$> zip [0 ..] outputs'
        pure $ Changes reorderedOutputs
    ]

removePTFromMintedValue :: TxOut CtxUTxO -> Tx -> Value
removePTFromMintedValue output tx =
  case txMintValue $ txBodyContent $ txBody tx of
    TxMintValueNone -> error "expected minted value"
    TxMintValue v _ -> valueFromList $ filter (not . isPT) $ valueToList v
 where
  outValue = txOutValue output
  assetNames =
    [ (policyId, pkh) | (AssetId policyId pkh, _) <- valueToList outValue, policyId == testPolicyId
    ]
  (headId, assetName) =
    case assetNames of
      [assetId] -> assetId
      _ -> error "expected one assetId"
  isPT = \case
    (AssetId pid asset, _) ->
      pid == headId && asset == assetName
    _ -> False
