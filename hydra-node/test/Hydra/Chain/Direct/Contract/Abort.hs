{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import Hydra.Cardano.Api

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  anyPayToPubKeyTxOut,
  changeMintedValueQuantityFrom,
  headTxIn,
 )
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.Tx (
  UTxOWithScript,
  abortTx,
  headPolicyId,
  headValue,
  mkHeadOutputInitial,
  mkHeadTokenScript,
 )
import Hydra.Chain.Direct.TxSpec (drop3rd, genAbortableOutputs)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.Party (Party, partyToChain)
import Hydra.Prelude
import Test.QuickCheck (Property, choose, counterexample, elements, oneof, suchThat)

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
      <> UTxO (Map.fromList (drop3rd <$> healthyCommits))

  tx =
    either (error . show) id $
      abortTx
        somePartyCardanoVerificationKey
        (healthyHeadInput, toUTxOContext headOutput, headDatum)
        headTokenScript
        (Map.fromList (tripleToPair <$> healthyInitials))
        (Map.fromList (tripleToPair <$> healthyCommits))

  somePartyCardanoVerificationKey = flip generateWith 42 $ do
    genForParty genVerificationKey <$> elements healthyParties

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput = mkHeadOutputInitial testNetworkId testPolicyId healthyHeadParameters

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
    { contestationPeriod = 10
    , parties = healthyParties
    }

healthyInitials :: [UTxOWithScript]
healthyCommits :: [UTxOWithScript]
(healthyInitials, healthyCommits) =
  -- TODO: Refactor this to be an AbortTx generator because we actually want
  -- to test healthy abort txs with varied combinations of inital and commit
  -- outputs
  generateWith (genAbortableOutputs healthyParties `suchThat` thereIsAtLeastOneCommit) 42

thereIsAtLeastOneCommit :: ([UTxOWithScript], [UTxOWithScript]) -> Bool
thereIsAtLeastOneCommit (is, cs) = not (null cs) && not (null is)

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]

propHasInitial :: (Tx, UTxO) -> Property
propHasInitial (_, utxo) =
  any paysToInitialScript utxo
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Initial Script: " <> show addr)
 where
  addr = mkScriptAddress @PlutusScriptV1 testNetworkId (fromPlutusScript Initial.validatorScript)
  paysToInitialScript txOut =
    txOutAddress txOut == addr

propHasCommit :: (Tx, UTxO) -> Property
propHasCommit (_, utxo) =
  any paysToCommitScript utxo
    & counterexample ("UTxO: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Commit Script: " <> show addr)
 where
  addr = mkScriptAddress @PlutusScriptV1 testNetworkId (fromPlutusScript Commit.validatorScript)
  paysToCommitScript txOut =
    txOutAddress txOut == addr

data AbortMutation
  = MutateParties
  | DropOneCommitOutput
  | MutateHeadScriptInput
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
  deriving (Generic, Show, Enum, Bounded)

genAbortMutation :: (Tx, UTxO) -> Gen SomeMutation
genAbortMutation (tx, utxo) =
  oneof
    [ SomeMutation MutateParties . ChangeHeadDatum <$> do
        moreParties <- (: healthyParties) <$> arbitrary
        c <- arbitrary
        pure $ Head.Initial c (partyToChain <$> moreParties)
    , SomeMutation DropOneCommitOutput
        . RemoveOutput
        <$> choose (0, fromIntegral (length (txOuts' tx) - 1))
    , SomeMutation MutateHeadScriptInput <$> (ChangeInput (headTxIn utxo) <$> anyPayToPubKeyTxOut <*> pure Nothing)
    , SomeMutation MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx (-1)
    , SomeMutation BurnOneTokenMore <$> addPTWithQuantity tx (-1)
    , SomeMutation DropCollectedInput . RemoveInput <$> elements (txIns' tx)
    , SomeMutation MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation MutateHeadId <$> do
        illedHeadResolvedInput <-
          mkHeadOutputInitial testNetworkId
            <$> fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
            <*> pure healthyHeadParameters
        return $ ChangeInput healthyHeadInput (toUTxOContext illedHeadResolvedInput) (Just $ toScriptData Head.Abort)
    , SomeMutation UseInputFromOtherHead <$> do
        (input, output, _) <- elements healthyInitials
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))

        let TxOut addr value datum = output
            assetNames =
              [ (policyId, pkh) | (AssetId policyId pkh, _) <- valueToList value, policyId == testPolicyId
              ]
            (originalPolicyId, assetName) =
              case assetNames of
                [assetId] -> assetId
                _ -> error "expected one assetId"

            newValue = headValue <> valueFromList [(AssetId otherHeadId assetName, 1)]

            ptForAssetName = \case
              (AssetId pid asset, _) ->
                pid == originalPolicyId && asset == assetName
              _ -> False

            mintedValue' = case txMintValue $ txBodyContent $ txBody tx of
              TxMintValueNone -> error "expected minted value"
              TxMintValue v _ -> valueFromList $ filter (not . ptForAssetName) $ valueToList v

            output' = TxOut addr newValue datum

        pure $
          Changes
            [ ChangeInput input output' (Just $ toScriptData Initial.Abort)
            , ChangeMintedValue mintedValue'
            ]
    ]
