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
import Hydra.Chain.Direct.Contract.Mutation (Mutation (ChangeHeadDatum, ChangeInput, RemoveOutput), SomeMutation (..), anyPayToPubKeyTxOut, headTxIn)
import Hydra.Chain.Direct.Fixture (testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (UTxOWithScript, abortTx, mkHeadOutputInitial)
import Hydra.Chain.Direct.TxSpec (drop2nd, drop3rd, genAbortableOutputs)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Party (Party, vkey)
import Hydra.Prelude
import Test.QuickCheck (Property, choose, counterexample, oneof)

--
-- AbortTx
--

healthyAbortTx :: (Tx, UTxO)
healthyAbortTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, toUTxOContext headOutput)
      <> UTxO (Map.fromList (drop3rd <$> healthyInitials))
      <> UTxO (Map.fromList (drop3rd <$> healthyCommits))

  Right tx =
    abortTx
      Fixture.testNetworkId
      (headInput, headDatum)
      (Map.fromList (drop2nd <$> healthyInitials))
      (Map.fromList (tripleToPair <$> healthyCommits))

  headInput = generateWith arbitrary 42

  headOutput = mkHeadOutputInitial Fixture.testNetworkId headParameters

  headParameters =
    HeadParameters
      { contestationPeriod = 10
      , parties = healthyParties
      }

  headDatum = unsafeGetDatum headOutput

  -- XXX: We loose type information by dealing with 'TxOut CtxTx' where datums
  -- are optional
  unsafeGetDatum = fromJust . getScriptData

  tripleToPair (a, b, c) = (a, (b, c))

healthyInitials :: [UTxOWithScript]
healthyCommits :: [UTxOWithScript]
(healthyInitials, healthyCommits) =
  -- NOTE: Why 43 one may ask? Because 42 does not generate commit UTXOs
  -- TODO: Refactor this to be an AbortTx generator because we actually want
  -- to test healthy abort txs with varied combinations of inital and commit
  -- outputs
  generateWith (genAbortableOutputs healthyParties) 43

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
  | MutateDropCommitOutput
  | MutateHeadScriptInput
  deriving (Generic, Show, Enum, Bounded)

genAbortMutation :: (Tx, UTxO) -> Gen SomeMutation
genAbortMutation (_, utxo) =
  oneof
    [ SomeMutation MutateParties . ChangeHeadDatum <$> do
        moreParties <- (: healthyParties) <$> arbitrary
        c <- arbitrary
        pure $ Head.Initial c (partyFromVerKey . vkey <$> moreParties)
    , SomeMutation MutateDropCommitOutput
        . RemoveOutput
        . (+ 1) -- NOTE(AB): Assumes the transaction's first output is the head output
        <$> choose (0, fromIntegral (length healthyCommits - 1))
    , SomeMutation MutateHeadScriptInput . ChangeInput (headTxIn utxo) <$> anyPayToPubKeyTxOut
    ]
