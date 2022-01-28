{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Mutation (Mutation (ChangeHeadDatum), SomeMutation (..))
import Hydra.Chain.Direct.Fixture (testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (abortTx, mkHeadOutputInitial)
import Hydra.Chain.Direct.TxSpec (drop2nd, drop3rd, genAbortableOutputs)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Cardano (
  CardanoTx,
  PlutusScriptV1,
  Utxo,
  Utxo' (Utxo),
  fromPlutusScript,
  getDatum,
  mkScriptAddress,
  singletonUtxo,
  toUtxoContext,
  txOutAddress,
 )
import Hydra.Party (Party, vkey)
import Hydra.Prelude
import Test.QuickCheck (Property, counterexample, oneof)

--
-- AbortTx
--

healthyAbortTx :: (CardanoTx, Utxo)
healthyAbortTx =
  (tx, lookupUtxo)
 where
  lookupUtxo =
    singletonUtxo (headInput, toUtxoContext headOutput)
      <> Utxo (Map.fromList (drop3rd <$> initials))
      <> Utxo (Map.fromList (drop3rd <$> commits))

  Right tx =
    abortTx
      Fixture.testNetworkId
      (headInput, headDatum)
      (Map.fromList (drop2nd <$> initials))
      (Map.fromList (drop2nd <$> commits))

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
  unsafeGetDatum = fromJust . getDatum

  (initials, commits) =
    -- NOTE: Why 43 one may ask? Because 42 does not generate commit UTXOs
    -- TODO: Refactor this to be an AbortTx generator because we actually want
    -- to test healthy abort txs with varied combinations of inital and commit
    -- outputs
    generateWith (genAbortableOutputs healthyParties) 43

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]

propHasInitial :: (CardanoTx, Utxo) -> Property
propHasInitial (_, utxo) =
  any paysToInitialScript utxo
    & counterexample ("Utxo: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Initial Script: " <> show address)
 where
  address = mkScriptAddress @PlutusScriptV1 testNetworkId (fromPlutusScript Initial.validatorScript)
  paysToInitialScript txOut =
    txOutAddress txOut == address

propHasCommit :: (CardanoTx, Utxo) -> Property
propHasCommit (_, utxo) =
  any paysToCommitScript utxo
    & counterexample ("Utxo: " <> decodeUtf8 (encodePretty utxo))
    & counterexample ("Looking for Commit Script: " <> show address)
 where
  address = mkScriptAddress @PlutusScriptV1 testNetworkId (fromPlutusScript Commit.validatorScript)
  paysToCommitScript txOut =
    txOutAddress txOut == address

data AbortMutation
  = MutateParties
  deriving (Generic, Show, Enum, Bounded)

genAbortMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genAbortMutation _ =
  oneof
    [ SomeMutation MutateParties . ChangeHeadDatum <$> do
        moreParties <- (: healthyParties) <$> arbitrary
        c <- arbitrary
        pure $ Head.Initial c (partyFromVerKey . vkey <$> moreParties)
    ]
