{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..))
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (abortTx, mkHeadOutputInitial, mkInitialOutput)
import Hydra.Chain.Direct.TxSpec (drop2nd, drop3rd, genAbortableOutputs)
import Hydra.Ledger.Cardano (CardanoTx, Utxo, Utxo' (Utxo), getDatum, singletonUtxo, toUtxoContext)
import Hydra.Prelude
import Test.QuickCheck.Gen (vectorOf)

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
      , parties
      }

  headDatum = unsafeGetDatum headOutput

  -- XXX: We loose type information by dealing with 'TxOut CtxTx' where datums
  -- are optional
  unsafeGetDatum = fromJust . getDatum

  parties =
    [ generateWith arbitrary i | i <- [1 .. 3]
    ]

  (initials, commits) =
    generateWith (genAbortableOutputs parties) 42

propHasInitial :: (CardanoTx, Utxo) -> Property
propHasInitial _ = property False

propHasCommit :: (CardanoTx, Utxo) -> Property
propHasCommit _ = property False

data AbortMutation = AbortMutation
  deriving (Generic, Show, Enum, Bounded)

genAbortMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genAbortMutation = undefined
