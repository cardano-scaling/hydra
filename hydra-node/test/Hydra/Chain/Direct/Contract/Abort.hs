{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import Hydra.Chain (HeadParameters (..))
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (abortTx, mkHeadOutputInitial)
import Hydra.Ledger.Cardano (CardanoTx, Utxo, getDatum, singletonUtxo, toUtxoContext)
import Hydra.Prelude

--
-- AbortTx
--

healthyAbortTx :: (CardanoTx, Utxo)
healthyAbortTx =
  (tx, lookupUtxo)
 where
  lookupUtxo =
    singletonUtxo (headInput, toUtxoContext headOutput)

  Right tx =
    abortTx
      Fixture.testNetworkId
      (headInput, headDatum)
      initials

  headInput = generateWith arbitrary 42

  headOutput = mkHeadOutputInitial Fixture.testNetworkId headParameters

  headParameters =
    HeadParameters
      { contestationPeriod = 10
      , parties = mempty
      }

  Just headDatum = getDatum headOutput

  initials = mempty
