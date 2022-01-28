{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Mutation-based script validator tests for the abort transaction where a
-- 'healthyAbortTx' gets mutated by an arbitrary 'AbortMutation'.
module Hydra.Chain.Direct.Contract.Abort where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (abortTx, mkHeadOutputInitial, mkInitialOutput)
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
      <> initialsUtxo

  Right tx =
    abortTx
      Fixture.testNetworkId
      (headInput, headDatum)
      initials
      mempty

  headInput = generateWith arbitrary 42

  headOutput = mkHeadOutputInitial Fixture.testNetworkId headParameters

  headParameters =
    HeadParameters
      { contestationPeriod = 10
      , parties = mempty
      }

  headDatum = unsafeGetDatum headOutput

  -- XXX: We loose type information by dealing with 'TxOut CtxTx' where datums
  -- are optional
  unsafeGetDatum = fromJust . getDatum

  participantsCardanoKeys = generateWith arbitrary 42

  initialTxOuts = mkInitialOutput Fixture.testNetworkId <$> participantsCardanoKeys

  initialTxIns = generateWith (vectorOf (length initialTxOuts) arbitrary) 42

  initialsUtxo =
    Utxo $ Map.fromList $ zip initialTxIns (toUtxoContext <$> initialTxOuts)

  initials = Map.fromList $ zip initialTxIns (unsafeGetDatum <$> initialTxOuts)
