{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.FanOut where

import Hydra.Prelude hiding (label)

import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
 )
import Hydra.Chain.Direct.Tx (
  fanoutTx,
 )
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Ledger.Cardano (
  CardanoTx,
  Utxo,
  adaOnly,
  fromLedgerTx,
  fromLedgerUtxo,
  genOutput,
  genUtxoWithSimplifiedAddresses,
  genValue,
  getOutputs,
  hashTxOuts,
  modifyTxOutValue,
  txOutValue,
 )
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (toBuiltin, toData)
import Test.QuickCheck (
  elements,
  oneof,
  suchThat,
 )
import Test.QuickCheck.Instances ()

healthyFanoutTx :: (CardanoTx, Utxo)
healthyFanoutTx =
  ( fromLedgerTx tx
  , fromLedgerUtxo lookupUtxo
  )
 where
  tx = fanoutTx healthyFanoutUtxo (headInput, headDatum)
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyFanoutDatum
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

healthyFanoutUtxo :: Utxo
healthyFanoutUtxo =
  -- NOTE: we trim down the generated tx's output to make sure it fits w/in
  -- TX size limits
  adaOnly <$> generateWith genUtxoWithSimplifiedAddresses 42

healthyFanoutDatum :: Head.State
healthyFanoutDatum =
  Head.Closed 1 (toBuiltin $ hashTxOuts $ toList healthyFanoutUtxo)

data FanoutMutation
  = MutateAddUnexpectedOutput
  | MutateChangeOutputValue
  deriving (Generic, Show, Enum, Bounded)

genFanoutMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genFanoutMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutput
    , SomeMutation MutateChangeOutputValue <$> do
        let outs = getOutputs tx
        (ix, out) <- elements (zip [0 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    ]
