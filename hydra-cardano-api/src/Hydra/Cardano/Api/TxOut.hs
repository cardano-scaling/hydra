module Hydra.Cardano.Api.TxOut where

import Hydra.Cardano.Api.Prelude

txOuts' :: Tx Era -> [TxOut CtxTx Era]
txOuts' (getTxBody -> txBody) =
  let TxBody TxBodyContent{txOuts} = txBody
   in txOuts

{-# DEPRECATED getOutputs "use txOuts' instead." #-}
getOutputs :: Tx Era -> [TxOut CtxTx Era]
getOutputs = txOuts'
