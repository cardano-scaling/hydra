module Hydra.Cardano.Api.TxOut where

import Cardano.Api

txOuts' :: Tx AlonzoEra -> [TxOut CtxTx AlonzoEra]
txOuts' tx =
  let TxBody TxBodyContent{txOuts} = getTxBody tx
   in txOuts

{-# DEPRECATED getOutputs "use txOuts' instead." #-}
getOutputs :: Tx AlonzoEra -> [TxOut CtxTx AlonzoEra]
getOutputs = txOuts'
