module Hydra.Model.MockChainSpec where

import Hydra.Model.MockChain (scriptLedger)
import "QuickCheck" Test.QuickCheck (Property, Testable (property), counterexample, forAllBlind, (===))
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-cardano-api" Hydra.Cardano.Api (Tx, TxIn (TxIn), UTxO, prettyPrintJSON)
import "hydra-cardano-api" Hydra.Cardano.Api.Pretty (renderTx, renderUTxO)
import "hydra-node" Hydra.Ledger (Ledger (applyTransactions))
import "hydra-node" Test.Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import "hydra-tx" Hydra.Tx.IsTx (IsTx (txId))
import "text" Data.Text (unpack)

spec :: Spec
spec =
  prop "works with valid transaction" appliesValidTransaction

appliesValidTransaction :: Property
appliesValidTransaction =
  forAllBlind genSequenceOfSimplePaymentTransactions $ \(utxo, txs) ->
    let result = applyTransactions scriptLedger (ChainSlot 0) utxo txs
     in case result of
          Right u ->
            isOutputOfLastTransaction txs u
          Left (tx, err) ->
            property False
              & counterexample ("Error: " <> show err)
              & counterexample ("Failing tx: " <> renderTx tx)
              & counterexample ("All txs: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON txs))
              & counterexample ("Initial UTxO: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON utxo))

isOutputOfLastTransaction :: [Tx] -> UTxO -> Property
isOutputOfLastTransaction txs utxo =
  case (listToMaybe $ reverse txs, UTxO.toList utxo) of
    (Just tx, [(TxIn txid _, _)]) ->
      txId tx === txid
    (Just _, _) ->
      property False
        & counterexample ("Resulting Utxo: " <> unpack (foldMap renderUTxO (UTxO.toList utxo)))
        & counterexample ("Txs: " <> show (txId <$> txs))
    (Nothing, _) ->
      property True
