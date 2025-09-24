module Hydra.Model.MockChainSpec where

import Cardano.Api.UTxO qualified as UTxO
import Data.Text (unpack)
import Hydra.Cardano.Api (Tx, TxIn (TxIn), UTxO, prettyPrintJSON, renderUTxO)
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.Ledger (Ledger (applyTransactions))
import Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import Hydra.Model.MockChain (scriptLedger)
import Hydra.Prelude
import Hydra.Tx.IsTx (IsTx (txId))
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable (property), counterexample, forAllBlind, (===))

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
        & counterexample ("Resulting Utxo: " <> renderUTxO utxo)
        & counterexample ("Txs: " <> show (txId <$> txs))
    (Nothing, _) ->
      property True
