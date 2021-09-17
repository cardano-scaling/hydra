-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (
  CardanoEra (AlonzoEra),
  Lovelace,
  TxBody (TxBody),
  TxBodyContent (txOuts),
  TxOut (TxOut),
  TxOutValue (TxOutAdaOnly, TxOutValue),
  valueToLovelace,
 )
import Gen.Cardano.Api.Typed (genTxIn, genTxOut)
import Hydra.Chain.Direct.Tx (initTx)
import Test.QuickCheck (counterexample, forAll, property, scale, (==>))
import Test.QuickCheck.Hedgehog (hedgehog)

spec :: Spec
spec =
  parallel $
    describe "initTx" $ do
      prop "can be constructed" $ \params ->
        forAll (hedgehog genTxIn) $ \txIn ->
          forAll (hedgehog $ genTxOut AlonzoEra) $ \txOut ->
            case initTx params (txIn, txOut) of
              Left err -> counterexample ("TxBodyError: " <> show err) False
              Right _ -> property True

      prop "pays back the change" $ \params ->
        forAll (hedgehog genTxIn) $ \txIn ->
          -- TODO(SN): the genTxOut / genTxOutValue by cardano-api is only
          -- generating low quantity of assets and can't seem to get it scaled
          -- appropriately (in constrast non-alonzo lovelace generation is
          -- between 0-5000)
          forAll (hedgehog $ genTxOut AlonzoEra) $ \txOut ->
            txOut `holdsMoreThan` 10
              ==> case initTx params (txIn, txOut) of
                Left err -> counterexample ("TxBodyError: " <> show err) False
                Right (TxBody content) ->
                  let outs = txOuts content
                   in -- TODO(SN): expect that there is all but some fee in one of the outputs?
                      counterexample ("txOuts: " <> show outs) $
                        not (null outs)

holdsMoreThan :: TxOut era -> Lovelace -> Bool
holdsMoreThan (TxOut _ outValue _) required = case outValue of
  TxOutAdaOnly _ l -> l >= required
  TxOutValue _ v -> case valueToLovelace v of
    Just l -> l >= required
    Nothing -> False
