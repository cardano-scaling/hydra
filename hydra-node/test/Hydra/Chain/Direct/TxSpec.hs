-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (CardanoEra (AlonzoEra), TxBody (TxBody), TxBodyContent (txOuts))
import Gen.Cardano.Api.Typed (genTxIn, genTxOut)
import Hydra.Chain.Direct.Tx (initTx)
import Test.QuickCheck (counterexample, forAll, property)
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
          forAll (hedgehog $ genTxOut AlonzoEra) $ \txOut ->
            -- TODO(SN): only generate / assert that there is enough lovelace to pay fees in txOut?
            case initTx params (txIn, txOut) of
              Left err -> counterexample ("TxBodyError: " <> show err) False
              Right (TxBody content) ->
                let outs = txOuts content
                 in -- TODO(SN): expect that there is all but some fee in one of the outputs?
                    counterexample ("txOuts: " <> show outs) $
                      not (null outs)
