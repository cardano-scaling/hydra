-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (
  AlonzoEra,
  Hash,
  ScriptData,
  TxBody (TxBody),
  TxBodyContent (txOuts),
  TxOut (TxOut),
  TxOutDatumHash (TxOutDatumHash),
  makeTransactionBody,
 )
import Gen.Cardano.Api.Typed (genTxIn)
import Hydra.Chain (HeadParameters (HeadParameters, parties))
import Hydra.Chain.Direct.Tx (initTx)
import Test.QuickCheck (counterexample, forAll, property)
import Test.QuickCheck.Hedgehog (hedgehog)

spec :: Spec
spec =
  parallel $
    describe "initTx" $ do
      prop "can be used to construct a non-balanced TxBody" $ \params ->
        forAll (hedgehog genTxIn) $ \txIn ->
          case makeTransactionBody $ initTx params txIn of
            Left err -> counterexample ("TxBodyError: " <> show err) False
            Right _ -> property True

      prop "contains HeadParameters as datum" $ \params ->
        forAll (hedgehog genTxIn) $ \txIn ->
          case makeTransactionBody $ initTx params txIn of
            Left err -> counterexample ("TxBodyError: " <> show err) False
            Right (TxBody content) ->
              let outs = txOuts content
                  datumHashes = mapMaybe datumHash outs
               in counterexample ("txOuts: " <> show outs) $
                    counterexample ("datumHashes: " <> show datumHashes) $
                      -- TODO(SN): we could check that the hash corresponds to
                      -- what we expect here, but the Tx won't contain the
                      -- `Datum` itself.. so no point in continuing here
                      length datumHashes == 1

datumHash :: TxOut AlonzoEra -> Maybe (Hash ScriptData)
datumHash = \case
  (TxOut _ _ (TxOutDatumHash _ h)) -> Just h
  _ -> Nothing
