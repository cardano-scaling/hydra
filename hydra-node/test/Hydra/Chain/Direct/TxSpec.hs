-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import qualified Data.ByteString.Lazy as LBS
import Hydra.Chain.Direct.Tx (initTx, mkUnsignedTx)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (counterexample)

spec :: Spec
spec =
  parallel $
    describe "initTx" $ do
      prop "can construct & serialize unsigned initTx" $ \params txIn ->
        let tx = mkUnsignedTx $ initTx params txIn
            cbor = serialize tx
            len = LBS.length cbor
         in counterexample ("Tx: " <> show tx) $
              counterexample ("Cbor: " <> show cbor) $ len > 0

--       prop "contains HeadParameters as datum" $ \params ->
--         forAll (hedgehog genTxIn) $ \txIn ->
--           case makeTransactionBody $ initTx params txIn of
--             Left err -> counterexample ("TxBodyError: " <> show err) False
--             Right (TxBody content) ->
--               let outs = txOuts content
--                   datumHashes = mapMaybe datumHash outs
--                in counterexample ("txOuts: " <> show outs) $
--                     counterexample ("datumHashes: " <> show datumHashes) $
--                       -- TODO(SN): we could check that the hash corresponds to
--                       -- what we expect here, but the Tx won't contain the
--                       -- `Datum` itself.. so no point in continuing here
--                       length datumHashes == 1

-- datumHash :: TxOut AlonzoEra -> Maybe (Hash ScriptData)
-- datumHash = \case
--   (TxOut _ _ (TxOutDatumHash _ h)) -> Just h
--   _ -> Nothing
