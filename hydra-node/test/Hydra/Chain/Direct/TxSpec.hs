-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx, wits))
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (TxWitness, txdats), nullDats, unTxDats)
import qualified Data.ByteString.Lazy as LBS
import Hydra.Chain.Direct.Tx (initTx)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (Testable (property), counterexample)

spec :: Spec
spec =
  parallel $
    describe "initTx" $ do
      prop "can construct & serialize unsigned initTx" $ \txIn params ->
        let tx = initTx params txIn
            cbor = serialize tx
            len = LBS.length cbor
         in counterexample ("Tx: " <> show tx) $
              counterexample ("Cbor: " <> show cbor) $ len > 0

      prop "contains some datums" $ \txIn params ->
        let ValidatedTx{wits} = initTx params txIn
            dats = txdats wits
         in counterexample ("TxDats: " <> show dats) $
              not $ nullDats dats
