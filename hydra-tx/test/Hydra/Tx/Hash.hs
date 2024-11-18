
module Hydra.Tx.Hash where

import Hydra.Prelude

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===))
import Hydra.Contract.Util (hashTxOuts)

spec :: Spec
spec = do
  describe "hashTxOuts/hashUTxO" $
    it "hashing empty [TxOut] is the same as empty UTxO hash" $ do
      let hashUTxO = hashUTxO mempty
      let hashOuts = hashTxOuts []
       in hashUTxO === hashOuts
