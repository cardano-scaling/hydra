module Hydra.Tx.Hash where

import "hydra-prelude" Hydra.Prelude
import "QuickCheck" Test.QuickCheck ((===))
import "hspec" Test.Hspec (Spec, describe, it)
import "hydra-plutus" Hydra.Contract.Util (hashTxOuts)

spec :: Spec
spec = do
  describe "hashTxOuts/hashUTxO" $
    it "hashing empty [TxOut] is the same as empty UTxO hash" $ do
      let hashUTxO = hashUTxO mempty
      let hashOuts = hashTxOuts []
       in hashUTxO === hashOuts
