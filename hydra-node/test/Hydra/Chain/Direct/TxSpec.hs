-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Cardano.Binary (serialize)
import Data.ByteString.Lazy as LBS
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (counterexample)
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()

spec :: Spec
spec =
  parallel $ do
    prop "can construct & serialize initTx" $ \params txIn ->
      let cbor = serialize (initTx params txIn)
          len = LBS.length cbor
       in counterexample (show cbor) $ len > 0
