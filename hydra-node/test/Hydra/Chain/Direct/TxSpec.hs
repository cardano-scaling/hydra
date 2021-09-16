-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (counterexample, property)

spec :: Spec
spec =
  parallel $ do
    prop "can construct initTx" $ \params txIn ->
      case initTx params txIn of
        Left err -> counterexample ("TxBodyError: " <> show err) False
        Right _ -> property True
