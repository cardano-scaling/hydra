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
    prop "can construct initTx" $ \params ->
      case initTx params of
        Left err -> counterexample (show err) False
        Right _ -> property True
