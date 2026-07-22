module Hydra.Tx.SecretSpec where

import Hydra.Prelude

import Hydra.Tx.Secret (mkSecret, withSecret)
import Test.Hydra.Prelude
import Test.QuickCheck (Fun, applyFun, (===))

spec :: Spec
spec = describe "Secret" $ do
  prop "withSecret . mkSecret = ($)" $ \(x :: Int) (f :: Fun Int Int) ->
    withSecret (mkSecret x) (applyFun f) === applyFun f x

  prop "mkSecret preserves the value via id" $ \(x :: Int) ->
    withSecret (mkSecret x) id === x
