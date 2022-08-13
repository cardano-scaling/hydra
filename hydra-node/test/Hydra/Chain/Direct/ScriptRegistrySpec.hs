module Hydra.Chain.Direct.ScriptRegistrySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.Direct.ScriptRegistry (
  genScriptRegistry,
  newScriptRegistry,
  registryUTxO,
 )
import Test.QuickCheck (forAll, (===))

spec :: Spec
spec =
  prop "newScriptRegistry (registryUTxO r) === Just r" $
    forAll genScriptRegistry $ \r ->
      newScriptRegistry (registryUTxO r) === Right r
