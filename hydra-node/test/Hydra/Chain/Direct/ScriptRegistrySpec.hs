module Hydra.Chain.Direct.ScriptRegistrySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.Direct.ScriptRegistry (
  genScriptRegistry,
  newScriptRegistry,
  registryUtxo,
 )
import Test.QuickCheck (forAll, (===))

spec :: Spec
spec =
  prop "newScriptRegistry (registryUtxo r) === Just r" $
    forAll genScriptRegistry $ \r ->
      newScriptRegistry (registryUtxo r) === Just r
