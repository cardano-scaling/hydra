module Hydra.Chain.Direct.ScriptRegistrySpec where

import Hydra.Prelude ( ($), Either(Right) )
import Test.Hydra.Prelude ( prop, Spec )

import Hydra.Tx.ScriptRegistry (
  newScriptRegistry,
  registryUTxO,
 )
import Test.Hydra.Tx.Gen (genScriptRegistry)
import Test.QuickCheck (forAllBlind, (===))

spec :: Spec
spec =
  prop "newScriptRegistry (registryUTxO r) === Just r" $
    forAllBlind genScriptRegistry $ \r ->
      newScriptRegistry (registryUTxO r) === Right r
