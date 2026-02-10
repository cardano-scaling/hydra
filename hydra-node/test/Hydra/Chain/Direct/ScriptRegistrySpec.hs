module Hydra.Chain.Direct.ScriptRegistrySpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (forAllBlind, (===))
import "hydra-tx" Hydra.Tx.ScriptRegistry (
  newScriptRegistry,
  registryUTxO,
 )
import "hydra-tx" Test.Hydra.Tx.Gen (genScriptRegistry)

spec :: Spec
spec =
  prop "newScriptRegistry (registryUTxO r) === Just r" $
    forAllBlind genScriptRegistry $ \r ->
      newScriptRegistry (registryUTxO r) === Right r
