module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Hydra.Chain (Chain (..))
import Hydra.Chain.ExternalPAB (withExternalPAB)
import Hydra.HeadLogic (OnChainTx (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Test.Hspec.Core.Spec (Spec, describe, it, pendingWith)

spec :: Spec
spec =
  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1" $ do
      pendingWith "launch hydra-pab as process"
      -- TODO(SN): launch hydra-pab as process
      withExternalPAB nullTracer (error "called back") $ \Chain{postTx} ->
        postTx $ InitTx @SimpleTx (error "unused")
