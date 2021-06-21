module Hydra.Chain.ExternalPABSpec where

import Cardano.Prelude
import Hydra.Chain (Chain (..))
import Hydra.Chain.ExternalPAB (withExternalPAB)
import Hydra.HeadLogic (OnChainTx (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Test.Hspec.Core.Spec (Spec, describe, it)

spec :: Spec
spec =
  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1" $ do
      -- TODO(SN): launch hydra-pab as process
      withExternalPAB nullTracer (panic "called back") $ \Chain{postTx} ->
        postTx $ InitTx @SimpleTx (panic "unused")
