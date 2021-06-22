module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Hydra.Chain (Chain (..))
import Hydra.Chain.ExternalPAB (withExternalPAB)
import Hydra.HeadLogic (OnChainTx (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import System.Process (proc, withCreateProcess)
import Test.Hspec.Core.Spec (Spec, describe, it)

spec :: Spec
spec =
  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1" $ do
      withHydraPAB $ do
        withExternalPAB nullTracer (error "called back") $ \Chain{postTx} ->
          postTx $ InitTx @SimpleTx (error "unused")

withHydraPAB :: IO a -> IO a
withHydraPAB action =
  withCreateProcess (proc "hydra-pab" []) $ \_ _ _ _ -> do
    threadDelay 2
    action
