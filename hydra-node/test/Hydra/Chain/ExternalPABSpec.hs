module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (Chain (..))
import Hydra.Chain.ExternalPAB (withExternalPAB)
import Hydra.HeadLogic (OnChainTx (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import System.Process (proc, withCreateProcess, CreateProcess (std_in), StdStream (CreatePipe))
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Util (failAfter)
import Test.Hspec (shouldReturn)

spec :: Spec
spec =
  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1 and observes it also" $ do
      failAfter 10 $ withHydraPAB $ do
        calledBack <- newEmptyMVar
        withExternalPAB nullTracer (putMVar calledBack) $ \Chain{postTx} -> do
          let parties = mempty
          postTx $ InitTx @SimpleTx parties
          failAfter 3 $
            takeMVar calledBack `shouldReturn` InitTx parties

withHydraPAB :: IO a -> IO a
withHydraPAB action =
  -- Open a stdin, as pab tries to read from it
  withCreateProcess (pab { std_in = CreatePipe }) $ \_ _ _ _ ->
    action
 where
  pab = proc "hydra-pab" []
