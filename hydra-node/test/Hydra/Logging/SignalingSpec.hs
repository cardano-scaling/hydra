{-# LANGUAGE TypeApplications #-}

module Hydra.Logging.SignalingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.API.Server (Server (..))
import Hydra.API.ServerOutput (ServerOutput (SomeHeadInitializing))
import Hydra.Cardano.Api (Tx)
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.Handlers (DirectChainLog (SomeHeadObserved))
import Hydra.Logging (nullTracer, traceWith)
import Hydra.Logging.Messages (HydraLog (DirectChain))
import Hydra.Logging.Signaling (installSignal, withSignaling)

spec :: Spec
spec =
  it "signals observations from the chain" $ do
    let someHeadId = HeadId "1234"
        signalled = runSimOrThrow $ withSignaling nullTracer $ \tracer signalVar -> do
          sentSignals <- newTVarIO []
          let server = Server @Tx $ \output -> atomically (modifyTVar' sentSignals (output :))
          installSignal signalVar server
          traceWith tracer (DirectChain $ SomeHeadObserved someHeadId ["12345"])
          readTVarIO sentSignals

    signalled `shouldBe` [SomeHeadInitializing someHeadId ["3132333435"]]
