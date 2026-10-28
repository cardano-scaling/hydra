module Test.Hydra.Cluster.FaultSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (putTMVar, takeTMVar)
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (cancel)
import HydraNode (withSuspendedProcess)
import System.IO (hGetLine, hPutStrLn)
import System.Posix.Signals (sigCONT, signalProcess)
import System.Process (CreateProcess (..), StdStream (CreatePipe), cleanupProcess, createProcess, getPid, proc)
import System.Timeout qualified as Timeout

spec :: Spec
spec = describe "scoped peer suspension" $ do
  it "resumes the child when the fault action throws" $
    withChild $ \pid childInput childOutput -> do
      result :: Either SomeException () <- try $ withSuspendedProcess pid $ fail "scenario failed"
      result `shouldSatisfy` isLeft
      hPutStrLn childInput "continue"
      hFlush childInput
      Timeout.timeout 2_000_000 (hGetLine childOutput) `shouldReturn` Just "resumed"

  it "resumes the child when the scenario is cancelled" $
    withChild $ \pid childInput childOutput -> do
      entered <- newLabelledEmptyTMVarIO "peer-paused"
      blocked <- newLabelledEmptyTMVarIO @IO @() "scenario-blocked"
      withAsyncLabelled
        ("paused-peer", withSuspendedProcess pid $ atomically (putTMVar entered ()) >> atomically (takeTMVar blocked))
        $ \worker -> do
          atomically $ takeTMVar entered
          cancel worker
      hPutStrLn childInput "continue"
      hFlush childInput
      Timeout.timeout 2_000_000 (hGetLine childOutput) `shouldReturn` Just "resumed"
 where
  withChild action =
    bracket
      (createProcess (proc "sh" ["-c", "read line; printf 'resumed\\n'"]){std_in = CreatePipe, std_out = CreatePipe})
      cleanupProcess
      $ \case
        (Just childInput, Just childOutput, _, process) -> do
          pid <- getPid process >>= maybe (fail "Child has no PID") pure
          action pid childInput childOutput `finally` void (try @IO @IOException $ signalProcess sigCONT pid)
        _ -> expectationFailure "Missing child pipes"
