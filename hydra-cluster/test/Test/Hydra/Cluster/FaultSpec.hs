module Test.Hydra.Cluster.FaultSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (putTMVar, readTMVar, takeTMVar)
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (cancel)
import Control.Tracer.JSON (Tracer (..), emit)
import Data.ByteString qualified as BS
import HydraNode (withProcessAndLogs, withSuspendedNodeProcesses)
import System.Directory (createFileLink, findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hGetLine, hIsClosed, hPutStrLn)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals (sigCONT, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (..), StdStream (CreatePipe), cleanupProcess, createProcess, getPid, proc, waitForProcess)
import System.Process.Typed qualified as Typed
import System.Timeout qualified as Timeout
import Prelude qualified

spec :: Spec
spec = logForwardingSpec >> peerSuspensionSpec

peerSuspensionSpec :: Spec
peerSuspensionSpec = describe "scoped peer suspension" $ do
  it "resumes the node if its etcd child cannot be found"
    $ bracket
      (createProcess (proc "sh" ["-c", "read line; printf 'resumed\\n'"]){std_in = CreatePipe, std_out = CreatePipe})
      cleanupProcess
    $ \case
      (Just childInput, Just childOutput, _, process) -> do
        pid <- getPid process >>= maybe (fail "Child has no PID") pure
        ( do
            result :: Either SomeException () <- try $ withSuspendedNodeProcesses pid (pure ())
            result `shouldSatisfy` isLeft
            hPutStrLn childInput "continue"
            hFlush childInput
            Timeout.timeout 2_000_000 (hGetLine childOutput) `shouldReturn` Just "resumed"
          )
          `finally` void (try @IO @IOException $ signalProcess sigCONT pid)
      _ -> expectationFailure "Missing child pipes"

  it "resumes the node and its etcd child when the fault action throws" $
    withChild $ \pid childInput childOutput -> do
      result :: Either SomeException () <- try $ withSuspendedNodeProcesses pid $ do
        hPutStrLn childInput "continue"
        hFlush childInput
        Timeout.timeout 200_000 (hGetLine childOutput) `shouldReturn` Nothing
        fail "scenario failed"
      result `shouldSatisfy` isLeft
      Timeout.timeout 2_000_000 (hGetLine childOutput) `shouldReturn` Just "resumed"

  it "resumes the node and its etcd child when the scenario is cancelled" $
    withChild $ \pid childInput childOutput -> do
      entered <- newLabelledEmptyTMVarIO "peer-paused"
      blocked <- newLabelledEmptyTMVarIO @IO @() "scenario-blocked"
      withAsyncLabelled
        ("paused-peer", withSuspendedNodeProcesses pid $ atomically (putTMVar entered ()) >> atomically (takeTMVar blocked))
        $ \worker -> do
          atomically $ takeTMVar entered
          hPutStrLn childInput "continue"
          hFlush childInput
          Timeout.timeout 200_000 (hGetLine childOutput) `shouldReturn` Nothing
          cancel worker
      Timeout.timeout 2_000_000 (hGetLine childOutput) `shouldReturn` Just "resumed"
 where
  withChild :: (ProcessID -> Handle -> Handle -> IO ()) -> IO ()
  withChild action = withTempDir "suspended-node" $ \dir -> do
    shellPath <- findExecutable "sh" >>= maybe (fail "Cannot find sh") pure
    let etcdPath = dir </> "etcd"
    createFileLink shellPath etcdPath
    bracket
      (createProcess (proc "sh" ["-c", "set -m; \"$1\" -c 'printf \"ready %s\\n\" \"$$\"; read line; printf \"resumed\\n\"' <&0 & wait \"$!\"", "sh", etcdPath]){std_in = CreatePipe, std_out = CreatePipe})
      cleanupProcess
      $ \case
        (Just childInput, Just childOutput, _, process) -> do
          pid <- getPid process >>= maybe (fail "Child has no PID") pure
          ready <- hGetLine childOutput
          etcdPid <- case Prelude.words ready of
            ["ready", childPid] -> maybe (fail "Invalid child PID") pure $ readMaybe childPid
            _ -> fail "Missing child readiness"
          ( do
              nodeGroup <- getProcessGroupIDOf pid
              etcdGroup <- getProcessGroupIDOf etcdPid
              etcdGroup `shouldNotBe` nodeGroup
              action pid childInput childOutput
              Timeout.timeout 2_000_000 (waitForProcess process) `shouldReturn` Just ExitSuccess
            )
            `finally` do
              void (try @IO @IOException $ signalProcess sigCONT etcdPid)
              void (try @IO @IOException $ signalProcess sigCONT pid)
        _ -> expectationFailure "Missing child pipes"

logForwardingSpec :: Spec
logForwardingSpec = describe "process log forwarding" $ do
  it "drains the final marker when the child fails" $ failAfter 5 $ do
    (result, output) <- captureTail
      "printf '{\"message\":\"ready\"}\\n'; read line; printf 'final marker\\n'; exit 7"
      $ \p -> do
        hPutStrLn (Typed.getStdin p) "exit"
        hFlush (Typed.getStdin p)
        Typed.waitExitCode p `shouldReturn` Typed.ExitFailure 7
        fail "child failed"
    fmap displayException (leftToMaybe result) `shouldBe` Just "user error (child failed)"
    output `shouldBe` "{\"message\":\"ready\"}\nfinal marker\n"

  it "captures termination logs when the action returns" $ failAfter 5 $ do
    (result, output) <- captureTail shutdownScript $ const $ pure ()
    result `shouldSatisfy` isRight
    output `shouldBe` "{\"message\":\"ready\"}\nfinal marker\n"
 where
  -- The shell has no grandchildren and installs its handler before readiness.
  shutdownScript :: String
  shutdownScript = "trap 'printf \"final marker\\n\"; exit 0' TERM; printf '{\"message\":\"ready\"}\\n'; while :; do :; done"

  child :: String -> Typed.ProcessConfig Handle () Handle
  child script =
    Typed.proc "sh" ["-c", script]
      & Typed.setStdin Typed.createPipe
      & Typed.setStderr Typed.createPipe
      & Typed.setCloseFds True

  captureTail :: String -> (Typed.Process Handle () Handle -> IO ()) -> IO (Either IOException (), ByteString)
  captureTail script action = withTempDir "process-log-tail" $ \dir -> do
    ready <- newLabelledEmptyTMVarIO "log-ready"
    process <- newLabelledEmptyTMVarIO "log-process"
    let logPath = dir </> "stdout.log"
        tracer = Tracer . emit $ \_ -> do
          atomically $ putTMVar ready ()
          p <- atomically $ readTMVar process
          -- Hold the reader until process cleanup starts. This leaves the tail
          -- unread when the action exits, independently of thread scheduling.
          let awaitCleanup :: IO ()
              awaitCleanup = do
                closed <- hIsClosed $ Typed.getStderr p
                unless closed $ threadDelay 0.001 >> awaitCleanup
          awaitCleanup
    result <- withFile logPath WriteMode $ \logFile ->
      try @IO @IOException $ withProcessAndLogs tracer logFile (child script) $ \p -> do
        atomically $ putTMVar process p
        atomically $ takeTMVar ready
        action p
    output <- BS.readFile logPath
    pure (result, output)
