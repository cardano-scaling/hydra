{-# LANGUAGE CPP #-}

module Hydra.Network.EtcdBinarySpec where

import Hydra.Prelude

import Data.ByteString.Lazy.Char8 qualified as LBS
#ifdef linux_HOST_OS
import Foreign.C.Error (Errno (Errno), eTXTBSY, errnoToIOError)
import GHC.IO.Exception (ioe_errno)
import System.IO (hClose, withBinaryFile)
#endif
import Hydra.Network (WhichEtcd (EmbeddedEtcd))
import Hydra.Network.EtcdBinary (getEtcdBinary, retryOnBusyExecutable)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Process.Typed (proc, readProcessStdout_, runProcess_)
import Test.Hydra.Prelude

spec :: Spec
spec = do
  it "installs and runs embedded etcd from a path containing spaces, including after restart" $
    withTempDir "embedded-etcd" $ \tmp ->
      -- Exercise installation while unrelated children fork and execute.
      withAsyncLabelled ("unrelated-child", forever $ runProcess_ $ proc "sleep" ["0.05"]) $ \_ -> do
        let persistenceDir = tmp </> "node state with spaces"
            checkExecutable =
              failAfter 10 $ do
                executable <- getEtcdBinary persistenceDir EmbeddedEtcd
                output <- retryOnBusyExecutable $ readProcessStdout_ $ proc executable ["--version"]
                LBS.unpack output `shouldContain` "etcd Version:"
        replicateM_ 50 checkExecutable

  it "does not retry a missing executable" $
    failAfter 10 $
      withTempDir "embedded-etcd-missing" $ \tmp -> do
        attempts <- newIORef (0 :: Int)
        retryOnBusyExecutable
          ( do
              modifyIORef' attempts (+ 1)
              runProcess_ $ proc (tmp </> "missing") []
          )
          `shouldThrow` isDoesNotExistError
        readIORef attempts `shouldReturn` 1

  busyExecutableSpec

busyExecutableSpec :: Spec
#ifdef linux_HOST_OS
busyExecutableSpec = do
  it "retries a busy executable after the writable descriptor closes" $
    failAfter 10 $
      withTempDir "embedded-etcd-busy" $ \tmp -> do
        executable <- getEtcdBinary tmp EmbeddedEtcd
        attempts <- newIORef (0 :: Int)
        withBinaryFile executable ReadWriteMode $ \writer -> do
          -- The first exec fails deterministically; releasing the descriptor
          -- then models the unrelated child's close-on-exec cleanup.
          output <- retryOnBusyExecutable $ do
            modifyIORef' attempts (+ 1)
            readProcessStdout_ (proc executable ["--version"])
              `onException` hClose writer
          LBS.unpack output `shouldContain` "etcd Version:"
        readIORef attempts >>= (`shouldSatisfy` (> 1))

  it "stops retrying a persistent busy-executable error" $
    failAfter 30 $ do
      -- Use a fixed ETXTBSY error to exercise retry exhaustion with constant
      -- work per attempt.
      let busyException = errnoToIOError "startProcess" eTXTBSY Nothing Nothing
      attempts <- newIORef (0 :: Int)
      retryOnBusyExecutable
        ( do
            modifyIORef' attempts (+ 1)
            throwIO busyException
        )
        `shouldThrow` \exception -> (Errno <$> ioe_errno exception) == Just eTXTBSY
      readIORef attempts `shouldReturn` 21
#else
busyExecutableSpec = pure ()
#endif
