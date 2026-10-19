module Hydra.Network.EtcdBinarySpec where

import Hydra.Prelude

import Data.ByteString.Lazy.Char8 qualified as LBS
import Hydra.Network (WhichEtcd (EmbeddedEtcd))
import Hydra.Network.EtcdBinary (getEtcdBinary)
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcessStdout_, runProcess_)
import Test.Hydra.Prelude

spec :: Spec
spec =
  it "installs and runs embedded etcd from a path containing spaces, including after restart" $
    failAfter 30 $
      withTempDir "embedded-etcd" $ \tmp ->
        -- Another process must not inherit the installer's write handle: on
        -- Linux that prevents executing etcd with "Text file busy".
        withAsyncLabelled ("unrelated-child", forever $ runProcess_ $ proc "sleep" ["0.05"]) $ \_ -> do
          let persistenceDir = tmp </> "node state with spaces"
              checkExecutable = do
                executable <- getEtcdBinary persistenceDir EmbeddedEtcd
                output <- readProcessStdout_ $ proc executable ["--version"]
                LBS.unpack output `shouldContain` "etcd Version:"
          replicateM_ 50 checkExecutable
