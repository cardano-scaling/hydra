{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Embedding and installation of 'etcd' binary. In a dedicated module as HLS
-- tends to choke on the executable's Template Haskell splice.
module Hydra.Network.EtcdBinary where

import Hydra.Prelude

#ifndef mingw32_HOST_OS
import Data.Bits ((.|.))
import System.Posix (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode)
#endif
import Hydra.Network (WhichEtcd (..))
import Hydra.Node.EmbedTH (embedExecutable)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory, (</>))

-- | Return the path of the etcd binary. Will either install it first, or just
-- assume there is one available on the system path.
getEtcdBinary :: FilePath -> WhichEtcd -> IO FilePath
getEtcdBinary _ SystemEtcd = pure etcdExecutable
getEtcdBinary persistenceDir EmbeddedEtcd =
  let path = persistenceDir </> "bin" </> etcdExecutable
   in installEtcd path >> pure path

etcdExecutable :: FilePath
#ifdef mingw32_HOST_OS
etcdExecutable = "etcd.exe"
#else
etcdExecutable = "etcd"
#endif

-- | Install the embedded 'etcd' binary to given file path.
installEtcd :: FilePath -> IO ()
installEtcd fp = do
  createDirectoryIfMissing True (takeDirectory fp)
  -- Replace the executable without truncating a previous installation. This
  -- also avoids rewriting an inode still mapped by a process during shutdown.
  writeFileBS tmp $(embedExecutable "HYDRA_ETCD_PATH" "etcd")
  makeExecutable tmp
  renameFile tmp fp
 where
  tmp = fp <> ".tmp"

makeExecutable :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
makeExecutable _ = pure ()
#else
makeExecutable fp = setFileMode fp (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)
#endif
