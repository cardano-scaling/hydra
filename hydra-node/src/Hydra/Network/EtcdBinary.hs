{-# LANGUAGE TemplateHaskell #-}

-- | Embedding and installation of 'etcd' binary. In a dedicated module as HLS
-- tends to choke on $(embedExecutable "etcd").
module Hydra.Network.EtcdBinary where

import "hydra-prelude" Hydra.Prelude
import "base" Data.Bits ((.|.))
import "directory" System.Directory (createDirectoryIfMissing)
import "filepath" System.FilePath (takeDirectory, (</>))
import "unix" System.Posix (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode)

import Hydra.Network (WhichEtcd (..))
import Hydra.Node.EmbedTH (embedExecutable)

-- | Return the path of the etcd binary. Will either install it first, or just
-- assume there is one available on the system path.
getEtcdBinary :: FilePath -> WhichEtcd -> IO FilePath
getEtcdBinary _ SystemEtcd = pure "etcd"
getEtcdBinary persistenceDir EmbeddedEtcd =
  let path = persistenceDir </> "bin" </> "etcd"
   in installEtcd path >> pure path

-- | Install the embedded 'etcd' binary to given file path.
installEtcd :: FilePath -> IO ()
installEtcd fp = do
  createDirectoryIfMissing True (takeDirectory fp)
  writeFileBS fp $(embedExecutable "etcd")
  setFileMode fp (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)
