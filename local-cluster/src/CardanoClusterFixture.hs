{-# LANGUAGE TemplateHaskell #-}

module CardanoClusterFixture where

import Hydra.Prelude

import Data.FileEmbed (embedDir, makeRelativeToProject)
import qualified Data.List as List
import System.IO.Error (doesNotExistErrorType, mkIOError)

configFiles :: [(FilePath, ByteString)]
configFiles = $(makeRelativeToProject "config" >>= embedDir)

writeConfigFile :: FilePath -> FilePath -> IO ()
writeConfigFile source target =
  case List.lookup source configFiles of
    Nothing -> throwIO $ mkIOError doesNotExistErrorType "cannot find fixture" Nothing (Just source)
    Just bs -> writeFileBS target bs
