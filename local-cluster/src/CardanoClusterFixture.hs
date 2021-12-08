{-# LANGUAGE TemplateHaskell #-}

-- | Embeds the local-cluster's "config" files and makes them as available to
-- consumers via 'readConfigFile'.
module CardanoClusterFixture where

import Hydra.Prelude

import Data.FileEmbed (embedDir, makeRelativeToProject)
import qualified Data.List as List
import System.IO.Error (doesNotExistErrorType, mkIOError)

configFiles :: [(FilePath, ByteString)]
configFiles = $(makeRelativeToProject "config" >>= embedDir)

-- | Lookup a config file similar reading a file from disk.
readConfigFile :: FilePath -> IO ByteString
readConfigFile source =
  case List.lookup source configFiles of
    Nothing ->
      throwIO $
        mkIOError
          doesNotExistErrorType
          ("cannot find fixture in embedded config files: " <> show (fst <$> configFiles))
          Nothing
          (Just source)
    Just bs -> pure bs
