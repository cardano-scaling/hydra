-- | Template haskell expression to embed executables.
module Hydra.Node.EmbedTH where

import "hydra-prelude" Hydra.Prelude

import "directory" System.Directory (findExecutable)
import "file-embed" Data.FileEmbed (embedFile)
import "template-haskell" Language.Haskell.TH (Exp, Q, runIO)

-- | Template haskell expression to find and embed an executable with given name.
embedExecutable :: String -> Q Exp
embedExecutable exe = do
  fp <- runIO $ do
    findExecutable exe >>= \case
      Nothing -> fail $ exe <> " not found, ensure it is in PATH when compiling (and do a cabal clean)"
      Just fp -> do
        putStrLn $ "Embedding " <> exe <> " from: " <> fp
        pure fp
  embedFile fp
